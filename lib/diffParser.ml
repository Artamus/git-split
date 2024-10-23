open Angstrom
open Buffered

(** Various utility bits *)

let ( let* ) = Result.bind

module Result = struct
  include Result

  let rec all = function
    | [] -> Ok []
    | Ok x :: tl -> map (List.cons x) (all tl)
    | Error e :: _ -> Error e
end

let is_eol = function '\n' -> true | _ -> false
let is_not_eol = function '\n' -> false | _ -> true
let is_digit = function '0' .. '9' -> true | _ -> false
let integer = take_while is_digit >>| int_of_string
let eol = char '\n'

(** Diff lines *)

let added_line =
  char '+' *> take_till is_eol <* eol >>= fun content ->
  return (`AddedLine content) <?> "added line"

let removed_line =
  char '-' *> take_till is_eol <* eol >>= fun content ->
  return (`RemovedLine content) <?> "removed line"

let context_line =
  char ' ' *> take_till is_eol <* eol >>= fun content ->
  return (`ContextLine content) <?> "context line"

let line = choice [ added_line; removed_line; context_line ] <?> "line"

(** Hunk *)

let hunk_header =
  string "@@ -" *> integer
  <* take_till (function '@' -> true | _ -> false) *> string "@@"
  <?> "hunk header"

let hunk_context = char ' ' *> take_till is_eol <?> "hunk context"
let hunk_context_opt = option None (hunk_context >>| Option.some)

let hunk =
  lift4
    (fun starting_line context_snippet _ lines -> Diff.{ starting_line; context_snippet; lines })
    hunk_header hunk_context_opt eol (many1 line)

(** Content *)

let left_file = string "---" *> take_till is_eol <* eol <?> "left file"
let right_file = string "+++" *> take_till is_eol <* eol <?> "right file"

let text_content =
  left_file *> right_file *> many1 hunk <?> "text content" >>| fun hunks -> `Text hunks

let text_content_opt = option (Ok (`Text [])) (text_content >>| Result.ok)
let binary_header_line = string "GIT binary patch" <* eol
let binary_count_prefix = string "literal " <|> string "delta "
let binary_count = integer <* eol

let binary_chunk_meta_line =
  lift2 (fun prefix count -> prefix ^ string_of_int count) binary_count_prefix binary_count

let binary_chunk_lines = many1 (take_while1 is_not_eol <* eol) <* eol

let binary_content =
  lift4
    (fun new_chunk_meta new_binary_lines old_chunk_meta old_binary_lines ->
      let new_content = new_chunk_meta :: new_binary_lines |> String.concat "\n" in
      let old_content = old_chunk_meta :: old_binary_lines |> String.concat "\n" in
      Ok (`Binary (new_content ^ "\n\n" ^ old_content ^ "\n\n")))
    (binary_header_line *> binary_chunk_meta_line)
    binary_chunk_lines binary_chunk_meta_line binary_chunk_lines

let missing_binary_content =
  string "Binary files a/" *> many_till any_char (string " differ") <* eol >>| fun _ ->
  Error "cannot parse diff of binary file without its content"

let content = missing_binary_content <|> binary_content <|> text_content_opt <?> "content"

(** Mode change *)

let old_mode = string "old mode " *> integer <* eol
let new_mode = string "new mode " *> integer <* eol
let mode_change = lift2 (fun old_mode new_mode -> Diff.{ old_mode; new_mode }) old_mode new_mode
let mode_change_opt = option None (mode_change >>| Option.some)

(** Rename *)

let similarity = string "similarity index " *> integer <* char '%' <* eol
let rename_from = string "rename from " *> take_while1 is_not_eol <* eol
let rename_to = string "rename to " *> take_while1 is_not_eol <* eol
let rename = similarity <* rename_from <* rename_to
let rename_opt = option None (rename >>| Option.some)

(** New/deleted file *)

let new_file_mode = string "new file mode " *> integer <* eol
let deleted_file_mode = string "deleted file mode " *> integer <* eol

(** File *)

let index = string "index" *> take_while1 is_not_eol <* eol <?> "index"
let index_opt = option None (index >>| Option.some)

let first_file_name =
  many_till any_char (string " b/") >>| fun chars -> String.of_seq (List.to_seq chars)

let second_file_name = take_while1 is_not_eol <* eol

let file_header =
  string "diff --git a/" *> first_file_name >>= fun old_path ->
  second_file_name >>= fun new_path ->
  return
    (if old_path = new_path then Diff.Path old_path else Diff.ChangedPath { old_path; new_path })

let changed_file =
  file_header >>= fun path ->
  mode_change_opt >>= fun mode_change ->
  rename_opt >>= fun _ ->
  index_opt >>= fun _ ->
  content >>| fun content ->
  let* content = content in
  Ok (Diff.ChangedFile { path; mode_change; content })

let created_file =
  file_header >>= fun path ->
  new_file_mode >>= fun mode ->
  index_opt >>= fun _ ->
  content >>| fun content ->
  let* path =
    match path with Path p -> Ok p | _ -> Error "created file cannot have changed path"
  in
  let* content = content in
  let content =
    match content with
    | `Text hunks ->
        let all_lines =
          hunks
          |> List.map (fun (hunk : Diff.hunk) : Diff.line list -> hunk.lines)
          |> List.flatten
          |> List.filter_map (fun line ->
                 match line with `AddedLine content -> Some (`AddedLine content) | _ -> None)
        in
        `Text all_lines
    | `Binary binary -> `Binary binary
  in
  Ok (Diff.CreatedFile { path; mode; content })

let deleted_file =
  file_header >>= fun path ->
  deleted_file_mode >>= fun mode ->
  index_opt >>= fun _ ->
  content >>| fun content ->
  let* path =
    match path with Path p -> Ok p | _ -> Error "deleted file cannot have changed path"
  in
  let* content = content in
  let content =
    match content with
    | `Text hunks ->
        let all_lines =
          hunks
          |> List.map (fun (hunk : Diff.hunk) : Diff.line list -> hunk.lines)
          |> List.flatten
          |> List.filter_map (fun line ->
                 match line with `RemovedLine content -> Some (`RemovedLine content) | _ -> None)
        in
        `Text all_lines
    | `Binary binary -> `Binary binary
  in
  Ok (Diff.DeletedFile { path; mode; content })

(** Parser *)
let parser =
  many1 (created_file <|> deleted_file <|> changed_file) >>| fun files ->
  let* files = Result.all files in
  Ok Diff.{ files }

let parse str = parse_string ~consume:All parser str |> Result.join

(** A slightly more involved version of the parser that gives greater insight into failures. *)
let _parse_debug str =
  let state = Buffered.parse parser in
  let state = feed state (`String str) in
  let end_state = feed state `Eof in
  let result = state_to_result end_state |> Result.join in
  let unconsumed =
    match state_to_unconsumed end_state with
    | None -> None
    | Some { buf; off; len } -> if len = 0 then None else Some (Bigstringaf.substring buf ~off ~len)
  in
  let result =
    match (result, unconsumed) with
    | x, None -> x
    | Ok _, Some u -> Error (Printf.sprintf "unconsumed: '%s'" u)
    | Error msg, Some u -> Error (Printf.sprintf "%s / unconsumed: '%s'" msg u)
  in
  result
