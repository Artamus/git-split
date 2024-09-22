open Diff

let ( let* ) = Result.bind
let ( let+ ) = Option.bind

module Result = struct
  include Result

  let rec all = function
    | [] -> Ok []
    | Ok x :: tl -> map (List.cons x) (all tl)
    | Error e :: _ -> Error e
end

let not_empty str = String.length str > 0

let parse_line line : (line option, string) result =
  (* TODO: Introduce a test case with this at the end. *)
  if String.starts_with ~prefix:"\\ No newline at end of file" line then Ok None
  else
    let first_char = line.[0] in
    let line_contents = String.sub line 1 (String.length line - 1) in
    let parsed_line =
      match first_char with
      | ' ' -> Ok (`ContextLine line_contents)
      | '-' -> Ok (`RemovedLine line_contents)
      | '+' -> Ok (`AddedLine line_contents)
      | c -> Error (Printf.sprintf "Found unexpected character: %c" c)
    in
    parsed_line |> Result.map (fun line -> Some line)

let get_lines lines_diff =
  String.split_on_char '\n' lines_diff
  |> List.filter not_empty |> List.map parse_line |> Result.all
  |> Result.map (fun lines -> lines |> List.filter_map Fun.id)

let parse_hunk hunk =
  let hunk_regex =
    Re.Perl.re ~opts:[ `Multiline ] {|^-(\d+),?\d* \+\d+,?\d* @@(.*)$([.\s\S]*)|} |> Re.compile
  in
  let hunk_groups = Re.exec hunk_regex hunk in

  let starting_line = int_of_string @@ Re.Group.get hunk_groups 1 in
  let raw_context_snippet = Re.Group.get hunk_groups 2 in
  let context_snippet =
    if not_empty raw_context_snippet then
      Some (String.sub raw_context_snippet 1 (String.length raw_context_snippet - 1))
    else None
  in

  (* TODO: Propagate errors. *)
  let lines = get_lines @@ Re.Group.get hunk_groups 3 in

  { starting_line; context_snippet; lines = Result.get_ok lines }

let parse_text_content (file_diff : string) =
  let hunk_split_regex = Re.Perl.re ~opts:[ `Multiline ] "^@@ " |> Re.compile in
  let raw_hunks = Re.split hunk_split_regex file_diff |> List.tl in
  raw_hunks |> List.map parse_hunk

let parse_binary_content file_diff =
  let binary_content_regex =
    Re.Perl.re ~opts:[ `Multiline ] {|^GIT binary patch\n([.\s\S]*)|} |> Re.Perl.compile
  in
  let+ binary_content_grp = Re.exec_opt binary_content_regex file_diff in
  Re.Group.get_opt binary_content_grp 1

let parse_content file_diff =
  let regex_incomplete_binary =
    Re.Perl.re ~opts:[ `Multiline ] "^Binary files(.*) differ$" |> Re.Perl.compile
  in
  let is_incomplete_binary = Re.execp regex_incomplete_binary file_diff in

  if is_incomplete_binary then Error "cannot parse diff of binary file without its content"
  else
    let binary_content = parse_binary_content file_diff in
    match binary_content with
    | Some content -> Ok (`Binary content)
    | None -> Ok (`Text (parse_text_content file_diff))

let parse_path file_diff =
  let paths_regex = Re.Perl.re ~opts:[ `Multiline ] "a/(.*) b/(.*)" |> Re.Perl.compile in
  let+ paths_grp = Re.exec_opt paths_regex file_diff in
  let+ src = Re.Group.get_opt paths_grp 1 in
  let+ dst = Re.Group.get_opt paths_grp 2 in
  Some (src, dst)

let filter_flatten_hunks_lines hunks filter =
  hunks |> List.map (fun hunk -> hunk.lines) |> List.flatten |> List.filter_map filter

let select_added_line = function `AddedLine line -> Some (`AddedLine line) | _ -> None
let select_removed_line = function `RemovedLine line -> Some (`RemovedLine line) | _ -> None

let parse_file_diff file_diff =
  let* diff_paths =
    parse_path file_diff |> Option.to_result ~none:"could not parse file diff paths"
  in
  let* diff_content = parse_content file_diff in

  let regex_deleted_file =
    Re.Perl.re ~opts:[ `Multiline ] "^deleted file mode" |> Re.Perl.compile
  in
  let was_file_deleted = Re.execp regex_deleted_file file_diff in
  let regex_created_file = Re.Perl.re ~opts:[ `Multiline ] "^new file mode" |> Re.Perl.compile in
  let was_file_created = Re.execp regex_created_file file_diff in

  if was_file_created then
    let path, _ = diff_paths in
    let* content =
      match diff_content with
      | `Binary content -> Ok (`Binary content)
      | `Text hunks ->
          if List.length hunks > 1 then Error "created file cannot have multiple hunks"
          else
            let added_lines = filter_flatten_hunks_lines hunks select_added_line in
            Ok (`Text added_lines)
    in
    Ok (CreatedFile { path; content })
  else if was_file_deleted then
    let path, _ = diff_paths in
    let* content =
      match diff_content with
      | `Binary content -> Ok (`Binary content)
      | `Text hunks ->
          if List.length hunks > 1 then Error "deleted file cannot have multiple hunks"
          else
            let removed_lines = filter_flatten_hunks_lines hunks select_removed_line in
            Ok (`Text removed_lines)
    in
    Ok (DeletedFile { path; content })
  else
    let src, dst = diff_paths in
    let path = if src = dst then Path src else ChangedPath { src; dst } in
    Ok (ChangedFile { path; content = diff_content })

let parse raw_diff =
  let file_split_regex = Re.Perl.re ~opts:[ `Multiline ] "^diff --git " |> Re.Perl.compile in
  let file_diffs = Re.split file_split_regex raw_diff in
  let* files = file_diffs |> List.map parse_file_diff |> Result.all in
  Ok { files }
