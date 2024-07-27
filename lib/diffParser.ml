open Diff

let ( let* ) = Result.bind

module Result = struct
  include Result

  let rec all = function
    | [] -> Ok []
    | Ok x :: tl -> map (List.cons x) (all tl)
    | Error e :: _ -> Error e
end

let not_empty str = String.length str > 0

let parse_line line =
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

let parse_hunk_context_snippet hunk =
  let hunk_context_snippet_regex = Re.Perl.re "@@" |> Re.Perl.compile in
  let context_snippet_elements =
    hunk |> String.split_on_char '\n' |> List.hd |> Re.split hunk_context_snippet_regex |> List.tl
  in
  if List.is_empty context_snippet_elements then None
  else
    let snippet = List.hd context_snippet_elements in
    let context_snippet = String.sub snippet 1 (String.length snippet - 1) in
    Some context_snippet

let parse_hunk_diff hunk =
  let hunk_first_line_idx =
    hunk |> String.split_on_char ' ' |> List.hd |> String.split_on_char ',' |> List.hd
    |> int_of_string
  in
  let maybe_hunk_context_snippet = parse_hunk_context_snippet hunk in
  let lines = String.split_on_char '\n' hunk |> List.tl in
  lines |> List.filter not_empty |> List.map parse_line |> Result.all
  |> Result.map (fun lines ->
         {
           starting_line = hunk_first_line_idx;
           context_snippet = maybe_hunk_context_snippet;
           lines = lines |> List.filter_map Fun.id;
         })

let parse_file_hunks file_diff =
  let hunk_split_regex = Re.Perl.re ~opts:[ `Multiline ] "^@@ -" |> Re.Perl.compile in
  let file_hunk_split = Re.split hunk_split_regex file_diff in
  let hunk_content = List.tl file_hunk_split in
  hunk_content |> List.map parse_hunk_diff |> Result.all

let parse_deleted_file file_diff : deleted_file =
  let file_path_regex = Re.str " b/" |> Re.compile in
  let file_path = Re.split file_path_regex file_diff |> List.hd in
  let hunk_splitting_regex = Re.str "@@" |> Re.compile in
  let file_elements = Re.split hunk_splitting_regex file_diff in
  let lines =
    if List.length file_elements < 3 then []
    else
      let lines_content = List.nth file_elements 2 in
      lines_content |> String.split_on_char '\n' |> List.tl
      (* TODO: See if it would be better to throw it to the general line parsing function. *)
      |> List.map (fun line -> `RemovedLine (String.sub line 1 (String.length line - 1)))
  in
  { path = file_path; lines }

let parse_created_file file_diff : created_file =
  let file_path_regex = Re.str " b/" |> Re.compile in
  let file_path = Re.split file_path_regex file_diff |> List.hd in
  let hunk_splitting_regex = Re.str "@@" |> Re.compile in
  let file_elements = Re.split hunk_splitting_regex file_diff in
  let lines =
    if List.length file_elements < 3 then []
    else
      let lines_content = List.nth file_elements 2 in
      lines_content |> String.split_on_char '\n' |> List.tl
      (* TODO: See if it would be better to throw it to the general line parsing function. *)
      |> List.map (fun line -> `AddedLine (String.sub line 1 (String.length line - 1)))
  in
  { path = file_path; lines }

let parse_renamed_file file_diff =
  let lines = String.split_on_char '\n' file_diff in
  let old_file_line_prefix = "rename from " in
  let old_file_line =
    lines |> List.find (fun line -> String.starts_with ~prefix:old_file_line_prefix line)
  in
  let old_file_path_len = String.length old_file_line - String.length old_file_line_prefix in
  let old_file = String.sub old_file_line (String.length old_file_line_prefix) old_file_path_len in
  let new_file_line_prefix = "rename to " in
  let new_file_line =
    lines |> List.find (fun line -> String.starts_with ~prefix:new_file_line_prefix line)
  in
  let new_file_path_len = String.length new_file_line - String.length new_file_line_prefix in
  let new_file = String.sub new_file_line (String.length new_file_line_prefix) new_file_path_len in
  let hunks_present_regex = Re.str "@@ " |> Re.compile in
  let* hunks =
    if Re.execp hunks_present_regex file_diff then parse_file_hunks file_diff else Ok []
  in
  Ok { old_path = old_file; new_path = new_file; hunks }

let parse_changed_file file_diff =
  let file_path_regex = Re.Perl.re " b/" |> Re.Perl.compile in
  let file_path = Re.split file_path_regex file_diff |> List.hd in
  let* hunks = parse_file_hunks file_diff in
  Ok { path = file_path; hunks }

let parse_file_diff file_diff =
  let file_deleted_regex =
    Re.Perl.re ~opts:[ `Multiline ] "^deleted file mode" |> Re.Perl.compile
  in
  let was_file_deleted = Re.execp file_deleted_regex file_diff in
  let file_created_regex = Re.Perl.re ~opts:[ `Multiline ] "^new file mode" |> Re.Perl.compile in
  let was_file_created = Re.execp file_created_regex file_diff in
  let file_renamed_regex = Re.Perl.re ~opts:[ `Multiline ] "^rename from " |> Re.Perl.compile in
  let was_file_renamed = Re.execp file_renamed_regex file_diff in
  if was_file_deleted then Ok (DeletedFile (parse_deleted_file file_diff))
  else if was_file_created then Ok (CreatedFile (parse_created_file file_diff))
  else if was_file_renamed then
    parse_renamed_file file_diff |> Result.map (fun file -> RenamedFile file)
  else parse_changed_file file_diff |> Result.map (fun file -> ChangedFile file)

let parse_diff raw_diff =
  let file_split_regex = Re.Perl.re ~opts:[ `Multiline ] "^diff --git a/" |> Re.Perl.compile in
  let file_diffs = Re.split file_split_regex raw_diff in
  let* files = file_diffs |> List.map parse_file_diff |> Result.all in
  Ok { files }
