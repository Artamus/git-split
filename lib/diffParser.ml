open Diff

exception Invalid_character of string

let not_empty str = String.length str > 0

let parse_line line =
  let first_char = line.[0] in
  let line_contents = String.sub line 1 (String.length line - 1) in
  match first_char with
  | ' ' -> `ContextLine line_contents
  | '-' -> `RemovedLine line_contents
  | '+' -> `AddedLine line_contents
  | c -> raise (Invalid_character (Printf.sprintf "Found unexpected character: %c" c))

let parse_hunk_context_snippet hunk =
  let hunk_context_snippet_regex = Re.Perl.re "@@" |> Re.Perl.compile in
  let foo =
    hunk |> String.split_on_char '\n' |> List.hd |> Re.split hunk_context_snippet_regex |> List.tl
  in
  if List.is_empty foo then None
  else
    let snippet = List.hd foo in
    let context_snippet = String.sub snippet 1 (String.length snippet - 1) in
    Some context_snippet

let parse_hunk_diff hunk =
  let hunk_first_line_idx =
    hunk |> String.split_on_char ' ' |> List.hd |> String.split_on_char ',' |> List.hd
    |> int_of_string
  in
  let maybe_hunk_context_snippet = parse_hunk_context_snippet hunk in
  let lines = String.split_on_char '\n' hunk |> List.tl in
  let non_empty_lines = List.filter not_empty lines in
  {
    starting_line = hunk_first_line_idx;
    context_snippet = maybe_hunk_context_snippet;
    lines = List.map parse_line non_empty_lines;
  }

let parse_file_hunks file_diff =
  let hunk_split_regex = Re.Perl.re ~opts:[ `Multiline ] "^@@ -" |> Re.Perl.compile in
  let file_hunk_split = Re.split hunk_split_regex file_diff in
  let hunk_content = List.tl file_hunk_split in
  List.map parse_hunk_diff hunk_content

let parse_file_diff file_diff =
  let file_path_regex = Re.Perl.re " b/" |> Re.Perl.compile in
  let file_path = Re.split file_path_regex file_diff |> List.hd in
  { path = file_path; hunks = parse_file_hunks file_diff }

let parse_file_rename file_diff =
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
  let hunks = if Re.execp hunks_present_regex file_diff then parse_file_hunks file_diff else [] in
  { old_path = old_file; new_path = new_file; hunks }

let parse_file_diff file_diff =
  let file_rename_regex = Re.str "rename from " |> Re.compile in
  let is_file_rename = Re.execp file_rename_regex file_diff in
  if is_file_rename then RenamedFile (parse_file_rename file_diff)
  else ChangedFile (parse_file_diff file_diff)

let parse_diff raw_diff =
  let file_split_regex = Re.Perl.re ~opts:[ `Multiline ] "^diff --git a/" |> Re.Perl.compile in
  let file_diffs = Re.split file_split_regex raw_diff in
  { files = List.map parse_file_diff file_diffs }
