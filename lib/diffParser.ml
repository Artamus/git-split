open Model

exception Invalid_character of string

let parse_line line =
  let first_char = line.[0] in
  let line_contents = String.sub line 1 (String.length line - 1) in
  match first_char with
  | ' ' -> Model.UnchangedLine line_contents
  | '-' -> Model.RemovedLine line_contents
  | '+' -> Model.AddedLine line_contents
  | c -> raise (Invalid_character (Printf.sprintf "Found unexpected character: %c" c))

let not_empty str = String.length str > 0

let parse_hunk_diff hunk_diff =
  let lines = String.split_on_char '\n' hunk_diff in
  let non_empty_lines = List.filter not_empty lines in
  { lines = List.map parse_line non_empty_lines }

let parse_file_diff file_diff =
  let hunk_split_regex = Re.Perl.re ~opts:[ `Multiline ] "^@@.*$" |> Re.Perl.compile in
  let file_hunk_split = Re.split hunk_split_regex file_diff in
  let file_metadata_text = List.hd file_hunk_split in
  let file_path = String.split_on_char ' ' file_metadata_text |> List.hd in
  let hunk_diffs = List.tl file_hunk_split in
  { path = file_path; hunks = List.map parse_hunk_diff hunk_diffs }

let parse_diff raw_diff =
  print_endline raw_diff;
  let file_split_regex = Re.Perl.re ~opts:[ `Multiline ] "^diff --git a/" |> Re.Perl.compile in
  let file_diffs = Re.split file_split_regex raw_diff in
  { files = List.map parse_file_diff file_diffs }
