type line = UnchangedLine of string | RemovedLine of string | AddedLine of string
[@@deriving show, eq]

type hunk = { lines : line list } [@@deriving show, eq]
type file = { path : string; hunks : hunk list } [@@deriving show, eq]
type renamed_file = { old_path : string; new_path : string } [@@deriving show, eq]
type diff_file = RenamedFile of renamed_file | DiffFile of file [@@deriving show, eq]
type diff = { files : diff_file list } [@@deriving show, eq]

exception Invalid_character of string

let parse_line line =
  let first_char = line.[0] in
  let line_contents = String.sub line 1 (String.length line - 1) in
  match first_char with
  | ' ' -> UnchangedLine line_contents
  | '-' -> RemovedLine line_contents
  | '+' -> AddedLine line_contents
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
  { old_path = old_file; new_path = new_file }

let parse_file_diff file_diff =
  print_endline "##########";
  print_endline file_diff;
  print_endline "##########";
  let file_rename_regex = Re.str "rename from " |> Re.compile in
  let is_file_rename = Re.execp file_rename_regex file_diff in
  if is_file_rename then RenamedFile (parse_file_rename file_diff)
  else DiffFile (parse_file_diff file_diff)

let parse_diff raw_diff =
  let file_split_regex = Re.Perl.re ~opts:[ `Multiline ] "^diff --git a/" |> Re.Perl.compile in
  let file_diffs = Re.split file_split_regex raw_diff in
  { files = List.map parse_file_diff file_diffs }
