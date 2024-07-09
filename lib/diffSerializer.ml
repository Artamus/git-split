open Diff

let count_left_lines lines =
  lines
  |> List.fold_left
       (fun count line ->
         match line with
         | UnchangedLine _ -> count + 1
         | RemovedLine _ -> count + 1
         | AddedLine _ -> count)
       0

let count_right_lines lines =
  lines
  |> List.fold_left
       (fun count line ->
         match line with
         | UnchangedLine _ -> count + 1
         | RemovedLine _ -> count
         | AddedLine _ -> count + 1)
       0

let serialize_line = function
  | UnchangedLine content -> Printf.sprintf " %s" content
  | RemovedLine content -> Printf.sprintf "-%s" content
  | AddedLine content -> Printf.sprintf "+%s" content

let serialize_hunk hunk right_offset =
  let left_start_line = hunk.first_line_idx in
  let left_line_count = count_left_lines hunk.lines in

  let right_start_line = left_start_line + right_offset in
  let right_line_count = count_right_lines hunk.lines in

  let left_lines =
    if left_line_count = 1 then Printf.sprintf "-%d" left_start_line
    else Printf.sprintf "-%d,%d" left_start_line left_line_count
  in
  let right_lines =
    if right_line_count = 1 then Printf.sprintf "+%d" right_start_line
    else Printf.sprintf "+%d,%d" right_start_line right_line_count
  in

  let hunk_header = Printf.sprintf "@@ %s %s @@" left_lines right_lines in
  let lines = hunk.lines |> List.map serialize_line in
  hunk_header :: lines |> String.concat "\n"

let serialize_diff_file file =
  let file_header =
    Printf.sprintf {|diff --git a/%s b/%s
--- a/%s
+++ b/%s|} file.path file.path file.path
      file.path
  in
  let _, hunks =
    List.fold_left_map
      (fun acc hunk ->
        let left_lines = count_left_lines hunk.lines in
        let right_lines = count_right_lines hunk.lines in
        (acc + right_lines - left_lines, serialize_hunk hunk acc))
      0 file.hunks
  in
  file_header :: hunks |> String.concat "\n"

let serialize_file = function
  | DiffFile diff_file -> serialize_diff_file diff_file
  | RenamedFile _ -> ""

let serialize diff = diff.files |> List.map serialize_file |> String.concat "\n"
