open Diff

let serialize_line = function
  | UnchangedLine content -> Printf.sprintf " %s" content
  | RemovedLine content -> Printf.sprintf "-%s" content
  | AddedLine content -> Printf.sprintf "+%s" content

let serialize_hunk hunk =
  let minus_startline = hunk.first_line_idx in
  let minus_endline =
    List.fold_left
      (fun acc line ->
        match line with UnchangedLine _ -> acc + 1 | RemovedLine _ -> acc + 1 | AddedLine _ -> acc)
      (minus_startline - 1) hunk.lines
  in

  let plus_startline = minus_startline in
  let plus_endline =
    List.fold_left
      (fun acc line ->
        match line with UnchangedLine _ -> acc + 1 | RemovedLine _ -> acc | AddedLine _ -> acc + 1)
      (plus_startline - 1) hunk.lines
  in

  let left_line_indices =
    if minus_startline = minus_endline then Printf.sprintf "-%d" minus_startline
    else Printf.sprintf "-%d,%d" minus_startline minus_endline
  in
  let right_line_indices =
    if plus_startline = plus_endline then Printf.sprintf "+%d" plus_startline
    else Printf.sprintf "+%d,%d" plus_startline plus_endline
  in

  let hunk_header = Printf.sprintf "@@ %s %s @@" left_line_indices right_line_indices in
  let lines = hunk.lines |> List.map serialize_line in
  hunk_header :: lines |> String.concat "\n"

let serialize_diff_file file =
  let file_header =
    Printf.sprintf {|diff --git a/%s b/%s
--- a/%s
+++ b/%s|} file.path file.path file.path
      file.path
  in
  let hunks = file.hunks |> List.map serialize_hunk in
  file_header :: hunks |> String.concat "\n"

let serialize_file = function
  | DiffFile diff_file -> serialize_diff_file diff_file
  | RenamedFile _ -> ""

let serialize diff = diff.files |> List.map serialize_file |> String.concat "\n"
