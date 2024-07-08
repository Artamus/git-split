open Diff

let serialize_line = function
  | UnchangedLine content -> Printf.sprintf " %s" content
  | RemovedLine content -> Printf.sprintf "-%s" content
  | AddedLine content -> Printf.sprintf "+%s" content

let serialize_hunk hunk =
  let hunk_header = Printf.sprintf "@@ -0,4 +0,4 @@" in
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
