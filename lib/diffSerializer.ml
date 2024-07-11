open Diff

let serialize_line = function
  | `ContextLine content -> Printf.sprintf " %s" content
  | `RemovedLine content -> Printf.sprintf "-%s" content
  | `AddedLine content -> Printf.sprintf "+%s" content

let serialize_deleted_file (file : deleted_file) =
  let file_header =
    Printf.sprintf {|diff --git a/%s b/%s
deleted file mode 100644
--- a/%s
+++ /dev/null|}
      file.path file.path file.path
  in
  let hunk_header = Printf.sprintf "@@ -1,%d +0,0 @@" (List.length file.lines) in
  let lines = file.lines |> List.map serialize_line in
  file_header :: hunk_header :: lines |> String.concat "\n"

let serialize_created_file (file : created_file) =
  let file_header =
    Printf.sprintf {|diff --git a/%s b/%s
new file mode 100644
--- /dev/null
+++ b/%s|} file.path
      file.path file.path
  in
  let hunk_header = Printf.sprintf "@@ -0,0 +1,%d @@" (List.length file.lines) in
  let lines = file.lines |> List.map serialize_line in
  file_header :: hunk_header :: lines |> String.concat "\n"

let count_left_lines lines =
  lines
  |> List.fold_left
       (fun count line ->
         match line with
         | `ContextLine _ -> count + 1
         | `RemovedLine _ -> count + 1
         | `AddedLine _ -> count)
       0

let count_right_lines lines =
  lines
  |> List.fold_left
       (fun count line ->
         match line with
         | `ContextLine _ -> count + 1
         | `RemovedLine _ -> count
         | `AddedLine _ -> count + 1)
       0

let serialize_hunk hunk right_offset =
  let left_start_line = hunk.starting_line in
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
  let context_snippet =
    match hunk.context_snippet with None -> "" | Some snippet -> Printf.sprintf " %s" snippet
  in

  let hunk_header = Printf.sprintf "@@ %s %s @@%s" left_lines right_lines context_snippet in
  let lines = hunk.lines |> List.map serialize_line in
  hunk_header :: lines |> String.concat "\n"

let serialize_changed_file (file : changed_file) =
  let file_header =
    Printf.sprintf {|diff --git a/%s b/%s
--- a/%s
+++ b/%s|} file.path file.path file.path
      file.path
  in
  let _, hunks =
    file.hunks
    |> List.fold_left_map
         (fun acc (hunk : hunk) ->
           let left_lines = count_left_lines hunk.lines in
           let right_lines = count_right_lines hunk.lines in
           (acc + right_lines - left_lines, serialize_hunk hunk acc))
         0
  in
  file_header :: hunks |> String.concat "\n"

let serialize_renamed_file (file : renamed_file) =
  Printf.sprintf {|diff --git a/%s b/%s
similarity index 100%%
rename from %s
rename to %s|}
    file.old_path file.new_path file.old_path file.new_path

let serialize_file = function
  | DeletedFile file -> serialize_deleted_file file
  | CreatedFile file -> serialize_created_file file
  | ChangedFile file -> serialize_changed_file file
  | RenamedFile file -> serialize_renamed_file file

let serialize diff = diff.files |> List.map serialize_file |> String.concat "\n"
