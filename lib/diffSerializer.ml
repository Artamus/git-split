open Diff

let serialize_line = function
  | `ContextLine content -> Printf.sprintf " %s" content
  | `RemovedLine content -> Printf.sprintf "-%s" content
  | `AddedLine content -> Printf.sprintf "+%s" content

let serialize_binary_content binary_content = Printf.sprintf "GIT binary patch\n%s" binary_content

let serialize_created_file (file : created_file) =
  let diff_header = Printf.sprintf "diff --git a/%s b/%s" file.path file.path in
  let file_mode = Printf.sprintf "new file mode %d" file.mode in
  let content =
    match file.content with
    | `Binary binary_content -> [ serialize_binary_content binary_content ]
    | `Text added_lines ->
        let lines = added_lines |> List.map serialize_line in
        if List.length lines = 0 then []
        else
          let file_path_header = Printf.sprintf {|--- /dev/null
+++ b/%s|} file.path in
          let hunk_header = Printf.sprintf "@@ -0,0 +1,%d @@" (List.length lines) in
          file_path_header :: hunk_header :: lines
  in
  diff_header :: file_mode :: content |> String.concat "\n"

let serialize_deleted_file (file : deleted_file) =
  let diff_header = Printf.sprintf "diff --git a/%s b/%s" file.path file.path in
  let file_mode = Printf.sprintf "deleted file mode %d" file.mode in
  let content =
    match file.content with
    | `Binary binary_content -> [ serialize_binary_content binary_content ]
    | `Text removed_lines ->
        let lines = removed_lines |> List.map serialize_line in
        if List.length lines = 0 then []
        else
          let file_path_header = Printf.sprintf {|--- a/%s
+++ /dev/null|} file.path in
          let hunk_header = Printf.sprintf "@@ -1,%d +0,0 @@" (List.length lines) in
          file_path_header :: hunk_header :: lines
  in
  diff_header :: file_mode :: content |> String.concat "\n"

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
  hunk_header :: lines

let serialize_changed_file (changed_file : changed_file) =
  let src_path, dst_path =
    match changed_file.path with
    | Path path -> (path, path)
    | ChangedPath { old_path; new_path } -> (old_path, new_path)
  in
  let diff_header = [ Printf.sprintf "diff --git a/%s b/%s" src_path dst_path ] in

  let mode_change =
    match changed_file.mode_change with
    | None -> []
    | Some mode_change ->
        [
          Printf.sprintf "old mode %d" mode_change.prev;
          Printf.sprintf "new mode %d" mode_change.next;
        ]
  in

  let rename =
    match changed_file.path with
    | Path _ -> []
    | ChangedPath { old_path; new_path } ->
        [
          "similarity index 100%";
          Printf.sprintf "rename from %s" old_path;
          Printf.sprintf "rename to %s" new_path;
        ]
  in

  let content =
    match changed_file.content with
    | `Binary binary_content -> [ serialize_binary_content binary_content ]
    | `Text hunks ->
        let changed_file_header = Printf.sprintf {|--- a/%s
+++ b/%s|} src_path dst_path in
        let _, hunks =
          hunks
          |> List.fold_left_map
               (fun acc hunk ->
                 let left_lines = count_left_lines hunk.lines in
                 let right_lines = count_right_lines hunk.lines in
                 (acc + right_lines - left_lines, serialize_hunk hunk acc))
               0
        in
        let hunk_lines = hunks |> List.flatten in
        if List.length hunks > 0 then changed_file_header :: hunk_lines else []
  in

  List.concat [ diff_header; mode_change; rename; content ] |> String.concat "\n"

let serialize_file = function
  | CreatedFile file -> serialize_created_file file
  | DeletedFile file -> serialize_deleted_file file
  | ChangedFile file -> serialize_changed_file file

let serialize diff = diff.files |> List.map serialize_file |> String.concat "\n"
