(* TODO: Rename unchanged line to context line. *)
type line = UnchangedLine of string | RemovedLine of string | AddedLine of string
[@@deriving show, eq]

type hunk = { lines : line list } [@@deriving show, eq]
type file = { path : string; hunks : hunk list } [@@deriving show, eq]

type renamed_file = { old_path : string; new_path : string; hunks : hunk list }
[@@deriving show, eq]

type diff_file = RenamedFile of renamed_file | DiffFile of file [@@deriving show, eq]
type diff = { files : diff_file list } [@@deriving show, eq]

let contains_changes hunks =
  hunks
  |> List.exists (fun hunk ->
         hunk.lines
         |> List.exists (fun line ->
                match line with RemovedLine _ | AddedLine _ -> true | _ -> false))

let contains_changes diff =
  diff.files
  |> List.exists (fun file ->
         match file with
         | DiffFile diff_file -> contains_changes diff_file.hunks
         | RenamedFile renamed_file -> contains_changes renamed_file.hunks)

exception Internal_state_error of string

let subtract_line ~open_line ~commit_line =
  match (open_line, commit_line) with
  | RemovedLine _, RemovedLine _ -> None
  | AddedLine content, AddedLine _ -> Some (UnchangedLine content)
  | _, _ -> raise (Internal_state_error "line filtering should have happened before")

let subtract_hunk ~open_hunk ~commit_hunk =
  open_hunk.lines
  |> List.map (fun line ->
         let maybe_commit_line =
           commit_hunk.lines |> List.find_opt (fun commit_line -> commit_line = line)
         in
         match maybe_commit_line with None -> line | Some commit_line -> commit_line)

let subtract_file ~open_file ~commit_file =
  match (open_file, commit_file) with
  | DiffFile open_diff_file, DiffFile commit_diff_file ->
      let hunks =
        open_diff_file.hunks
        |> List.map (fun hunk ->
               let maybe_commit_hunk =
                 commit_diff_file.hunks
                 |> List.find_opt (fun commit_hunk ->
                        List.hd hunk.lines = List.hd commit_hunk.lines)
               in
               match maybe_commit_hunk with None -> hunk | Some commit_hunk -> commit_hunk)
      in
      DiffFile { path = open_diff_file.path; hunks }
  | RenamedFile _, RenamedFile _ -> open_file
  | _, _ -> raise (Internal_state_error "TODO")

let subtract ~open_diff ~commit_diff =
  {
    files =
      open_diff.files
      |> List.map (fun file ->
             match file with
             | DiffFile diff_file -> (
                 let maybe_commit_file =
                   commit_diff.files
                   |> List.find_opt (fun commit_file ->
                          match commit_file with
                          | DiffFile commit_diff -> commit_diff.path = diff_file.path
                          | RenamedFile _ -> false)
                 in
                 match maybe_commit_file with
                 | None -> DiffFile diff_file
                 | Some commit_file -> subtract_file ~open_file:file ~commit_file)
             | RenamedFile renamed_file -> RenamedFile renamed_file);
  }
