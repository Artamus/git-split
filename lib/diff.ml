(* TODO: Rename unchanged line to context line. *)
type line = UnchangedLine of string | RemovedLine of string | AddedLine of string
[@@deriving show, eq]

type hunk = { lines : line list } [@@deriving show, eq]
type file = { path : string; hunks : hunk list } [@@deriving show, eq]

type renamed_file = { old_path : string; new_path : string; hunks : hunk list }
[@@deriving show, eq]

type diff_file = RenamedFile of renamed_file | DiffFile of file [@@deriving show, eq]
type diff = { files : diff_file list } [@@deriving show, eq]

let subtract open_diff selected_diff =
  {
    files =
      open_diff.files
      |> List.filter (fun file ->
             match file with
             | DiffFile { path = open_diff_path; _ } -> (
                 match
                   List.find_opt
                     (fun foo ->
                       match foo with
                       | DiffFile { path = selected_diff_path; _ } ->
                           open_diff_path = selected_diff_path
                       | _ -> false)
                     selected_diff.files
                 with
                 | None -> true
                 | Some _ -> false)
             | RenamedFile { old_path = open_diff_old_path; new_path = open_diff_new_path; _ } -> (
                 match
                   List.find_opt
                     (fun foo ->
                       match foo with
                       | RenamedFile
                           {
                             old_path = selected_diff_old_path;
                             new_path = selected_diff_new_path;
                             _;
                           } ->
                           open_diff_old_path = selected_diff_old_path
                           && open_diff_new_path = selected_diff_new_path
                       | _ -> false)
                     selected_diff.files
                 with
                 | None -> true
                 | Some _ -> false));
  }
