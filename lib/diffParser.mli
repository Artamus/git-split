(* TODO: Rename unchanged line to context line. *)
type line = UnchangedLine of string | RemovedLine of string | AddedLine of string
[@@deriving show, eq]

type hunk = { lines : line list } [@@deriving show, eq]
type file = { path : string; hunks : hunk list } [@@deriving show, eq]
type renamed_file = { old_path : string; new_path : string } [@@deriving show, eq]
type diff_file = RenamedFile of renamed_file | DiffFile of file [@@deriving show, eq]
type diff = { files : diff_file list } [@@deriving show, eq]

val parse_diff : string -> diff
