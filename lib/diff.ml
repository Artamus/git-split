type context_line = [ `ContextLine of string ] [@@deriving show, eq]
type removed_line = [ `RemovedLine of string ] [@@deriving show, eq]
type added_line = [ `AddedLine of string ] [@@deriving show, eq]
type line = [ context_line | removed_line | added_line ] [@@deriving show, eq]

type hunk = { starting_line : int; context_snippet : string option; lines : line list }
[@@deriving show, eq]

type changed_file = { path : string; hunks : hunk list } [@@deriving show, eq]

type renamed_file = { old_path : string; new_path : string; hunks : hunk list }
[@@deriving show, eq]

type deleted_file = { path : string; lines : removed_line list } [@@deriving show, eq]
type created_file = { path : string; lines : added_line list } [@@deriving show, eq]

type file =
  | DeletedFile of deleted_file
  | CreatedFile of created_file
  | ChangedFile of changed_file
  | RenamedFile of renamed_file
[@@deriving show, eq]

type diff = { files : file list } [@@deriving show, eq]
