type context_line = [ `ContextLine of string ] [@@deriving show, eq]
type removed_line = [ `RemovedLine of string ] [@@deriving show, eq]
type added_line = [ `AddedLine of string ] [@@deriving show, eq]
type line = [ context_line | removed_line | added_line ] [@@deriving show, eq]

type hunk = { starting_line : int; context_snippet : string option; lines : line list }
[@@deriving show, eq]

type binary_content = [ `Binary of string ] [@@deriving show, eq]
type path = Path of string | ChangedPath of { src : string; dst : string } [@@deriving show, eq]

type changed_file = { path : path; content : [ `Text of hunk list | binary_content ] }
[@@deriving show, eq]

type created_file = { path : string; content : [ `Text of added_line list | binary_content ] }
[@@deriving show, eq]

type deleted_file = { path : string; content : [ `Text of removed_line list | binary_content ] }
[@@deriving show, eq]

type file =
  | ChangedFile of changed_file
  | CreatedFile of created_file
  | DeletedFile of deleted_file
[@@deriving show, eq]

type diff = { files : file list } [@@deriving show, eq]
