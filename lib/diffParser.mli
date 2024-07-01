type line = UnchangedLine of string | RemovedLine of string | AddedLine of string
[@@deriving show, eq]

type hunk = { lines : line list } [@@deriving show, eq]
type file = { path : string; hunks : hunk list } [@@deriving show, eq]
type diff = { files : file list } [@@deriving show, eq]

val parse_diff : string -> diff
