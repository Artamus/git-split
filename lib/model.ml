type diffLine = UnchangedLine of string | RemovedLine of string | AddedLine of string
[@@deriving show, eq]

type diffHunk = { lines : diffLine list } [@@deriving show, eq]
type diffFile = { path : string; hunks : diffHunk list } [@@deriving show, eq]
type diff = { files : diffFile list } [@@deriving show, eq]
