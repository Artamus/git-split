type line =
  | Context of string
  | Diff of string * [ `added | `removed ] * [ `included | `notincluded ]
[@@deriving show, eq]

type visibility = Expanded | Collapsed [@@deriving show, eq]

type hunk = {
  starting_line : int;
  context_snippet : string option;
  visibility : visibility;
  lines : line list;
}
[@@deriving show, eq]

type path = Path of string | ChangedPath of { old_path : string; new_path : string }
[@@deriving show, eq]

type file_kind = ChangedFile | CreatedFile | DeletedFile [@@deriving show, eq]
type mode = Mode of int | ChangedMode of { old_mode : int; new_mode : int } [@@deriving show, eq]

type content =
  | Text of { visibility : visibility; hunks : hunk list }
  | Binary of (string * [ `included | `notincluded ])
[@@deriving show, eq]

type file = { path : path; kind : file_kind; mode : mode option; content : content }
[@@deriving show, eq]
