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

(* Eventually this will have an alternative variant for binary content. *)
(* TODO: Move visibility into this struct. *)
type content = Text of { hunks : hunk list } [@@deriving show, eq]
type file = { path : path; visibility : visibility; content : content } [@@deriving show, eq]
