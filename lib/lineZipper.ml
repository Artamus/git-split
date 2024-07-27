open TuiTypes

type zipper = Zip of line list * line * line list [@@deriving show, eq]

let from_list l =
  let rec from_list' left = function
    | [] -> None
    | hd :: tl -> (
        match hd with
        | Context _ -> from_list' (hd :: left) tl
        | Diff _ -> Some (Zip (left, hd, tl)))
  in

  from_list' [] l

let from_list_rev l =
  let rec from_list' left right =
    match left with
    | [] -> None
    | hd :: tl -> (
        match hd with
        | Context _ -> from_list' tl (hd :: right)
        | Diff _ -> Some (Zip (tl, hd, right)))
  in
  from_list' (List.rev l) []

let to_list (Zip (ls, focus, rs)) = List.rev ls @ (focus :: rs)

let rec list_exhausted' = function
  | [] -> true
  | [ Context _ ] -> true
  | [ Diff _ ] -> false
  | hd :: tl -> ( match hd with Diff _ -> false | Context _ -> list_exhausted' tl)

let at_begin = function Zip (ls, _, _) -> list_exhausted' ls
let at_end = function Zip (_, _, rs) -> list_exhausted' rs

(* Have to ensure that we do not go past starting context lines. *)
let prev zipper =
  let rec prev' left right =
    match left with
    | hd :: tl -> (
        match hd with Context _ -> prev' tl (hd :: right) | Diff _ -> Some (tl, hd :: right))
    | [] -> None
  in
  match zipper with
  | Zip ([], _, _) as z -> z
  | Zip (ls, focus, rs) -> (
      match prev' ls [] with
      | None -> Zip (ls, focus, rs)
      | Some (ls, new_rs) -> Zip (ls, List.hd new_rs, List.tl new_rs @ (focus :: rs)))

let next zipper =
  let rec next' left right =
    match right with
    | hd :: tl -> (
        match hd with Context _ -> next' (hd :: left) tl | Diff _ -> Some (left, hd :: tl))
    | [] -> None
  in
  match zipper with
  | Zip (_, _, []) as z -> z
  | Zip (ls, focus, rs) -> (
      match next' [] rs with
      | None -> Zip (ls, focus, rs)
      | Some (new_ls, rs) -> Zip (new_ls @ (focus :: ls), List.hd rs, List.tl rs))

let cursor (Zip (_, focus, _)) = focus
let replace a = function Zip (ls, _, rs) -> Zip (ls, a, rs)
