type 'a zipper = Zip of 'a list * 'a * 'a list [@@deriving show, eq]

let from_list l = if List.length l = 0 then None else Some (Zip ([], List.hd l, List.tl l))

let from_list_exn l =
  match from_list l with
  | None -> invalid_arg "cannot create zipper of empty list"
  | Some zipper -> zipper

let from_list_rev l =
  if List.length l = 0 then None
  else
    let rev_list = List.rev l in
    Some (Zip (List.tl rev_list, List.hd rev_list, []))

let from_list_rev_exn l =
  match from_list_rev l with
  | None -> invalid_arg "cannot create zipper of empty list"
  | Some zipper -> zipper

let to_list (Zip (ls, focus, rs)) = List.rev ls @ (focus :: rs)
let at_begin = function Zip ([], _, _) -> true | _ -> false
let at_end = function Zip (_, _, []) -> true | _ -> false

let to_begin (Zip (ls, focus, rs)) =
  let items = List.rev ls @ (focus :: rs) in
  Zip ([], List.hd items, List.tl items)

let to_end (Zip (ls, focus, rs)) =
  let items = List.rev rs @ (focus :: ls) in
  Zip (List.tl items, List.hd items, [])

let prev = function Zip (a :: ls, focus, rs) -> Zip (ls, a, focus :: rs) | z -> z
let prev_wrap z = if at_begin z then z |> to_end else prev z
let next = function Zip (ls, focus, a :: rs) -> Zip (focus :: ls, a, rs) | z -> z
let next_wrap z = if at_end z then to_begin z else next z
let cursor (Zip (_, focus, _)) = focus
let insert a (Zip (ls, focus, rs)) = Zip (focus :: ls, a, rs)
let delete = function Zip (ls, _, a :: rs) -> Zip (ls, a, rs) | z -> z
let replace a = function Zip (ls, _, rs) -> Zip (ls, a, rs)
