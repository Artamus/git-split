let () =
  print_endline "Hello, World!" in
  let git_diff_cmd = Feather.process "git" [ "diff"; "HEAD~1"; "HEAD"] in
  let stdout = git_diff_cmd |> Feather.collect Feather.stdout in
  (* Now parse the diff. *)
  let regex = Re.str "diff --git " |> Re.compile in
  let splitted = Re.split regex stdout in
  let first = List.hd splitted in
  let second = List.hd (List.tl splitted) in
  let () = print_endline first in
  print_endline second
