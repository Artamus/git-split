open Git_split

let main () =
  let git_commit_id =
    Feather.process "git" [ "rev-parse"; "--short"; "HEAD" ] |> Feather.collect Feather.stdout
  in
  let git_diff =
    Feather.process "git" [ "diff"; git_commit_id ^ "~1"; git_commit_id ]
    |> Feather.collect Feather.stdout
  in
  let _diff = DiffParser.parse_diff git_diff in
  let final_model = NottyTui.run NottyTui.initial_model in
  let final_diff = NottyTui.diff_of_model final_model in
  print_endline @@ Diff.show_diff final_diff;
  let serialized_diff = DiffSerializer.serialize final_diff in
  print_endline serialized_diff;
  ()

let () = main ()
