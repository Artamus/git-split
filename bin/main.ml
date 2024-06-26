let main () =
  let git_diff_cmd = Feather.process "git" [ "diff"; "HEAD~1"; "HEAD" ] in
  let stdout = git_diff_cmd |> Feather.collect Feather.stdout in
  let _dummy_stdout =
    "diff --git a/bin/dune b/bin/dune\n\
     index 3e8a4cb..fc8e91d 100644\n\
     --- a/bin/dune\n\
     +++ b/bin/dune\n\
     @@ -1,4 +1,4 @@\n\
    \ (executable\n\
    \  (public_name git_split)\n\
    \  (name main)\n\
     - (libraries git_split))\n\
     + (libraries git_split feather re))"
  in
  let _diff = Git_split.DiffParser.parse_diff stdout in
  Minttea.start Git_split.Minttea_tui.app ~initial_model:Git_split.Minttea_tui.initial_model

let () = main ()
