open Git_split
open Feather
open Feather.Infix

let rec select_changes current_diff_commit target_commit =
  let head_diff = process "git" [ "diff"; current_diff_commit; target_commit ] |> collect stdout in
  if String.length head_diff = 0 then ()
  else
    let diff = DiffParser.parse_diff head_diff in
    let final_model = NottyTui.run @@ NottyTui.model_of_diff diff in
    match final_model with
    | None -> print_endline "Job's done!"
    | Some model ->
        let selected_diff = NottyTui.diff_of_model model in
        let serialized_diff = DiffSerializer.serialize selected_diff in
        let tmp_filename = Printf.sprintf "/tmp/%s.diff" current_diff_commit in
        echo serialized_diff > tmp_filename |> run;
        process "git" [ "apply"; tmp_filename ] |> run;
        process "git" [ "add"; "." ] |> run;
        process "git" [ "commit" ] |> run;
        let current_diff_commit =
          process "git" [ "rev-parse"; "--short"; "HEAD" ] |> collect stdout
        in
        select_changes current_diff_commit target_commit

let main () =
  (* Only HEAD for now. *)
  let splittable_commit_id = process "git" [ "rev-parse"; "--short"; "HEAD" ] |> collect stdout in
  let starting_diff_commit_id =
    process "git" [ "rev-parse"; "--short"; splittable_commit_id ^ "~1" ] |> collect stdout
  in
  print_endline @@ "Resetting commit " ^ splittable_commit_id;
  process "git" [ "reset"; "--hard"; "HEAD~1" ] |> run;
  select_changes starting_diff_commit_id splittable_commit_id

let () = main ()
