open Git_split
open Feather
open Feather.Infix

let usage_msg = "git-split [commit-id]"
let commit_id = ref "HEAD"
let example = ref false
let anon_fun selected_commit = commit_id := selected_commit

let speclist =
  [
    ("[commit-id]", Arg.Set_string commit_id, "Commit to split. Defaults to HEAD.");
    ("--tui-example", Arg.Set example, "Run TUI in example mode without modifying the repo.");
  ]

type head_commit = { reference_commit : string; target_commit : string }

type intermediary_commit = {
  reference_commit : string;
  target_commit : string;
  original_head : string;
  branch : string;
}

type split_commit = HeadCommit of head_commit | IntermediaryCommit of intermediary_commit

let cleanup intermediary_commit =
  process "git"
    [
      "rebase";
      "--onto";
      intermediary_commit.reference_commit;
      intermediary_commit.target_commit ^ "~1";
      intermediary_commit.original_head;
    ]
  |> run;
  let head_commit_id = process "git" [ "rev-parse"; "--short"; "HEAD" ] |> collect stdout in
  process "git" [ "branch"; "-f"; intermediary_commit.branch; head_commit_id ] |> run

let rec select_changes split_commit =
  let reference_commit =
    match split_commit with
    | HeadCommit { reference_commit; _ } -> reference_commit
    | IntermediaryCommit { reference_commit; _ } -> reference_commit
  in
  let target_commit =
    match split_commit with
    | HeadCommit { target_commit; _ } -> target_commit
    | IntermediaryCommit { target_commit; _ } -> target_commit
  in
  let head_diff = process "git" [ "diff"; reference_commit; target_commit ] |> collect stdout in
  if String.length head_diff = 0 then
    let () =
      match split_commit with
      | HeadCommit _ -> ()
      | IntermediaryCommit intermediary_commit -> cleanup intermediary_commit
    in
    print_endline "Job's done!"
  else
    let diff = DiffParser.parse_diff head_diff in
    let final_model = Tui.run @@ Tui.model_of_diff diff in
    match final_model with
    | None -> print_endline "Aborted"
    | Some model ->
        let selected_diff = Tui.diff_of_model model in
        let serialized_diff = DiffSerializer.serialize selected_diff in
        let tmp_filename = Printf.sprintf "/tmp/%s.diff" reference_commit in
        echo serialized_diff > tmp_filename |> run;
        process "git" [ "apply"; tmp_filename ] |> run;
        process "git" [ "add"; "." ] |> run;
        process "git" [ "commit" ] |> run;
        let current_diff_commit =
          process "git" [ "rev-parse"; "--short"; "HEAD" ] |> collect stdout
        in
        let new_split_commit =
          match split_commit with
          | HeadCommit head_commit ->
              HeadCommit { head_commit with reference_commit = current_diff_commit }
          | IntermediaryCommit intermediary_commit ->
              IntermediaryCommit { intermediary_commit with reference_commit = current_diff_commit }
        in
        select_changes new_split_commit

let () =
  Arg.parse speclist anon_fun usage_msg;

  if !example then
    let _result = Tui.run Tui.initial_model in
    ()
  else
    let starting_reference_commit_id =
      process "git" [ "rev-parse"; "--short"; !commit_id ^ "~1" ] |> collect stdout
    in
    let head_commit_id = process "git" [ "rev-parse"; "--short"; "HEAD" ] |> collect stdout in
    let split_commit =
      if !commit_id = "HEAD" then
        HeadCommit
          { reference_commit = starting_reference_commit_id; target_commit = head_commit_id }
      else
        let branch = process "git" [ "rev-parse"; "--abbrev-ref"; "HEAD" ] |> collect stdout in
        IntermediaryCommit
          {
            reference_commit = starting_reference_commit_id;
            target_commit = !commit_id;
            original_head = head_commit_id;
            branch;
          }
    in

    print_endline @@ "Resetting to commit " ^ starting_reference_commit_id;
    process "git" [ "reset"; "--hard"; starting_reference_commit_id ] |> run;

    select_changes split_commit
