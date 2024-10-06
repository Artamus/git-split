open Git_split

let ( let* ) = Result.bind
let usage_msg = "git-split [commit-id]"
let commit_id = ref "HEAD"
let example = ref false
let view_only = ref false
let anon_fun selected_commit = commit_id := selected_commit

let speclist =
  [
    ("[commit-id]", Arg.Set_string commit_id, "Commit to split. Defaults to HEAD.");
    ("--example", Arg.Set example, "Run TUI with example code, overrides all other options.");
    ("--view-only", Arg.Set view_only, "Run TUI in selection mode, but do not make any changes.");
  ]

let process cmd args = Unix.open_process_args_full cmd (Array.concat [ [| cmd |]; args ]) [||]

let collect (in_channel, out_channel, err_channel) =
  let content = In_channel.input_all in_channel in
  let err_content = In_channel.input_all err_channel in
  let status = Unix.close_process_full (in_channel, out_channel, err_channel) in

  let content_length = String.length content in
  let content =
    if content.[content_length - 1] = '\n' then String.sub content 0 (content_length - 2)
    else content
  in
  (status, content, err_content)

let run (in_channel, out_channel, err_channel) =
  let _ = Unix.close_process_full (in_channel, out_channel, err_channel) in
  ()

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
    [|
      "rebase";
      "--onto";
      intermediary_commit.reference_commit;
      intermediary_commit.target_commit ^ "~1";
      intermediary_commit.original_head;
    |]
  |> run;
  let _, head_commit_id, _ = process "git" [| "rev-parse"; "--short"; "HEAD" |] |> collect in
  process "git" [| "branch"; "-f"; intermediary_commit.branch; head_commit_id |] |> run;
  ()

let rec select_changes reference_commit target_commit =
  let _, head_diff, _ =
    process "git" [| "diff"; "--binary"; reference_commit; target_commit |] |> collect
  in
  if String.length head_diff = 0 then Result.ok ()
  else
    let* diff = DiffParser.parse head_diff in
    let* tui_model =
      Tui.model_of_diff diff
      |> Option.to_result
           ~none:
             (Printf.sprintf "unable to parse diff between %s and %s" reference_commit target_commit)
    in
    let* final_model = Tui.run tui_model |> Option.to_result ~none:"aborted by user" in
    let selected_diff = Tui.diff_of_model final_model in
    let serialized_diff = DiffSerializer.serialize selected_diff in
    let tmp_filename = Printf.sprintf "/tmp/%s.diff" reference_commit in
    process "echo" [| serialized_diff; ">"; tmp_filename |] |> run;
    let apply_status, _, apply_error = process "git" [| "apply"; tmp_filename |] |> collect in
    if apply_status <> Unix.WEXITED 0 then
      Error ("Failed to apply selected diff with error \"" ^ apply_error ^ "\"")
    else (
      process "git" [| "add"; "." |] |> run;
      process "git" [| "commit" |] |> run;
      let _, current_diff_commit, _ =
        process "git" [| "rev-parse"; "--short"; "HEAD" |] |> collect
      in
      select_changes current_diff_commit target_commit)

let () =
  Arg.parse speclist anon_fun usage_msg;

  if !example then
    let _result = Tui.run TuiModelExample.model in
    ()
  else if !view_only then
    let _, raw_diff, _ =
      process "git" [| "diff"; !commit_id ^ "~"; !commit_id; "--binary" |] |> collect
    in
    let diff =
      DiffParser.parse raw_diff
      |> Result.map_error (fun err -> Printf.sprintf "Failed to parse diff, %s" err)
    in
    let model =
      Result.bind diff (fun diff ->
          Tui.model_of_diff diff |> Option.to_result ~none:"Unable to convert diff to TUI model")
    in
    model
    |> Result.fold
         ~ok:(fun model ->
           let _result = Tui.run model in
           ())
         ~error:(fun err -> print_endline err)
  else
    let _, starting_reference_commit_id, _ =
      process "git" [| "rev-parse"; "--short"; !commit_id ^ "~1" |] |> collect
    in
    let _, head_commit_id, _ = process "git" [| "rev-parse"; "--short"; "HEAD" |] |> collect in
    let split_commit =
      if !commit_id = "HEAD" then
        HeadCommit
          { reference_commit = starting_reference_commit_id; target_commit = head_commit_id }
      else
        let _, branch, _ = process "git" [| "rev-parse"; "--abbrev-ref"; "HEAD" |] |> collect in
        IntermediaryCommit
          {
            reference_commit = starting_reference_commit_id;
            target_commit = !commit_id;
            original_head = head_commit_id;
            branch;
          }
    in

    print_endline @@ "Resetting to commit " ^ starting_reference_commit_id;
    process "git" [| "reset"; "--hard"; starting_reference_commit_id |] |> run;

    let split_res =
      match split_commit with
      | HeadCommit { reference_commit; target_commit } ->
          select_changes reference_commit target_commit
      | IntermediaryCommit { reference_commit; target_commit; _ } ->
          select_changes reference_commit target_commit
    in
    Result.fold
      ~ok:(fun () ->
        match split_commit with
        | HeadCommit _ -> ()
        | IntermediaryCommit intermediary_commit ->
            cleanup intermediary_commit;
            print_endline "Job's done!";
            ())
      ~error:(fun err ->
        print_endline err;
        print_endline "Resetting HEAD";
        process "git" [| "reset"; "--hard"; head_commit_id |] |> run;
        ())
      split_res
