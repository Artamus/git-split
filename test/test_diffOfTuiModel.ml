open Alcotest
open Git_split

let diff_testable = testable Diff.pp_diff Diff.equal_diff

let changed_file_single_hunk () =
  let tui_model : NottyTui.model =
    {
      cursor = FileCursor 0;
      files =
        [
          ChangedFile
            {
              path = "src/main";
              hunks =
                [
                  {
                    starting_line = 1;
                    context_snippet = None;
                    lines =
                      [
                        Context "context";
                        Diff ("removed-line", `removed, `included);
                        Diff ("added-line", `added, `included);
                        Context "context";
                      ];
                    lines_visibility = Expanded;
                  };
                ];
              hunks_visibility = Collapsed;
            };
        ];
    }
  in

  let diff = NottyTui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = "src/main";
              hunks =
                [
                  {
                    starting_line = 1;
                    context_snippet = None;
                    lines =
                      [
                        `ContextLine "context";
                        `RemovedLine "removed-line";
                        `AddedLine "added-line";
                        `ContextLine "context";
                      ];
                  };
                ];
            };
        ];
    }
  in
  check diff_testable "same diffs" expected diff

let changed_file_multiple_hunks () =
  let tui_model : NottyTui.model =
    {
      cursor = FileCursor 0;
      files =
        [
          ChangedFile
            {
              path = "src/main";
              hunks =
                [
                  {
                    starting_line = 1;
                    context_snippet = None;
                    lines =
                      [
                        Context "context";
                        Diff ("removed-line", `removed, `included);
                        Diff ("added-line", `added, `included);
                        Context "context";
                      ];
                    lines_visibility = Expanded;
                  };
                  {
                    starting_line = 15;
                    context_snippet = Some "context";
                    lines =
                      [
                        Context "context";
                        Diff ("removed-line", `removed, `included);
                        Diff ("added-line", `added, `included);
                        Context "context";
                      ];
                    lines_visibility = Expanded;
                  };
                ];
              hunks_visibility = Collapsed;
            };
        ];
    }
  in

  let diff = NottyTui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = "src/main";
              hunks =
                [
                  {
                    starting_line = 1;
                    context_snippet = None;
                    lines =
                      [
                        `ContextLine "context";
                        `RemovedLine "removed-line";
                        `AddedLine "added-line";
                        `ContextLine "context";
                      ];
                  };
                  {
                    starting_line = 15;
                    context_snippet = Some "context";
                    lines =
                      [
                        `ContextLine "context";
                        `RemovedLine "removed-line";
                        `AddedLine "added-line";
                        `ContextLine "context";
                      ];
                  };
                ];
            };
        ];
    }
  in
  check diff_testable "same diffs" expected diff

let renamed_file_without_content_changes () =
  let tui_model : NottyTui.model =
    {
      cursor = FileCursor 0;
      files =
        [
          RenamedFile
            { old_path = "src/old"; new_path = "src/new"; hunks = []; included = `included };
        ];
    }
  in

  let diff = NottyTui.diff_of_model tui_model in

  let expected : Diff.diff =
    { files = [ RenamedFile { old_path = "src/old"; new_path = "src/new"; hunks = [] } ] }
  in
  check diff_testable "same diffs" expected diff

let renamed_file_with_content_changes () =
  let tui_model : NottyTui.model =
    {
      cursor = FileCursor 0;
      files =
        [
          RenamedFile
            {
              old_path = "src/old";
              new_path = "src/new";
              hunks =
                [
                  {
                    starting_line = 15;
                    context_snippet = Some "context";
                    lines =
                      [
                        Context "context";
                        Diff ("removed-line", `removed, `included);
                        Diff ("added-line", `added, `included);
                        Context "context";
                      ];
                    lines_visibility = Expanded;
                  };
                ];
              included = `included;
            };
        ];
    }
  in

  let diff = NottyTui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          RenamedFile
            {
              old_path = "src/old";
              new_path = "src/new";
              hunks =
                [
                  {
                    starting_line = 15;
                    context_snippet = Some "context";
                    lines =
                      [
                        `ContextLine "context";
                        `RemovedLine "removed-line";
                        `AddedLine "added-line";
                        `ContextLine "context";
                      ];
                  };
                ];
            };
        ];
    }
  in
  check diff_testable "same diffs" expected diff

let multiple_files () =
  let tui_model : NottyTui.model =
    {
      cursor = FileCursor 0;
      files =
        [
          ChangedFile
            {
              path = "src/main";
              hunks =
                [
                  {
                    starting_line = 1;
                    context_snippet = None;
                    lines =
                      [
                        Context "context";
                        Diff ("removed-line", `removed, `included);
                        Diff ("added-line", `added, `included);
                        Context "context";
                      ];
                    lines_visibility = Expanded;
                  };
                ];
              hunks_visibility = Collapsed;
            };
          ChangedFile
            {
              path = "src/other";
              hunks =
                [
                  {
                    starting_line = 15;
                    context_snippet = Some "context";
                    lines =
                      [
                        Context "context";
                        Diff ("removed-line", `removed, `included);
                        Diff ("added-line", `added, `included);
                        Context "context";
                      ];
                    lines_visibility = Expanded;
                  };
                ];
              hunks_visibility = Collapsed;
            };
        ];
    }
  in

  let diff = NottyTui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = "src/main";
              hunks =
                [
                  {
                    starting_line = 1;
                    context_snippet = None;
                    lines =
                      [
                        `ContextLine "context";
                        `RemovedLine "removed-line";
                        `AddedLine "added-line";
                        `ContextLine "context";
                      ];
                  };
                ];
            };
          ChangedFile
            {
              path = "src/other";
              hunks =
                [
                  {
                    starting_line = 15;
                    context_snippet = Some "context";
                    lines =
                      [
                        `ContextLine "context";
                        `RemovedLine "removed-line";
                        `AddedLine "added-line";
                        `ContextLine "context";
                      ];
                  };
                ];
            };
        ];
    }
  in
  check diff_testable "same diffs" expected diff

let diff_of_tui_model_suite =
  [
    ("single hunk", `Quick, changed_file_single_hunk);
    ("multiple hunks", `Quick, changed_file_multiple_hunks);
    (* TODO: Deleted file. *)
    (* TODO: Created file. *)
    ("renamed file without content", `Quick, renamed_file_without_content_changes);
    ("renamed file with content changes", `Quick, renamed_file_with_content_changes);
    ("multiple files", `Quick, multiple_files);
  ]
