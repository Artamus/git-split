open Alcotest
open Git_split

let tui_model_testable = testable NottyTui.pp_model NottyTui.equal_model

let changed_file_single_hunk () =
  let diff : Diff.diff =
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

  let tui_model = NottyTui.model_of_diff diff in

  let expected : NottyTui.model =
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
  check tui_model_testable "same TUI model" expected tui_model

let changed_file_multiple_hunks () =
  let diff : Diff.diff =
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

  let tui_model = NottyTui.model_of_diff diff in

  let expected : NottyTui.model =
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
  check tui_model_testable "same TUI model" expected tui_model

let deleted_file () =
  let diff : Diff.diff =
    {
      files =
        [
          DeletedFile
            {
              path = "src/main";
              lines =
                [
                  `RemovedLine "removed-line-1";
                  `RemovedLine "removed-line-2";
                  `RemovedLine "removed-line-3";
                  `RemovedLine "removed-line-4";
                ];
            };
        ];
    }
  in

  let tui_model = NottyTui.model_of_diff diff in

  let expected : NottyTui.model =
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
                    lines =
                      [
                        Diff ("removed-line-1", `removed, `included);
                        Diff ("removed-line-2", `removed, `included);
                        Diff ("removed-line-3", `removed, `included);
                        Diff ("removed-line-4", `removed, `included);
                      ];
                    lines_visibility = Expanded;
                  };
                ];
              hunks_visibility = Collapsed;
            };
        ];
    }
  in
  check tui_model_testable "same TUI model" expected tui_model

let created_file () =
  let diff : Diff.diff =
    {
      files =
        [
          CreatedFile
            {
              path = "src/main";
              lines =
                [
                  `AddedLine "added-line-1";
                  `AddedLine "added-line-2";
                  `AddedLine "added-line-3";
                  `AddedLine "added-line-4";
                ];
            };
        ];
    }
  in

  let tui_model = NottyTui.model_of_diff diff in

  let expected : NottyTui.model =
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
                    lines =
                      [
                        Diff ("added-line-1", `added, `included);
                        Diff ("added-line-2", `added, `included);
                        Diff ("added-line-3", `added, `included);
                        Diff ("added-line-4", `added, `included);
                      ];
                    lines_visibility = Expanded;
                  };
                ];
              hunks_visibility = Collapsed;
            };
        ];
    }
  in
  check tui_model_testable "same TUI model" expected tui_model

let renamed_file_without_content_changes () =
  let diff : Diff.diff =
    { files = [ RenamedFile { old_path = "src/old"; new_path = "src/new"; hunks = [] } ] }
  in

  let tui_model = NottyTui.model_of_diff diff in

  let expected : NottyTui.model =
    {
      cursor = FileCursor 0;
      files = [ RenamedFile { old_path = "src/old"; new_path = "src/new"; included = `included } ];
    }
  in
  check tui_model_testable "same TUI model" expected tui_model

let renamed_file_with_content_changes () =
  let diff : Diff.diff =
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

  let tui_model = NottyTui.model_of_diff diff in

  (* TODO: Support changes. *)
  let expected : NottyTui.model =
    {
      cursor = FileCursor 0;
      files = [ RenamedFile { old_path = "src/old"; new_path = "src/new"; included = `included } ];
    }
  in
  check tui_model_testable "same TUI model" expected tui_model

let multiple_files () =
  let diff : Diff.diff =
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

  let tui_model = NottyTui.model_of_diff diff in

  let expected : NottyTui.model =
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
  check tui_model_testable "same TUI model" expected tui_model

let tui_model_of_diff_suite =
  [
    ("single hunk", `Quick, changed_file_single_hunk);
    ("multiple hunks", `Quick, changed_file_multiple_hunks);
    ("deleted file", `Quick, deleted_file);
    ("created file", `Quick, created_file);
    ("renamed file without content", `Quick, renamed_file_without_content_changes);
    ("renamed file with content", `Quick, renamed_file_with_content_changes);
    ("multiple files", `Quick, multiple_files);
  ]
