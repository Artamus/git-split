open Alcotest
open Git_split

let diff_testable = testable Diff.pp_diff Diff.equal_diff

let changed_file_single_hunk () =
  let tui_model : NottyTui.model =
    {
      cursor = FileCursor 0;
      files =
        [
          {
            path = FilePath "src/main";
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
          {
            path = FilePath "src/main";
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

let deleted_file () =
  let tui_model : NottyTui.model =
    {
      cursor = FileCursor 0;
      files =
        [
          {
            path = FilePath "src/deleted";
            hunks =
              [
                {
                  starting_line = 1;
                  context_snippet = None;
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

  let diff = NottyTui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          DeletedFile
            {
              path = "src/deleted";
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
  check diff_testable "same diffs" expected diff

let created_file () =
  let tui_model : NottyTui.model =
    {
      cursor = FileCursor 0;
      files =
        [
          {
            path = FilePath "src/created";
            hunks =
              [
                {
                  starting_line = 1;
                  context_snippet = None;
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

  let diff = NottyTui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          CreatedFile
            {
              path = "src/created";
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
  check diff_testable "same diffs" expected diff

let renamed_file_without_content_changes () =
  let tui_model : NottyTui.model =
    {
      cursor = FileCursor 0;
      files =
        [
          {
            path = ChangedPath { old_path = "src/old"; new_path = "src/new" };
            hunks = [];
            hunks_visibility = Collapsed;
          };
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
          {
            path = ChangedPath { old_path = "src/old"; new_path = "src/new" };
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
          {
            path = FilePath "src/main";
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
          {
            path = FilePath "src/other";
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

let unselected_removed_become_context () =
  let tui_model : NottyTui.model =
    {
      cursor = FileCursor 0;
      files =
        [
          {
            path = FilePath "src/main";
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
                      Diff ("unselected-removed-line", `removed, `notincluded);
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
                        `ContextLine "unselected-removed-line";
                        `ContextLine "context";
                      ];
                  };
                ];
            };
        ];
    }
  in
  check diff_testable "same diffs" expected diff

let unselected_added_are_excluded () =
  let tui_model : NottyTui.model =
    {
      cursor = FileCursor 0;
      files =
        [
          {
            path = FilePath "src/main";
            hunks =
              [
                {
                  starting_line = 1;
                  context_snippet = None;
                  lines =
                    [
                      Context "context";
                      Diff ("removed-line", `removed, `included);
                      Diff ("unselected-added-line", `added, `notincluded);
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
                        `ContextLine "context"; `RemovedLine "removed-line"; `ContextLine "context";
                      ];
                  };
                ];
            };
        ];
    }
  in
  check diff_testable "same diffs" expected diff

let created_with_unselected_is_created () =
  let tui_model : NottyTui.model =
    {
      cursor = FileCursor 0;
      files =
        [
          {
            path = FilePath "src/created";
            hunks =
              [
                {
                  starting_line = 1;
                  context_snippet = None;
                  lines =
                    [
                      Diff ("added-line-1", `added, `included);
                      Diff ("added-line-2", `added, `included);
                      Diff ("added-line-3", `added, `notincluded);
                      Diff ("added-line-4", `added, `notincluded);
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
          CreatedFile
            {
              path = "src/created";
              lines = [ `AddedLine "added-line-1"; `AddedLine "added-line-2" ];
            };
        ];
    }
  in
  check diff_testable "same diffs" expected diff

let deleted_with_unselected_is_changed () =
  let tui_model : NottyTui.model =
    {
      cursor = FileCursor 0;
      files =
        [
          {
            path = FilePath "src/deleted";
            hunks =
              [
                {
                  starting_line = 1;
                  context_snippet = None;
                  lines =
                    [
                      Diff ("removed-line-1", `removed, `notincluded);
                      Diff ("removed-line-2", `removed, `notincluded);
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

  let diff = NottyTui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = "src/deleted";
              hunks =
                [
                  {
                    starting_line = 1;
                    context_snippet = None;
                    lines =
                      [
                        `ContextLine "removed-line-1";
                        `ContextLine "removed-line-2";
                        `RemovedLine "removed-line-3";
                        `RemovedLine "removed-line-4";
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
    ("deleted file", `Quick, deleted_file);
    ("created file", `Quick, created_file);
    ("renamed file without content", `Quick, renamed_file_without_content_changes);
    ("renamed file with content changes", `Quick, renamed_file_with_content_changes);
    ("multiple files", `Quick, multiple_files);
    ( "unselected removed lines become context lines in the diff",
      `Quick,
      unselected_removed_become_context );
    ("unselected added lines are excluded from the diff", `Quick, unselected_added_are_excluded);
    ( "created file with some unselected lines remains a created file",
      `Quick,
      created_with_unselected_is_created );
    ( "deleted file with some unselected lines becomes a changed file",
      `Quick,
      deleted_with_unselected_is_changed );
  ]
