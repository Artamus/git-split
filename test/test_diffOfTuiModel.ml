open Alcotest
open Git_split
open Git_split.TuiTypes

let diff_testable = testable Diff.pp_diff Diff.equal_diff

let changed_file_single_hunk () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = FilePath "src/main";
             visibility = Collapsed;
             hunks =
               [
                 {
                   starting_line = 1;
                   context_snippet = None;
                   visibility = Expanded;
                   lines =
                     [
                       Context "context";
                       Diff ("removed-line", `removed, `included);
                       Diff ("added-line", `added, `included);
                       Context "context";
                     ];
                 };
               ];
           };
         ])
  in

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "src/main";
              mode_change = None;
              content =
                `Text
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
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = FilePath "src/main";
             visibility = Collapsed;
             hunks =
               [
                 {
                   starting_line = 1;
                   context_snippet = None;
                   visibility = Expanded;
                   lines =
                     [
                       Context "context";
                       Diff ("removed-line", `removed, `included);
                       Diff ("added-line", `added, `included);
                       Context "context";
                     ];
                 };
                 {
                   starting_line = 15;
                   context_snippet = Some "context";
                   visibility = Expanded;
                   lines =
                     [
                       Context "context";
                       Diff ("removed-line", `removed, `included);
                       Diff ("added-line", `added, `included);
                       Context "context";
                     ];
                 };
               ];
           };
         ])
  in

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "src/main";
              mode_change = None;
              content =
                `Text
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
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = FilePath "src/deleted";
             visibility = Collapsed;
             hunks =
               [
                 {
                   starting_line = 1;
                   context_snippet = None;
                   visibility = Expanded;
                   lines =
                     [
                       Diff ("removed-line-1", `removed, `included);
                       Diff ("removed-line-2", `removed, `included);
                       Diff ("removed-line-3", `removed, `included);
                       Diff ("removed-line-4", `removed, `included);
                     ];
                 };
               ];
           };
         ])
  in

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          DeletedFile
            {
              path = "src/deleted";
              mode = 100644;
              content =
                `Text
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
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = FilePath "src/created";
             visibility = Collapsed;
             hunks =
               [
                 {
                   starting_line = 1;
                   context_snippet = None;
                   visibility = Expanded;
                   lines =
                     [
                       Diff ("added-line-1", `added, `included);
                       Diff ("added-line-2", `added, `included);
                       Diff ("added-line-3", `added, `included);
                       Diff ("added-line-4", `added, `included);
                     ];
                 };
               ];
           };
         ])
  in

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          CreatedFile
            {
              path = "src/created";
              mode = 100644;
              content =
                `Text
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
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = ChangedPath { old_path = "src/old"; new_path = "src/new" };
             visibility = Collapsed;
             hunks = [];
           };
         ])
  in

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = ChangedPath { old_path = "src/old"; new_path = "src/new" };
              mode_change = None;
              content = `Text [];
            };
        ];
    }
  in
  check diff_testable "same diffs" expected diff

let renamed_file_with_content_changes () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = ChangedPath { old_path = "src/old"; new_path = "src/new" };
             visibility = Collapsed;
             hunks =
               [
                 {
                   starting_line = 15;
                   context_snippet = Some "context";
                   visibility = Expanded;
                   lines =
                     [
                       Context "context";
                       Diff ("removed-line", `removed, `included);
                       Diff ("added-line", `added, `included);
                       Context "context";
                     ];
                 };
               ];
           };
         ])
  in

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = ChangedPath { old_path = "src/old"; new_path = "src/new" };
              mode_change = None;
              content =
                `Text
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

let multiple_changed_files () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = FilePath "src/main";
             visibility = Collapsed;
             hunks =
               [
                 {
                   starting_line = 1;
                   context_snippet = None;
                   visibility = Expanded;
                   lines =
                     [
                       Context "context";
                       Diff ("removed-line", `removed, `included);
                       Diff ("added-line", `added, `included);
                       Context "context";
                     ];
                 };
               ];
           };
           {
             path = FilePath "src/other";
             visibility = Collapsed;
             hunks =
               [
                 {
                   starting_line = 15;
                   context_snippet = Some "context";
                   visibility = Expanded;
                   lines =
                     [
                       Context "context";
                       Diff ("removed-line", `removed, `included);
                       Diff ("added-line", `added, `included);
                       Context "context";
                     ];
                 };
               ];
           };
         ])
  in

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "src/main";
              mode_change = None;
              content =
                `Text
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
              path = Path "src/other";
              mode_change = None;
              content =
                `Text
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
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = FilePath "src/main";
             visibility = Collapsed;
             hunks =
               [
                 {
                   starting_line = 1;
                   context_snippet = None;
                   visibility = Expanded;
                   lines =
                     [
                       Context "context";
                       Diff ("removed-line", `removed, `included);
                       Diff ("added-line", `added, `included);
                       Diff ("unselected-removed-line", `removed, `notincluded);
                       Context "context";
                     ];
                 };
               ];
           };
         ])
  in

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "src/main";
              mode_change = None;
              content =
                `Text
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
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = FilePath "src/main";
             visibility = Collapsed;
             hunks =
               [
                 {
                   starting_line = 1;
                   context_snippet = None;
                   visibility = Expanded;
                   lines =
                     [
                       Context "context";
                       Diff ("removed-line", `removed, `included);
                       Diff ("unselected-added-line", `added, `notincluded);
                       Context "context";
                     ];
                 };
               ];
           };
         ])
  in

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "src/main";
              mode_change = None;
              content =
                `Text
                  [
                    {
                      starting_line = 1;
                      context_snippet = None;
                      lines =
                        [
                          `ContextLine "context";
                          `RemovedLine "removed-line";
                          `ContextLine "context";
                        ];
                    };
                  ];
            };
        ];
    }
  in
  check diff_testable "same diffs" expected diff

let created_with_unselected_is_created () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = FilePath "src/created";
             visibility = Collapsed;
             hunks =
               [
                 {
                   starting_line = 1;
                   context_snippet = None;
                   visibility = Expanded;
                   lines =
                     [
                       Diff ("added-line-1", `added, `included);
                       Diff ("added-line-2", `added, `included);
                       Diff ("added-line-3", `added, `notincluded);
                       Diff ("added-line-4", `added, `notincluded);
                     ];
                 };
               ];
           };
         ])
  in

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          CreatedFile
            {
              path = "src/created";
              mode = 100644;
              content = `Text [ `AddedLine "added-line-1"; `AddedLine "added-line-2" ];
            };
        ];
    }
  in
  check diff_testable "same diffs" expected diff

let deleted_with_unselected_is_changed () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = FilePath "src/deleted";
             visibility = Collapsed;
             hunks =
               [
                 {
                   starting_line = 1;
                   context_snippet = None;
                   visibility = Expanded;
                   lines =
                     [
                       Diff ("removed-line-1", `removed, `notincluded);
                       Diff ("removed-line-2", `removed, `notincluded);
                       Diff ("removed-line-3", `removed, `included);
                       Diff ("removed-line-4", `removed, `included);
                     ];
                 };
               ];
           };
         ])
  in

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "src/deleted";
              mode_change = None;
              content =
                `Text
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
    ("with a single changed hunk", `Quick, changed_file_single_hunk);
    ("with multiple changed hunks", `Quick, changed_file_multiple_hunks);
    ("with a deleted file", `Quick, deleted_file);
    ("with a created file", `Quick, created_file);
    ("with a renamed file without content", `Quick, renamed_file_without_content_changes);
    ("with a renamed file with content changes", `Quick, renamed_file_with_content_changes);
    ("with multiple changed files", `Quick, multiple_changed_files);
    ( "unselected removed lines become context lines in the diff",
      `Quick,
      unselected_removed_become_context );
    ("excludes unselected added lines from the diff", `Quick, unselected_added_are_excluded);
    ( "keeps a created file with some unselected lines a created file",
      `Quick,
      created_with_unselected_is_created );
    ( "changes a deleted file with some unselected lines to a changed file",
      `Quick,
      deleted_with_unselected_is_changed );
  ]
