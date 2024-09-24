open Alcotest
open Git_split
open Git_split.TuiTypes

let tui_model_testable = testable TuiModel.pp_model TuiModel.equal_model
let optional_tui_model_testable = Alcotest.option tui_model_testable

let changed_file_single_hunk () =
  let diff : Diff.diff =
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

  let tui_model = Tui.model_of_diff diff in

  let expected : TuiModel.model option =
    Some
      (File
         (Zipper.from_list_exn
            [
              {
                path = Path "src/main";
                visibility = Collapsed;
                content =
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
            ]))
  in
  check optional_tui_model_testable "same TUI model" expected tui_model

let changed_file_multiple_hunks () =
  let diff : Diff.diff =
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

  let tui_model = Tui.model_of_diff diff in

  let expected : TuiModel.model option =
    Some
      (File
         (Zipper.from_list_exn
            [
              {
                path = Path "src/main";
                visibility = Collapsed;
                content =
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
            ]))
  in
  check optional_tui_model_testable "same TUI model" expected tui_model

let deleted_file () =
  let diff : Diff.diff =
    {
      files =
        [
          DeletedFile
            {
              path = "src/main";
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

  let tui_model = Tui.model_of_diff diff in

  let expected : TuiModel.model option =
    Some
      (File
         (Zipper.from_list_exn
            [
              {
                path = Path "src/main";
                visibility = Collapsed;
                content =
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
            ]))
  in
  check optional_tui_model_testable "same TUI model" expected tui_model

let created_file () =
  let diff : Diff.diff =
    {
      files =
        [
          CreatedFile
            {
              path = "src/main";
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

  let tui_model = Tui.model_of_diff diff in

  let expected : TuiModel.model option =
    Some
      (File
         (Zipper.from_list_exn
            [
              {
                path = Path "src/main";
                visibility = Collapsed;
                content =
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
            ]))
  in
  check optional_tui_model_testable "same TUI model" expected tui_model

let renamed_file_without_content_changes () =
  let diff : Diff.diff =
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

  let tui_model = Tui.model_of_diff diff in

  let expected : TuiModel.model option =
    Some
      (File
         (Zipper.from_list_exn
            [
              {
                path = ChangedPath { old_path = "src/old"; new_path = "src/new" };
                visibility = Collapsed;
                content = [];
              };
            ]))
  in

  check optional_tui_model_testable "same TUI model" expected tui_model

let renamed_file_with_content_changes () =
  let diff : Diff.diff =
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

  let tui_model = Tui.model_of_diff diff in

  let expected : TuiModel.model option =
    Some
      (File
         (Zipper.from_list_exn
            [
              {
                path = ChangedPath { old_path = "src/old"; new_path = "src/new" };
                visibility = Collapsed;
                content =
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
            ]))
  in

  check optional_tui_model_testable "same TUI model" expected tui_model

let multiple_files () =
  let diff : Diff.diff =
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

  let tui_model = Tui.model_of_diff diff in

  let expected : TuiModel.model option =
    Some
      (File
         (Zipper.from_list_exn
            [
              {
                path = Path "src/main";
                visibility = Collapsed;
                content =
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
                path = Path "src/other";
                visibility = Collapsed;
                content =
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
            ]))
  in
  check optional_tui_model_testable "same TUI model" expected tui_model

let tui_model_of_diff_suite =
  [
    ("containing a single hunk", `Quick, changed_file_single_hunk);
    ("containing multiple hunks", `Quick, changed_file_multiple_hunks);
    ("containing deleted file", `Quick, deleted_file);
    ("containing created file", `Quick, created_file);
    ("containing renamed file without content", `Quick, renamed_file_without_content_changes);
    ("containing renamed file with content", `Quick, renamed_file_with_content_changes);
    ("containing multiple files", `Quick, multiple_files);
  ]
