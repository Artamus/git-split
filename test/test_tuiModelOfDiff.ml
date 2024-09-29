open Alcotest
open Git_split
open Git_split.TuiTypes

let tui_model_testable = testable TuiModel.pp_model TuiModel.equal_model
let optional_tui_model_testable = Alcotest.option tui_model_testable

let test_changed_file_text_content () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "src/test";
              mode_change = None;
              content =
                `Text
                  [
                    {
                      starting_line = 4;
                      context_snippet = Some "fun main() {";
                      lines =
                        [
                          `ContextLine "  hunk-1-unchanged-line";
                          `RemovedLine "  hunk-1-removed-line";
                          `RemovedLine "  hunk-1-removed-line";
                          `AddedLine "  hunk-1-added-line";
                          `ContextLine "  hunk-1-unchanged-line";
                        ];
                    };
                    {
                      starting_line = 57;
                      context_snippet = None;
                      lines =
                        [
                          `ContextLine "  hunk-2-unchanged-line";
                          `AddedLine "  hunk-2-added-line";
                          `ContextLine "  hunk-2-unchanged-line";
                        ];
                    };
                  ];
            };
        ];
    }
  in

  let tui_model = Tui.model_of_diff diff in

  let expected : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "src/test";
             mode = None;
             content =
               Text
                 {
                   visibility = Collapsed;
                   hunks =
                     [
                       {
                         starting_line = 4;
                         context_snippet = Some "fun main() {";
                         visibility = Expanded;
                         lines =
                           [
                             Context "  hunk-1-unchanged-line";
                             Diff ("  hunk-1-removed-line", `removed, `included);
                             Diff ("  hunk-1-removed-line", `removed, `included);
                             Diff ("  hunk-1-added-line", `added, `included);
                             Context "  hunk-1-unchanged-line";
                           ];
                       };
                       {
                         starting_line = 57;
                         context_snippet = None;
                         visibility = Expanded;
                         lines =
                           [
                             Context "  hunk-2-unchanged-line";
                             Diff ("  hunk-2-added-line", `added, `included);
                             Context "  hunk-2-unchanged-line";
                           ];
                       };
                     ];
                 };
           };
         ])
  in
  check optional_tui_model_testable "same TUI model" (Some expected) tui_model

let test_changed_file_binary_content () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "test-font.ttf";
              mode_change = None;
              content =
                `Binary
                  "literal 94372\n\
                   zcmcG$2Ur|O(g50F134|Qz%H=M!tTNnHZK{L3<7~bD4@g=B#;mwf+blwpa{yzN|r1+";
            };
        ];
    }
  in

  let tui_model = Tui.model_of_diff diff in

  let expected : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "test-font.ttf";
             mode = None;
             content =
               Binary
                 ( "literal 94372\n\
                    zcmcG$2Ur|O(g50F134|Qz%H=M!tTNnHZK{L3<7~bD4@g=B#;mwf+blwpa{yzN|r1+",
                   `included );
           };
         ])
  in
  check optional_tui_model_testable "same TUI model" (Some expected) tui_model

let test_changed_file_renamed () =
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

  let expected : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = ChangedPath { old_path = "src/old"; new_path = "src/new" };
             mode = None;
             content = Text { visibility = Collapsed; hunks = [] };
           };
         ])
  in
  check optional_tui_model_testable "same TUI model" (Some expected) tui_model

let test_changed_file_renamed_with_text_content () =
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

  let expected : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = ChangedPath { old_path = "src/old"; new_path = "src/new" };
             mode = None;
             content =
               Text
                 {
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
           };
         ])
  in
  check optional_tui_model_testable "same TUI model" (Some expected) tui_model

(* TODO: Empty created file should not have any hunks. *)
let test_empty_created_file () =
  let diff : Diff.diff =
    { files = [ CreatedFile { path = "empty-new-file.md"; mode = 100644; content = `Text [] } ] }
  in

  let tui_model = Tui.model_of_diff diff in

  let expected : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "empty-new-file.md";
             mode = Some (Mode 100644);
             content =
               Text
                 {
                   visibility = Collapsed;
                   hunks =
                     [
                       {
                         starting_line = 1;
                         context_snippet = None;
                         visibility = Expanded;
                         lines = [];
                       };
                     ];
                 };
           };
         ])
  in
  check optional_tui_model_testable "same TUI model" (Some expected) tui_model

let test_created_file_text_content () =
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
                    `AddedLine "added-line-5";
                  ];
            };
        ];
    }
  in

  let tui_model = Tui.model_of_diff diff in

  let expected : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "src/main";
             mode = Some (Mode 100644);
             content =
               Text
                 {
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
                             Diff ("added-line-5", `added, `included);
                           ];
                       };
                     ];
                 };
           };
         ])
  in
  check optional_tui_model_testable "same TUI model" (Some expected) tui_model

let test_created_file_binary_content () =
  let diff : Diff.diff =
    { files = [ CreatedFile { path = "foo.bin"; mode = 100755; content = `Binary "literal 18" } ] }
  in

  let tui_model = Tui.model_of_diff diff in

  let expected : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "foo.bin";
             mode = Some (Mode 100755);
             content = Binary ("literal 18", `included);
           };
         ])
  in
  check optional_tui_model_testable "same TUI model" (Some expected) tui_model

(* TODO: Empty created file should not have any hunks. *)
let test_empty_deleted_file () =
  let diff : Diff.diff =
    { files = [ DeletedFile { path = "empty-new-file.md"; mode = 100644; content = `Text [] } ] }
  in

  let tui_model = Tui.model_of_diff diff in

  let expected : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "empty-new-file.md";
             mode = Some (Mode 100644);
             content =
               Text
                 {
                   visibility = Collapsed;
                   hunks =
                     [
                       {
                         starting_line = 1;
                         context_snippet = None;
                         visibility = Expanded;
                         lines = [];
                       };
                     ];
                 };
           };
         ])
  in
  check optional_tui_model_testable "same TUI model" (Some expected) tui_model

let test_deleted_file_text_content () =
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
                    `RemovedLine "removed-line-5";
                  ];
            };
        ];
    }
  in

  let tui_model = Tui.model_of_diff diff in

  let expected : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "src/main";
             mode = Some (Mode 100644);
             content =
               Text
                 {
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
                             Diff ("removed-line-5", `removed, `included);
                           ];
                       };
                     ];
                 };
           };
         ])
  in
  check optional_tui_model_testable "same TUI model" (Some expected) tui_model

let test_deleted_file_binary_content () =
  let diff : Diff.diff =
    {
      files =
        [
          DeletedFile
            { path = "foo.bin"; mode = 100755; content = `Binary "literal 0\nHcmV?d00001" };
        ];
    }
  in

  let tui_model = Tui.model_of_diff diff in

  let expected : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "foo.bin";
             mode = Some (Mode 100755);
             content = Binary ("literal 0\nHcmV?d00001", `included);
           };
         ])
  in
  check optional_tui_model_testable "same TUI model" (Some expected) tui_model

let test_diff_with_multiple_files () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "src/first";
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
              path = Path "src/second";
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

  let expected : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "src/first";
             mode = None;
             content =
               Text
                 {
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
           };
           {
             path = Path "src/second";
             mode = None;
             content =
               Text
                 {
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
           };
         ])
  in
  check optional_tui_model_testable "same TUI model" (Some expected) tui_model

let tui_model_of_diff_suite =
  [
    ("of changed file with text content", `Quick, test_changed_file_text_content);
    ("of changed file with binary content", `Quick, test_changed_file_binary_content);
    ("of renamed file", `Quick, test_changed_file_renamed);
    ("of renamed file with text content", `Quick, test_changed_file_renamed_with_text_content);
    (* It is not possible to have a renamed file with changed binary content. *)
    ("of empty created file", `Quick, test_empty_created_file);
    ("of created file with text content", `Quick, test_created_file_text_content);
    ("of created file with binary content", `Quick, test_created_file_binary_content);
    ("of empty deleted file", `Quick, test_empty_deleted_file);
    ("of deleted file with text content", `Quick, test_deleted_file_text_content);
    ("of deleted file with binary content", `Quick, test_deleted_file_binary_content);
    ("of multiple files", `Quick, test_diff_with_multiple_files);
  ]
