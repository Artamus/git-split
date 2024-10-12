open Alcotest
open Git_split
open Git_split.TuiTypes

let diff_testable = testable Diff.pp_diff Diff.equal_diff
let result_diff_testable = Alcotest.result diff_testable Alcotest.string

let test_changed_file_text_content () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "src/test";
             kind = ChangedFile;
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

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
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
  check result_diff_testable "same diffs" (Ok expected) diff

let test_changed_file_binary_content () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "test-font.ttf";
             kind = ChangedFile;
             mode = None;
             content =
               Binary
                 ( "literal 94372\n\
                    zcmcG$2Ur|O(g50F134|Qz%H=M!tTNnHZK{L3<7~bD4@g=B#;mwf+blwpa{yzN|r1+",
                   `included );
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
  check result_diff_testable "same diffs" (Ok expected) diff

let test_changed_file_renamed () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = ChangedPath { old_path = "src/old"; new_path = "src/new" };
             kind = ChangedFile;
             mode = None;
             content = Text { visibility = Collapsed; hunks = [] };
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
  check result_diff_testable "same diffs" (Ok expected) diff

let test_changed_file_renamed_with_text_content () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = ChangedPath { old_path = "src/old"; new_path = "src/new" };
             kind = ChangedFile;
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
  check result_diff_testable "same diffs" (Ok expected) diff

let test_file_mode_change () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "script";
             kind = ChangedFile;
             mode = Some (ChangedMode { old_mode = 100644; new_mode = 100755 });
             content = Text { visibility = Collapsed; hunks = [] };
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
              path = Path "script";
              mode_change = Some { old_mode = 100644; new_mode = 100755 };
              content = `Text [];
            };
        ];
    }
  in
  check result_diff_testable "same diffs" (Ok expected) diff

let test_file_mode_change_with_text_content () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "script";
             kind = ChangedFile;
             mode = Some (ChangedMode { old_mode = 100755; new_mode = 100644 });
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
                             Context "hello";
                             Diff ("", `added, `included);
                             Diff ("asd", `added, `included);
                             Diff ("asd", `added, `included);
                           ];
                       };
                     ];
                 };
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
              path = Path "script";
              mode_change = Some { old_mode = 100755; new_mode = 100644 };
              content =
                `Text
                  [
                    {
                      starting_line = 1;
                      context_snippet = None;
                      lines =
                        [ `ContextLine "hello"; `AddedLine ""; `AddedLine "asd"; `AddedLine "asd" ];
                    };
                  ];
            };
        ];
    }
  in
  check result_diff_testable "same diffs" (Ok expected) diff

let test_file_mode_change_with_binary_content () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "test2.bin";
             kind = ChangedFile;
             mode = Some (ChangedMode { old_mode = 100755; new_mode = 100644 });
             content = Binary ("delta 6", `included);
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
              path = Path "test2.bin";
              mode_change = Some { old_mode = 100755; new_mode = 100644 };
              content = `Binary "delta 6";
            };
        ];
    }
  in
  check result_diff_testable "same diffs" (Ok expected) diff

let test_changed_file_renamed_with_mode_change () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = ChangedPath { old_path = "script"; new_path = "scriptt" };
             kind = ChangedFile;
             mode = Some (ChangedMode { old_mode = 100644; new_mode = 100755 });
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
                             Context "hello";
                             Diff ("", `removed, `included);
                             Diff ("e", `added, `included);
                           ];
                       };
                     ];
                 };
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
              path = ChangedPath { old_path = "script"; new_path = "scriptt" };
              mode_change = Some { old_mode = 100644; new_mode = 100755 };
              content =
                `Text
                  [
                    {
                      starting_line = 1;
                      context_snippet = None;
                      lines = [ `ContextLine "hello"; `RemovedLine ""; `AddedLine "e" ];
                    };
                  ];
            };
        ];
    }
  in
  check result_diff_testable "same diffs" (Ok expected) diff

let test_empty_created_file () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "empty-new-file.md";
             kind = CreatedFile;
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

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
    { files = [ CreatedFile { path = "empty-new-file.md"; mode = 100644; content = `Text [] } ] }
  in
  check result_diff_testable "same diffs" (Ok expected) diff

let test_created_file_text_content () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "src/main";
             kind = CreatedFile;
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

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
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
  check result_diff_testable "same diffs" (Ok expected) diff

let test_created_file_binary_content () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "foo.bin";
             kind = CreatedFile;
             mode = Some (Mode 100755);
             content = Binary ("literal 18", `included);
           };
         ])
  in

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
    { files = [ CreatedFile { path = "foo.bin"; mode = 100755; content = `Binary "literal 18" } ] }
  in
  check result_diff_testable "same diffs" (Ok expected) diff

let test_empty_deleted_file () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "empty-deleted-file.md";
             kind = DeletedFile;
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

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files = [ DeletedFile { path = "empty-deleted-file.md"; mode = 100644; content = `Text [] } ];
    }
  in
  check result_diff_testable "same diffs" (Ok expected) diff

let test_deleted_file_text_content () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "src/main";
             kind = DeletedFile;
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

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
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
  check result_diff_testable "same diffs" (Ok expected) diff

let test_deleted_file_binary_content () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "foo.bin";
             kind = DeletedFile;
             mode = Some (Mode 100755);
             content = Binary ("literal 0\nHcmV?d00001", `included);
           };
         ])
  in

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
    {
      files =
        [
          DeletedFile
            { path = "foo.bin"; mode = 100755; content = `Binary "literal 0\nHcmV?d00001" };
        ];
    }
  in
  check result_diff_testable "same diffs" (Ok expected) diff

let test_diff_with_multiple_files () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "src/first";
             kind = ChangedFile;
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
             kind = ChangedFile;
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

  let diff = Tui.diff_of_model tui_model in

  let expected : Diff.diff =
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
  check result_diff_testable "same diffs" (Ok expected) diff

let unselected_added_are_excluded () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "src/main";
             kind = ChangedFile;
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
                             Diff ("unselected-added-line", `added, `notincluded);
                             Context "context";
                           ];
                       };
                     ];
                 };
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
  check result_diff_testable "same diffs" (Ok expected) diff

let deleted_with_unselected_is_changed () =
  let tui_model : TuiModel.model =
    File
      (Zipper.from_list_exn
         [
           {
             path = Path "src/deleted";
             kind = DeletedFile;
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
                             Diff ("removed-line-1", `removed, `notincluded);
                             Diff ("removed-line-2", `removed, `notincluded);
                             Diff ("removed-line-3", `removed, `included);
                             Diff ("removed-line-4", `removed, `included);
                           ];
                       };
                     ];
                 };
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
  check result_diff_testable "same diffs" (Ok expected) diff

let diff_of_tui_model_suite =
  [
    (* Tests converting diffs as-is. *)
    ("of changed file with text content", `Quick, test_changed_file_text_content);
    ("of changed file with binary content", `Quick, test_changed_file_binary_content);
    ("of renamed file", `Quick, test_changed_file_renamed);
    ("of renamed file with text content", `Quick, test_changed_file_renamed_with_text_content);
    ("of changed file with mode change", `Quick, test_file_mode_change);
    ( "of changed file with mode change and text content",
      `Quick,
      test_file_mode_change_with_text_content );
    ( "of changed file with mode change and binary content",
      `Quick,
      test_file_mode_change_with_binary_content );
    ("of renamed file with mode change", `Quick, test_changed_file_renamed_with_mode_change);
    ("of empty created file", `Quick, test_empty_created_file);
    ("of created file with text content", `Quick, test_created_file_text_content);
    ("of created file with binary content", `Quick, test_created_file_binary_content);
    ("of empty deleted file", `Quick, test_empty_deleted_file);
    ("of deleted file with text content", `Quick, test_deleted_file_text_content);
    ("of deleted file with binary content", `Quick, test_deleted_file_binary_content);
    ("of multiple files", `Quick, test_diff_with_multiple_files);
    (* Tests documenting interesting behaviour. *)
    ("excludes unselected added lines from a created file", `Quick, unselected_added_are_excluded);
    ( "converts a deleted file with some unselected lines to a changed file",
      `Quick,
      deleted_with_unselected_is_changed );
  ]
