open Alcotest
open Git_split

let diff_testable = testable Diff.pp_diff Diff.equal_diff
let result_diff_testable = Alcotest.result diff_testable Alcotest.string

let test_parses_changed_file_text_content () =
  let raw_diff =
    "diff --git a/src/test b/src/test\n\
     index 5ca75dc..b20f9ac 100644\n\
     --- a/src/test\n\
     +++ b/src/test\n\
     @@ -4,4 +4,3 @@ fun main() {\n\
    \   hunk-1-unchanged-line\n\
     -  hunk-1-removed-line\n\
     -  hunk-1-removed-line\n\
     +  hunk-1-added-line\n\
    \   hunk-1-unchanged-line\n\
     @@ -57,2 +56,3 @@\n\
    \   hunk-2-unchanged-line\n\
     +  hunk-2-added-line\n\
    \   hunk-2-unchanged-line\n"
  in

  let diff = DiffParser.parse raw_diff in

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

let test_parses_changed_file_binary_content () =
  let raw_diff =
    "diff --git a/test-font.ttf b/test-font.ttf\n\
     index 6df2b253603094de7f39886aae03181c686e375b..61e5303325a1b4d196d3ba631ac4681b1fdfb7c9 100644\n\
     GIT binary patch\n\
     literal 12\n\
     zcmcG$2Ur|O(g50F134|Qz%H=M!tTNnHZK{L3<7~bD4@g=B#;mwf+blwpa{yzN|r1+\n\n\
     literal 14\n\
     asd\n\n"
  in

  let diff = DiffParser.parse raw_diff in

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
                  "literal 12\n\
                   zcmcG$2Ur|O(g50F134|Qz%H=M!tTNnHZK{L3<7~bD4@g=B#;mwf+blwpa{yzN|r1+\n\n\
                   literal 14\n\
                   asd\n\n";
            };
        ];
    }
  in
  check result_diff_testable "same diffs" (Ok expected) diff

let test_parses_changed_file_renamed () =
  let raw_diff =
    "diff --git a/src/old b/src/new\n\
     similarity index 100%\n\
     rename from src/old\n\
     rename to src/new\n"
  in

  let diff = DiffParser.parse raw_diff in

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

let test_parses_changed_file_renamed_with_text_content () =
  let raw_diff =
    "diff --git a/src/old b/src/new\n\
     similarity index 55%\n\
     rename from src/old\n\
     rename to src/new\n\
     --- a/src/old\n\
     +++ b/src/new\n\
     @@ -1,3 +1,3 @@\n\
    \ context\n\
     -removed-line\n\
     +added-line\n\
    \ context\n"
  in

  let diff = DiffParser.parse raw_diff in

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

let test_parses_file_mode_change () =
  let raw_diff = "diff --git a/script b/script\nold mode 100644\nnew mode 100755\n" in

  let diff = DiffParser.parse raw_diff in

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

let test_parses_file_mode_change_with_text_content () =
  let raw_diff =
    "diff --git a/script b/script\n\
     old mode 100755\n\
     new mode 100644\n\
     index ce01362..afa86b4\n\
     --- a/script\n\
     +++ b/script\n\
     @@ -1 +1,4 @@\n\
    \ hello\n\
     +\n\
     +asd\n\
     +asd\n"
  in

  let diff = DiffParser.parse raw_diff in

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

let test_parses_file_mode_change_with_binary_content () =
  let raw_diff =
    "diff --git a/test2.bin b/test2.bin\n\
     old mode 100755\n\
     new mode 100644\n\
     index 9c08d46d3abcb3153ff2787df59123bdfe2f741b..e0d0c9f63c81124656498c9a513aee5a6449ba61\n\
     GIT binary patch\n\
     literal 12\n\
     asd\n\n\
     literal 14\n\
     bla\n\n"
  in

  let diff = DiffParser.parse raw_diff in

  let expected : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "test2.bin";
              mode_change = Some { old_mode = 100755; new_mode = 100644 };
              content = `Binary "literal 12\nasd\n\nliteral 14\nbla\n\n";
            };
        ];
    }
  in
  check result_diff_testable "same diffs" (Ok expected) diff

let test_parses_changed_file_renamed_with_mode_change () =
  let raw_diff =
    "diff --git a/script b/scriptt\n\
     old mode 100644\n\
     new mode 100755\n\
     similarity index 87%\n\
     rename from script\n\
     rename to scriptt\n\
     index afa86b4..509c98e\n\
     --- a/script\n\
     +++ b/scriptt\n\
     @@ -1,2 +1,2 @@\n\
    \ hello\n\
     -\n\
     +e\n"
  in

  let diff = DiffParser.parse raw_diff in

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

let test_parses_empty_created_file () =
  let raw_diff =
    "diff --git a/empty-new-file.md b/empty-new-file.md\n\
     new file mode 100644\n\
     index 0000000..e69de29\n"
  in

  let diff = DiffParser.parse raw_diff in

  let expected : Diff.diff =
    { files = [ CreatedFile { path = "empty-new-file.md"; mode = 100644; content = `Text [] } ] }
  in
  check result_diff_testable "same diffs" (Ok expected) diff

let test_parses_created_file_text_content () =
  let raw_diff =
    "diff --git a/src/main b/src/main\n\
     new file mode 100644\n\
     --- /dev/null\n\
     +++ b/src/main\n\
     @@ -0,0 +1,5 @@\n\
     +added-line-1\n\
     +added-line-2\n\
     +added-line-3\n\
     +added-line-4\n\
     +added-line-5\n"
  in

  let diff = DiffParser.parse raw_diff in

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

let test_parses_created_file_binary_content () =
  let raw_diff =
    "diff --git a/foo.bin b/foo.bin\n\
     new file mode 100755\n\
     index 0000000000000000000000000000000000000000..fd519fea17d3cf58a92acf18cd81042397aa8fbf\n\
     GIT binary patch\n\
     literal 18\n\
     some content\n\n\
     literal 19\n\
     asd\n\n"
  in

  let diff = DiffParser.parse raw_diff in

  let expected : Diff.diff =
    {
      files =
        [
          CreatedFile
            {
              path = "foo.bin";
              mode = 100755;
              content = `Binary "literal 18\nsome content\n\nliteral 19\nasd\n\n";
            };
        ];
    }
  in
  check result_diff_testable "same diffs" (Ok expected) diff

let test_parses_empty_deleted_file () =
  let raw_diff =
    "diff --git a/empty-new-file.md b/empty-new-file.md\n\
     deleted file mode 100644\n\
     index e69de29..0000000\n"
  in

  let diff = DiffParser.parse raw_diff in

  let expected : Diff.diff =
    {
      files = [ Diff.DeletedFile { path = "empty-new-file.md"; mode = 100644; content = `Text [] } ];
    }
  in
  check result_diff_testable "same diffs" (Ok expected) diff

let test_parses_deleted_file_text_content () =
  let raw_diff =
    "diff --git a/src/main b/src/main\n\
     deleted file mode 100644\n\
     --- a/src/main\n\
     +++ /dev/null\n\
     @@ -1,5 +0,0 @@\n\
     -removed-line-1\n\
     -removed-line-2\n\
     -removed-line-3\n\
     -removed-line-4\n\
     -removed-line-5\n"
  in

  let diff = DiffParser.parse raw_diff in

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

let test_parses_deleted_file_binary_content () =
  let raw_diff =
    "diff --git a/foo.bin b/foo.bin\n\
     deleted file mode 100755\n\
     index fd519fea17d3cf58a92acf18cd81042397aa8fbf..0000000000000000000000000000000000000000\n\
     GIT binary patch\n\
     literal 0\n\
     HcmV?d00001\n\n\
     literal 10\n\
     foobarbaz\n\n"
  in

  let diff = DiffParser.parse raw_diff in

  let expected : Diff.diff =
    {
      files =
        [
          DeletedFile
            {
              path = "foo.bin";
              mode = 100755;
              content = `Binary "literal 0\nHcmV?d00001\n\nliteral 10\nfoobarbaz\n\n";
            };
        ];
    }
  in
  check result_diff_testable "same diffs" (Ok expected) diff

let test_parses_diff_with_multiple_files () =
  let raw_diff =
    "diff --git a/src/first b/src/first\n\
     --- a/src/first\n\
     +++ b/src/first\n\
     @@ -1,3 +1,3 @@\n\
    \ context\n\
     -removed-line\n\
     +added-line\n\
    \ context\n\
     diff --git a/src/second b/src/second\n\
     --- a/src/second\n\
     +++ b/src/second\n\
     @@ -1,3 +1,3 @@\n\
    \ context\n\
     -removed-line\n\
     +added-line\n\
    \ context\n"
  in

  let diff = DiffParser.parse raw_diff in

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

let test_fails_incomplete_binary () =
  let raw_diff =
    "diff --git a/test-font.ttf b/test-font.ttf\n\
     index 6df2b25..61e5303 100644\n\
     Binary files a/test-font.ttf and b/test-font.ttf differ\n"
  in

  let diff = DiffParser.parse raw_diff in

  let expected = Error "cannot parse diff of binary file without its content" in
  check result_diff_testable "same diffs" expected diff

let diff_parser_suite =
  [
    ("of changed file with text content", `Quick, test_parses_changed_file_text_content);
    ("of changed file with binary content", `Quick, test_parses_changed_file_binary_content);
    ("of renamed file", `Quick, test_parses_changed_file_renamed);
    ("of renamed file with text content", `Quick, test_parses_changed_file_renamed_with_text_content);
    (* It is not possible to have a renamed file with changed binary content. *)
    ("of changed file with mode change", `Quick, test_parses_file_mode_change);
    ( "of changed file with mode change and text content",
      `Quick,
      test_parses_file_mode_change_with_text_content );
    ( "of changed file with mode change and binary content",
      `Quick,
      test_parses_file_mode_change_with_binary_content );
    ("of renamed file with mode change", `Quick, test_parses_changed_file_renamed_with_mode_change);
    ("of empty created file", `Quick, test_parses_empty_created_file);
    ("of created file with text content", `Quick, test_parses_created_file_text_content);
    ("of created file with binary content", `Quick, test_parses_created_file_binary_content);
    ("of empty deleted file", `Quick, test_parses_empty_deleted_file);
    ("of deleted file with text content", `Quick, test_parses_deleted_file_text_content);
    ("of deleted file with binary content", `Quick, test_parses_deleted_file_binary_content);
    ("of multiple files", `Quick, test_parses_diff_with_multiple_files);
    ("of binary file without actual data", `Quick, test_fails_incomplete_binary);
  ]
