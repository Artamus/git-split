open Alcotest
open Git_split

let diff_testable = testable Diff.pp_diff Diff.equal_diff
let result_diff_testable = Alcotest.result diff_testable Alcotest.string

let test_parses_single_text_hunk () =
  let raw_diff =
    "diff --git a/src/test b/src/test\n\
     index 5ca75dc..b20f9ac 100644\n\
     --- a/src/test\n\
     +++ b/src/test\n\
     @@ -4,8 +4,7 @@\n\
    \   hunk-1-unchanged-line\n\
     -  hunk-1-removed-line\n\
     -  hunk-1-removed-line\n\
     +  hunk-1-added-line\n\
    \   hunk-1-unchanged-line"
  in

  let diff = DiffParser.parse raw_diff in

  let expected : (Diff.diff, string) result =
    Ok
      {
        files =
          [
            ChangedFile
              {
                path = Path "src/test";
                content =
                  `Text
                    [
                      {
                        starting_line = 4;
                        context_snippet = None;
                        lines =
                          [
                            `ContextLine "  hunk-1-unchanged-line";
                            `RemovedLine "  hunk-1-removed-line";
                            `RemovedLine "  hunk-1-removed-line";
                            `AddedLine "  hunk-1-added-line";
                            `ContextLine "  hunk-1-unchanged-line";
                          ];
                      };
                    ];
              };
          ];
      }
  in
  check result_diff_testable "same diffs" expected diff

let test_parses_multiple_text_hunks () =
  let raw_diff =
    "diff --git a/src/test b/src/test\n\
     index 5ca75dc..b20f9ac 100644\n\
     --- a/src/test\n\
     +++ b/src/test\n\
     @@ -4,8 +4,7 @@ fun main() {\n\
    \   hunk-1-unchanged-line\n\
     -  hunk-1-removed-line\n\
     -  hunk-1-removed-line\n\
     +  hunk-1-added-line\n\
    \   hunk-1-unchanged-line\n\
     @@ -57,5 +56,6 @@\n\
    \   hunk-2-unchanged-line\n\
     +  hunk-2-added-line\n\
    \   hunk-2-unchanged-line"
  in

  let diff = DiffParser.parse raw_diff in

  let expected : (Diff.diff, string) result =
    Ok
      {
        files =
          [
            ChangedFile
              {
                path = Path "src/test";
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
  check result_diff_testable "same diffs" expected diff

let test_parses_renamed_file () =
  let raw_diff =
    "diff --git a/src/old b/src/new\nsimilarity index 100%\nrename from src/old\nrename to src/new"
  in

  let diff = DiffParser.parse raw_diff in

  let expected : (Diff.diff, string) result =
    Ok
      {
        files =
          [
            ChangedFile
              { path = ChangedPath { src = "src/old"; dst = "src/new" }; content = `Text [] };
          ];
      }
  in
  check result_diff_testable "same diffs" expected diff

let test_parses_renamed_file_with_changes () =
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
    \ context"
  in

  let diff = DiffParser.parse raw_diff in

  let expected : (Diff.diff, string) result =
    Ok
      {
        files =
          [
            ChangedFile
              {
                path = ChangedPath { src = "src/old"; dst = "src/new" };
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
  check result_diff_testable "same diffs" expected diff

let test_fails_incomplete_binary () =
  let raw_diff =
    "diff --git a/test-font.ttf b/test-font.ttf\n\
     index 6df2b25..61e5303 100644\n\
     Binary files a/test-font.ttf and b/test-font.ttf differ"
  in

  let diff = DiffParser.parse raw_diff in

  let expected = Error "cannot parse diff of binary file without its content" in
  check result_diff_testable "same diffs" expected diff

let test_parses_changed_binary () =
  let raw_diff =
    "diff --git a/test-font.ttf b/test-font.ttf\n\
     index 6df2b253603094de7f39886aae03181c686e375b..61e5303325a1b4d196d3ba631ac4681b1fdfb7c9 100644\n\
     GIT binary patch\n\
     literal 94372\n\
     zcmcG$2Ur|O(g50F134|Qz%H=M!tTNnHZK{L3<7~bD4@g=B#;mwf+blwpa{yzN|r1+"
  in

  let diff = DiffParser.parse raw_diff in

  let expected : (Diff.diff, string) result =
    Ok
      {
        files =
          [
            ChangedFile
              {
                path = Path "test-font.ttf";
                content =
                  `Binary
                    "literal 94372\n\
                     zcmcG$2Ur|O(g50F134|Qz%H=M!tTNnHZK{L3<7~bD4@g=B#;mwf+blwpa{yzN|r1+";
              };
          ];
      }
  in
  check result_diff_testable "same diffs" expected diff

let test_parses_empty_deleted_file () =
  let raw_diff =
    "diff --git a/empty-new-file.md b/empty-new-file.md\n\
     deleted file mode 100644\n\
     index e69de29..0000000"
  in

  let diff = DiffParser.parse raw_diff in

  let expected : (Diff.diff, string) result =
    Ok { files = [ DeletedFile { path = "empty-new-file.md"; content = `Text [] } ] }
  in
  check result_diff_testable "same diffs" expected diff

let test_parses_deleted_file () =
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
     -removed-line-5"
  in

  let diff = DiffParser.parse raw_diff in

  let expected : (Diff.diff, string) result =
    Ok
      {
        files =
          [
            DeletedFile
              {
                path = "src/main";
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
  check result_diff_testable "same diffs" expected diff

let test_parses_empty_created_file () =
  let raw_diff =
    "diff --git a/empty-new-file.md b/empty-new-file.md\n\
     new file mode 100644\n\
     index 0000000..e69de29"
  in

  let diff = DiffParser.parse raw_diff in

  let expected : (Diff.diff, string) result =
    Ok { files = [ CreatedFile { path = "empty-new-file.md"; content = `Text [] } ] }
  in
  check result_diff_testable "same diffs" expected diff

let test_parses_created_file () =
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
     +added-line-5"
  in

  let diff = DiffParser.parse raw_diff in

  let expected : (Diff.diff, string) result =
    Ok
      {
        files =
          [
            CreatedFile
              {
                path = "src/main";
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
  check result_diff_testable "same diffs" expected diff

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
    \ context"
  in

  let diff = DiffParser.parse raw_diff in

  let expected : (Diff.diff, string) result =
    Ok
      {
        files =
          [
            ChangedFile
              {
                path = Path "src/first";
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
  check result_diff_testable "same diffs" expected diff

let diff_parser_suite =
  [
    ("with a single hunk", `Quick, test_parses_single_text_hunk);
    ("with multiple hunks", `Quick, test_parses_multiple_text_hunks);
    ("with a renamed file", `Quick, test_parses_renamed_file);
    ("with a renamed and changed file", `Quick, test_parses_renamed_file_with_changes);
    ("with missing binary content", `Quick, test_fails_incomplete_binary);
    ("with binary content", `Quick, test_parses_changed_binary);
    ("with an empty deleted file", `Quick, test_parses_empty_deleted_file);
    ("with a non-empty deleted file", `Quick, test_parses_deleted_file);
    ("with an empty created file", `Quick, test_parses_empty_created_file);
    ("with a created file", `Quick, test_parses_created_file);
    ("with changes in multiple files", `Quick, test_parses_diff_with_multiple_files);
  ]
