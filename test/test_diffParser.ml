open Alcotest
open Git_split

let diff_testable = testable Diff.pp_diff Diff.equal_diff

let test_parses_single_hunk () =
  let raw_diff =
    "diff --git a/src/main b/src/main\n\
     index 3e8a4cb..fc8e91d 100644\n\
     --- a/src/main\n\
     +++ b/src/main\n\
     @@ -1,3 +1,3 @@\n\
    \ context\n\
     -removed-line\n\
     +added-line\n\
    \ context"
  in

  let diff = DiffParser.parse_diff raw_diff in

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

let test_parses_single_hunk_with_snippet () =
  let raw_diff =
    "diff --git a/src/main b/src/main\n\
     index 3e8a4cb..fc8e91d 100644\n\
     --- a/src/main\n\
     +++ b/src/main\n\
     @@ -5,3 +5,3 @@ context\n\
    \ context\n\
     -removed-line\n\
     +added-line\n\
    \ context"
  in

  let diff = DiffParser.parse_diff raw_diff in

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
                    starting_line = 5;
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

let test_parses_diff_with_multiple_hunks () =
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
     @@ -57,5 +56,6 @@ fun main() {\n\
    \   hunk-2-unchanged-line\n\
     +  hunk-2-added-line\n\
    \   hunk-2-unchanged-line"
  in

  let diff = DiffParser.parse_diff raw_diff in

  let expected : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = "src/test";
              hunks =
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
                    context_snippet = Some "fun main() {";
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
  check diff_testable "same diffs" expected diff

let test_parses_single_hunk_with_nonconsecutive_changes () =
  let raw_diff =
    "diff --git a/src/main b/src/main\n\
     index 3e8a4cb..fc8e91d 100644\n\
     --- a/src/main\n\
     +++ b/src/main\n\
     @@ -1,8 +1,8 @@\n\
    \ context\n\
    \ context\n\
     -removed-line\n\
     -removed-line-2\n\
     +added-line\n\
    \ context\n\
    \ context\n\
     -removed-line\n\
     +added-line\n\
     +added-line-2\n\
    \ context"
  in

  let diff = DiffParser.parse_diff raw_diff in

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
                        `ContextLine "context";
                        `RemovedLine "removed-line";
                        `RemovedLine "removed-line-2";
                        `AddedLine "added-line";
                        `ContextLine "context";
                        `ContextLine "context";
                        `RemovedLine "removed-line";
                        `AddedLine "added-line";
                        `AddedLine "added-line-2";
                        `ContextLine "context";
                      ];
                  };
                ];
            };
        ];
    }
  in
  check diff_testable "same diffs" expected diff

let test_parses_empty_deleted_file () =
  let raw_diff =
    "diff --git a/empty-new-file.md b/empty-new-file.md\n\
     deleted file mode 100644\n\
     index e69de29..0000000"
  in

  let diff = DiffParser.parse_diff raw_diff in

  let expected : Diff.diff =
    { files = [ DeletedFile { path = "empty-new-file.md"; lines = [] } ] }
  in
  check diff_testable "same diffs" expected diff

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

  let diff = DiffParser.parse_diff raw_diff in

  let expected : Diff.diff =
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
                  `RemovedLine "removed-line-5";
                ];
            };
        ];
    }
  in
  check diff_testable "same diffs" expected diff

let test_parses_empty_created_file () =
  let raw_diff =
    "diff --git a/empty-new-file.md b/empty-new-file.md\n\
     new file mode 100644\n\
     index 0000000..e69de29"
  in

  let diff = DiffParser.parse_diff raw_diff in

  let expected : Diff.diff =
    { files = [ CreatedFile { path = "empty-new-file.md"; lines = [] } ] }
  in
  check diff_testable "same diffs" expected diff

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

  let diff = DiffParser.parse_diff raw_diff in

  let expected : Diff.diff =
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
                  `AddedLine "added-line-5";
                ];
            };
        ];
    }
  in
  check diff_testable "same diffs" expected diff

let test_parses_renamed_file_no_changes () =
  let raw_diff =
    "diff --git a/src/old b/src/new\nsimilarity index 100%\nrename from src/old\nrename to src/new"
  in

  let diff = DiffParser.parse_diff raw_diff in

  let expected : Diff.diff =
    { files = [ RenamedFile { old_path = "src/old"; new_path = "src/new"; hunks = [] } ] }
  in
  check diff_testable "same diffs" expected diff

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

  let diff = DiffParser.parse_diff raw_diff in

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

  let diff = DiffParser.parse_diff raw_diff in

  let expected : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = "src/first";
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
              path = "src/second";
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

let diff_parser_suite =
  [
    ("containing a single hunk", `Quick, test_parses_single_hunk);
    ( "containing a single hunk that has a context snippet",
      `Quick,
      test_parses_single_hunk_with_snippet );
    ("containing multiple hunks", `Quick, test_parses_diff_with_multiple_hunks);
    ( "containing a hunk made up of nonconsecutive changes",
      `Quick,
      test_parses_single_hunk_with_nonconsecutive_changes );
    ("containing an empty deleted file", `Quick, test_parses_empty_deleted_file);
    ("containg a deleted file", `Quick, test_parses_deleted_file);
    ("containing an empty created file", `Quick, test_parses_empty_created_file);
    ("containing a created file", `Quick, test_parses_created_file);
    ("containing a file rename", `Quick, test_parses_renamed_file_no_changes);
    ("containing a file rename that also has changes", `Quick, test_parses_renamed_file_with_changes);
    ("containing changes for multiple files", `Quick, test_parses_diff_with_multiple_files);
  ]
