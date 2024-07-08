open Git_split
let diff_testable = testable Diff.pp_diff Diff.equal_diff
let test_single_hunk () =
  let diff = DiffParser.parse_diff raw_diff in
  let expected : Diff.diff =
  let diff = DiffParser.parse_diff raw_diff in
  let expected : Diff.diff =
let test_diff_with_multiple_files () =
  let raw_diff =
    "diff --git a/src/test b/src/test\n\
     index 25531f2..57f2dfb 100644\n\
     --- a/src/test\n\
     +++ b/src/test\n\
     @@ -1,10 +1,19 @@\n\
     -removed-line\n\
     +added-line\n\
     diff --git a/src/file b/src/file\n\
     new file mode 100644\n\
     index 0000000..47d9444\n\
     --- /dev/null\n\
     +++ b/src/file\n\
     @@ -0,0 +1,2 @@\n\
     -removed-line\n\
     + added-line"
  in

  let diff = DiffParser.parse_diff raw_diff in

  let expected : Diff.diff =
    {
      files =
        [
          DiffFile
            {
              path = "src/test";
              hunks = [ { lines = [ RemovedLine "removed-line"; AddedLine "added-line" ] } ];
            };
          DiffFile
            {
              path = "src/file";
              hunks = [ { lines = [ RemovedLine "removed-line"; AddedLine " added-line" ] } ];
            };
        ];
    }
  in
  check diff_testable "same diffs" diff expected

let test_diff_with_empty_added_file () =
  let raw_diff =
    "diff --git a/empty-new-file.md b/empty-new-file.md\n\
     new file mode 100644\n\
     index 0000000..e69de29"
  in

  let diff = DiffParser.parse_diff raw_diff in

  let expected : Diff.diff = { files = [ DiffFile { path = "empty-new-file.md"; hunks = [] } ] } in
  check diff_testable "same diffs" diff expected

let test_diff_with_added_file () =
  let raw_diff =
    "diff --git a/new-nonempty-file.md b/new-nonempty-file.md\n\
     new file mode 100644\n\
     index 0000000..a1df4ea\n\
     --- /dev/null\n\
     +++ b/new-nonempty-file.md\n\
     @@ -0,0 +1,2 @@\n\
     +A line\n\
     +Another line!"
  in

  let diff = DiffParser.parse_diff raw_diff in

  let expected : Diff.diff =
    {
      files =
        [
          DiffFile
            {
              path = "new-nonempty-file.md";
              hunks = [ { lines = [ AddedLine "A line"; AddedLine "Another line!" ] } ];
            };
        ];
    }
  in
  check diff_testable "same diffs" diff expected

let test_diff_with_empty_removed_file () =
  let raw_diff =
    "diff --git a/empty-new-file.md b/empty-new-file.md\n\
     deleted file mode 100644\n\
     index e69de29..0000000"
  in

  let diff = DiffParser.parse_diff raw_diff in

  let expected : Diff.diff = { files = [ DiffFile { path = "empty-new-file.md"; hunks = [] } ] } in
  check diff_testable "same diffs" diff expected

let test_diff_with_removed_file () =
  let raw_diff =
    "diff --git a/new-nonempty-file.md b/new-nonempty-file.md\n\
     deleted file mode 100644\n\
     index a1df4ea..0000000\n\
     --- a/new-nonempty-file.md\n\
     +++ /dev/null\n\
     @@ -1,2 +0,0 @@\n\
     -A line\n\
     -Another line!"
  in

  let diff = DiffParser.parse_diff raw_diff in

  let expected : Diff.diff =
    {
      files =
        [
          DiffFile
            {
              path = "new-nonempty-file.md";
              hunks = [ { lines = [ RemovedLine "A line"; RemovedLine "Another line!" ] } ];
            };
        ];
    }
  in
  check diff_testable "same diffs" diff expected

let test_diff_with_multiple_sets_of_changes_in_same_hunk () =
  let diff = DiffParser.parse_diff raw_diff in
  let expected : Diff.diff =
  let diff = DiffParser.parse_diff raw_diff in
  let expected : Diff.diff =
    {
      files =
        [
          RenamedFile
            { old_path = "lib/minttea_tui.ml"; new_path = "lib/mintteaTui.ml"; hunks = [] };
        ];
    }
  let diff = DiffParser.parse_diff raw_diff in
  let expected : Diff.diff =
    {
      files =
        [
          RenamedFile
            {
              old_path = "file.md";
              new_path = "file-super.md";
              hunks =
                [
                  {
                    lines =
                      [
                        UnchangedLine "line6";
                        UnchangedLine "line7";
                        UnchangedLine "line8";
                        RemovedLine "line9";
                        AddedLine "line91";
                        UnchangedLine "line10";
                        UnchangedLine "line11";
                        UnchangedLine "line12";
                      ];
                  };
                ];
            };
        ];
    }
let diff_parser_suite =
    ("parses a diff with a hunk of changes", `Quick, test_single_hunk);
    ("parses a diff with multiple files", `Quick, test_diff_with_multiple_files);
    ("parses a diff with an empty added file", `Quick, test_diff_with_empty_added_file);
    ("parses a diff with an added file with contents", `Quick, test_diff_with_added_file);
    ("parses a diff with an empty removed file", `Quick, test_diff_with_empty_removed_file);
    ("parses a diff with a removed file with contents", `Quick, test_diff_with_removed_file);
      test_diff_with_multiple_sets_of_changes_in_same_hunk );
    ("parses a diff with a renamed file", `Quick, test_diff_with_renamed_file);