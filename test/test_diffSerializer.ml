open Alcotest
open Git_split

let test_serializes_single_hunk_with_context () =
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

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/src/main b/src/main\n\
     --- a/src/main\n\
     +++ b/src/main\n\
     @@ -1,3 +1,3 @@\n\
    \ context\n\
     -removed-line\n\
     +added-line\n\
    \ context"
  in
  check string "same git diffs" expected git_diff

let test_serializes_single_hunk_with_snippet () =
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

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/src/main b/src/main\n\
     --- a/src/main\n\
     +++ b/src/main\n\
     @@ -1,3 +1,3 @@ context\n\
    \ context\n\
     -removed-line\n\
     +added-line\n\
    \ context"
  in
  check string "same git diffs" expected git_diff

let test_serializes_single_hunk_starting_with_removed_line () =
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
                        `RemovedLine "removed-line";
                        `RemovedLine "removed-line-2";
                        `ContextLine "context";
                      ];
                  };
                ];
            };
        ];
    }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/src/main b/src/main\n\
     --- a/src/main\n\
     +++ b/src/main\n\
     @@ -1,3 +1 @@\n\
     -removed-line\n\
     -removed-line-2\n\
    \ context"
  in
  check string "same git diffs" expected git_diff

let test_serializes_single_hunk_starting_with_added_line () =
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
                      [ `AddedLine "added-line"; `AddedLine "added-line-2"; `ContextLine "context" ];
                  };
                ];
            };
        ];
    }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/src/main b/src/main\n\
     --- a/src/main\n\
     +++ b/src/main\n\
     @@ -1 +1,3 @@\n\
     +added-line\n\
     +added-line-2\n\
    \ context"
  in
  check string "same git diffs" expected git_diff

let test_serializes_single_hunk_with_nonconsecutive_changes () =
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

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/src/main b/src/main\n\
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
  check string "same git diffs" expected git_diff

let test_serializes_multiple_hunks_with_asymmetric_change_counts () =
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
                        `RemovedLine "removed-line-2";
                        `ContextLine "context";
                        `ContextLine "context";
                        `ContextLine "context";
                      ];
                  };
                  {
                    starting_line = 8;
                    context_snippet = None;
                    lines =
                      [
                        `ContextLine "context";
                        `ContextLine "context";
                        `ContextLine "context";
                        `AddedLine "added-line";
                        `ContextLine "context";
                        `ContextLine "context";
                        `ContextLine "context";
                      ];
                  };
                  {
                    starting_line = 15;
                    context_snippet = None;
                    lines =
                      [
                        `ContextLine "context";
                        `ContextLine "context";
                        `ContextLine "context";
                        `RemovedLine "removed-line";
                        `AddedLine "added-line";
                        `ContextLine "context";
                        `ContextLine "context";
                        `ContextLine "context";
                      ];
                  };
                ];
            };
        ];
    }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/src/main b/src/main\n\
     --- a/src/main\n\
     +++ b/src/main\n\
     @@ -1,6 +1,4 @@\n\
    \ context\n\
     -removed-line\n\
     -removed-line-2\n\
    \ context\n\
    \ context\n\
    \ context\n\
     @@ -8,6 +6,7 @@\n\
    \ context\n\
    \ context\n\
    \ context\n\
     +added-line\n\
    \ context\n\
    \ context\n\
    \ context\n\
     @@ -15,7 +14,7 @@\n\
    \ context\n\
    \ context\n\
    \ context\n\
     -removed-line\n\
     +added-line\n\
    \ context\n\
    \ context\n\
    \ context"
  in
  check string "same git diffs" expected git_diff

let test_serializes_deleted_file () =
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
                  `RemovedLine "removed-line-5";
                ];
            };
        ];
    }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected =
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
  check string "same git diffs" expected git_diff

let test_serializes_created_file () =
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
                  `AddedLine "added-line-5";
                ];
            };
        ];
    }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected =
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
  check string "same git diffs" expected git_diff

let test_serializes_renamed_file_no_changes () =
  let diff : Diff.diff =
    { files = [ RenamedFile { old_path = "src/old"; new_path = "src/new"; hunks = [] } ] }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/src/old b/src/new\nsimilarity index 100%\nrename from src/old\nrename to src/new"
  in
  check string "same git diffs" expected git_diff

let test_serializes_renamed_file_with_changes () =
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

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/src/old b/src/new\n\
     similarity index 100%\n\
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
  check string "same git diffs" expected git_diff

let test_serializes_multiple_files () =
  let diff : Diff.diff =
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

  let git_diff = DiffSerializer.serialize diff in

  let expected =
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
  check string "same git diffs" expected git_diff

let diff_serializer_suite =
  [
    ( "serializes a hunk that starts with a context line",
      `Quick,
      test_serializes_single_hunk_with_context );
    ( "serializes a hunk that has a hunk context snippet",
      `Quick,
      test_serializes_single_hunk_with_snippet );
    ( "serializes a hunk that starts with a removed line",
      `Quick,
      test_serializes_single_hunk_starting_with_removed_line );
    ( "serializes a hunk that starts with an added line",
      `Quick,
      test_serializes_single_hunk_starting_with_added_line );
    ( "serializes a hunk that contains nonconsecutive changes",
      `Quick,
      test_serializes_single_hunk_with_nonconsecutive_changes );
    ( "serializes multiple hunks with asymmetric number of changes per hunk",
      `Quick,
      test_serializes_multiple_hunks_with_asymmetric_change_counts );
    ("serializes a removed file", `Quick, test_serializes_deleted_file);
    ("serializes an added file", `Quick, test_serializes_created_file);
    ( "serializes a renamed file with no content changes",
      `Quick,
      test_serializes_renamed_file_no_changes );
    ( "serializes a renamed file with content changes",
      `Quick,
      test_serializes_renamed_file_with_changes );
    ("serializes multiple files", `Quick, test_serializes_multiple_files);
  ]
