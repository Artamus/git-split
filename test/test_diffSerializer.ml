open Alcotest
open Git_split

let test_serializes_single_hunk_with_context () =
  let diff : Diff.diff =
    {
      files =
        [
          DiffFile
            {
              path = "src/main";
              hunks =
                [
                  {
                    first_line_idx = 1;
                    lines =
                      [
                        Diff.UnchangedLine "context";
                        Diff.RemovedLine "removed-line";
                        Diff.AddedLine "added-line";
                        Diff.UnchangedLine "context";
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

let test_serializes_single_hunk_starting_with_removed_line () =
  let diff : Diff.diff =
    {
      files =
        [
          DiffFile
            {
              path = "src/main";
              hunks =
                [
                  {
                    first_line_idx = 1;
                    lines =
                      [
                        Diff.RemovedLine "removed-line";
                        Diff.RemovedLine "removed-line-2";
                        Diff.UnchangedLine "context";
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
          DiffFile
            {
              path = "src/main";
              hunks =
                [
                  {
                    first_line_idx = 1;
                    lines =
                      [
                        Diff.AddedLine "added-line";
                        Diff.AddedLine "added-line-2";
                        Diff.UnchangedLine "context";
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
          DiffFile
            {
              path = "src/main";
              hunks =
                [
                  {
                    first_line_idx = 1;
                    lines =
                      [
                        Diff.UnchangedLine "context";
                        Diff.UnchangedLine "context";
                        Diff.RemovedLine "removed-line";
                        Diff.RemovedLine "removed-line-2";
                        Diff.AddedLine "added-line";
                        Diff.UnchangedLine "context";
                        Diff.UnchangedLine "context";
                        Diff.RemovedLine "removed-line";
                        Diff.AddedLine "added-line";
                        Diff.AddedLine "added-line-2";
                        Diff.UnchangedLine "context";
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
          DiffFile
            {
              path = "src/main";
              hunks =
                [
                  {
                    first_line_idx = 1;
                    lines =
                      [
                        Diff.UnchangedLine "context";
                        Diff.RemovedLine "removed-line";
                        Diff.RemovedLine "removed-line-2";
                        Diff.UnchangedLine "context";
                        Diff.UnchangedLine "context";
                        Diff.UnchangedLine "context";
                      ];
                  };
                  {
                    first_line_idx = 8;
                    lines =
                      [
                        Diff.UnchangedLine "context";
                        Diff.UnchangedLine "context";
                        Diff.UnchangedLine "context";
                        Diff.AddedLine "added-line";
                        Diff.UnchangedLine "context";
                        Diff.UnchangedLine "context";
                        Diff.UnchangedLine "context";
                      ];
                  };
                  {
                    first_line_idx = 15;
                    lines =
                      [
                        Diff.UnchangedLine "context";
                        Diff.UnchangedLine "context";
                        Diff.UnchangedLine "context";
                        Diff.RemovedLine "removed-line";
                        Diff.AddedLine "added-line";
                        Diff.UnchangedLine "context";
                        Diff.UnchangedLine "context";
                        Diff.UnchangedLine "context";
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

let test_serializes_removed_file () =
  let diff : Diff.diff =
    {
      files =
        [
          DiffFile
            {
              path = "src/main";
              hunks =
                [
                  {
                    first_line_idx = 1;
                    lines =
                      [
                        Diff.RemovedLine "removed-line-1";
                        Diff.RemovedLine "removed-line-2";
                        Diff.RemovedLine "removed-line-3";
                        Diff.RemovedLine "removed-line-4";
                        Diff.RemovedLine "removed-line-5";
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

let test_serializes_added_file () =
  let diff : Diff.diff =
    {
      files =
        [
          DiffFile
            {
              path = "src/main";
              hunks =
                [
                  {
                    first_line_idx = 1;
                    lines =
                      [
                        Diff.AddedLine "added-line-1";
                        Diff.AddedLine "added-line-2";
                        Diff.AddedLine "added-line-3";
                        Diff.AddedLine "added-line-4";
                        Diff.AddedLine "added-line-5";
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

(* TODO: Multiple files. *)
(* TODO: Renamed files. *)

let diff_serializer_suite =
  [
    ( "serializes a hunk that starts with a context line",
      `Quick,
      test_serializes_single_hunk_with_context );
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
    ("serializes a removed file", `Quick, test_serializes_removed_file);
    ("serializes an added file", `Quick, test_serializes_added_file);
  ]
