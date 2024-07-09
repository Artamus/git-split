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
  ]
