open Alcotest
open Git_split

let diff_testable = testable Diff.pp_diff Diff.equal_diff

let test_subtract_commit_is_same_as_open () =
  let open_diff : Diff.diff =
    {
      files =
        [
          DiffFile
            {
              path = "src/file";
              hunks =
                [
                  {
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
  let commit_diff : Diff.diff =
    {
      files =
        [
          DiffFile
            {
              path = "src/file";
              hunks =
                [
                  {
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

  let result_open_diff = Diff.subtract ~open_diff ~commit_diff in

  let expected : Diff.diff = { files = [] } in
  check diff_testable "same diffs" expected result_open_diff

let test_subtract_commit_missing_removed () =
  let open_diff : Diff.diff =
    {
      files =
        [
          DiffFile
            {
              path = "src/file";
              hunks =
                [
                  {
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
  let commit_diff : Diff.diff =
    {
      files =
        [
          DiffFile
            {
              path = "src/file";
              hunks =
                [
                  {
                    lines =
                      [
                        Diff.UnchangedLine "context";
                        Diff.UnchangedLine "removed-line";
                        Diff.AddedLine "added-line";
                        Diff.UnchangedLine "context";
                      ];
                  };
                ];
            };
        ];
    }
  in

  let result_open_diff = Diff.subtract ~open_diff ~commit_diff in

  let expected : Diff.diff =
    {
      files =
        [
          DiffFile
            {
              path = "src/file";
              hunks =
                [
                  {
                    lines =
                      [
                        Diff.UnchangedLine "context";
                        Diff.RemovedLine "removed-line";
                        Diff.UnchangedLine "added-line";
                        Diff.UnchangedLine "context";
                      ];
                  };
                ];
            };
        ];
    }
  in
  check diff_testable "same diffs" expected result_open_diff

let test_subtract_commit_missing_added () =
  let open_diff : Diff.diff =
    {
      files =
        [
          DiffFile
            {
              path = "src/file";
              hunks =
                [
                  {
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
  let commit_diff : Diff.diff =
    {
      files =
        [
          DiffFile
            {
              path = "src/file";
              hunks =
                [
                  {
                    lines =
                      [
                        Diff.UnchangedLine "context";
                        Diff.RemovedLine "removed-line";
                        Diff.UnchangedLine "context";
                      ];
                  };
                ];
            };
        ];
    }
  in

  let result_open_diff = Diff.subtract ~open_diff ~commit_diff in

  let expected : Diff.diff =
    {
      files =
        [
          DiffFile
            {
              path = "src/file";
              hunks =
                [
                  {
                    lines =
                      [
                        Diff.UnchangedLine "context";
                        Diff.AddedLine "added-line";
                        Diff.UnchangedLine "context";
                      ];
                  };
                ];
            };
        ];
    }
  in
  check diff_testable "same diffs" expected result_open_diff

let diff_suite =
  [
    ( "when subtracting a diff from itself, the result is an empty diff",
      `Quick,
      test_subtract_commit_is_same_as_open );
    ( "when the commit doesn't contain the removed line, it stays in the open diff",
      `Quick,
      test_subtract_commit_missing_removed );
    ( "when the commit doesn't contain the added line, it stays in the open diff",
      `Quick,
      test_subtract_commit_missing_added );
  ]
