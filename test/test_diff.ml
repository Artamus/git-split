open Alcotest
open Git_split

let diff_testable = testable Diff.pp_diff Diff.equal_diff

let test_subtract_diff_from_itself () =
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
  let selected_diff : Diff.diff =
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

  let diff = Diff.subtract open_diff selected_diff in

  let expected : Diff.diff = { files = [] } in
  check diff_testable "same diffs" diff expected

let diff_suite =
  [
    ( "when subtracting a diff from itself, the result is an empty diff",
      `Quick,
      test_subtract_diff_from_itself );
  ]
