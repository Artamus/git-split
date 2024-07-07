open Alcotest
open Git_split

let diff_testable = testable Diff.pp_diff Diff.equal_diff

let test_whole_open_diff_selected () =
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
                  {
                    lines =
                      [
                        Diff.UnchangedLine "more-context";
                        Diff.RemovedLine "another-removed-line";
                        Diff.AddedLine "another-added-line";
                        Diff.UnchangedLine "more-context";
                      ];
                  };
                ];
            };
          RenamedFile
            {
              old_path = "src/main";
              new_path = "src/new-main";
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
                  {
                    lines =
                      [
                        Diff.UnchangedLine "more-context";
                        Diff.RemovedLine "another-removed-line";
                        Diff.AddedLine "another-added-line";
                        Diff.UnchangedLine "more-context";
                      ];
                  };
                ];
            };
          RenamedFile
            {
              old_path = "src/main";
              new_path = "src/new-main";
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
  [ ("subtracts the whole diff from the open diff", `Quick, test_whole_open_diff_selected) ]
