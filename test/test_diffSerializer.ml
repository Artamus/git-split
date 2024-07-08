open Alcotest
open Git_split

let test_serializes_single_hunk () =
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

  let expected = "" in
  check string "same diffs" git_diff expected

let diff_serializer_suite =
  [ ("parses a diff with a hunk of changes", `Quick, test_serializes_single_hunk) ]
