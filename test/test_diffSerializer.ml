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

  let expected =
    "diff --git a/src/main b/src/main\n\
     --- a/src/main\n\
     +++ b/src/main\n\
     @@ -0,4 +0,4 @@\n\
    \ context\n\
     -removed-line\n\
     +added-line\n\
    \ context"
  in
  check string "same git diffs" expected git_diff

let diff_serializer_suite =
  [ ("serializes a diff with a single hunk", `Quick, test_serializes_single_hunk) ]
