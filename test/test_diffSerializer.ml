let test_serializes_single_hunk_starting_with_context_line () =
let test_serializes_single_hunk_starting_with_removed_line () =
                    context_snippet = None;
                        `RemovedLine "removed-line-2";
     @@ -1,3 +1 @@\n\
     -removed-line-2\n\
let test_serializes_single_hunk_starting_with_added_line () =
                      [ `AddedLine "added-line"; `AddedLine "added-line-2"; `ContextLine "context" ];
     @@ -1 +1,3 @@\n\
     +added-line\n\
     +added-line-2\n\
let test_serializes_single_hunk_with_snippet () =
                    context_snippet = Some "context";
                      [
                        `ContextLine "context";
                        `RemovedLine "removed-line";
                        `AddedLine "added-line";
                        `ContextLine "context";
                      ];
     @@ -1,3 +1,3 @@ context\n\
    \ context\n\
     -removed-line\n\
let test_serializes_renamed_file_no_changes () =
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