open Alcotest
open Git_split

let test_serializes_multiple_hunks () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "src/test";
              content =
                `Text
                  [
                    {
                      starting_line = 4;
                      context_snippet = Some "fun main() {";
                      lines =
                        [
                          `ContextLine "  hunk-1-unchanged-line";
                          `RemovedLine "  hunk-1-removed-line";
                          `AddedLine "  hunk-1-added-line";
                          `ContextLine "  hunk-1-unchanged-line";
                        ];
                    };
                    {
                      starting_line = 57;
                      context_snippet = None;
                      lines =
                        [
                          `ContextLine "  hunk-2-unchanged-line";
                          `AddedLine "  hunk-2-added-line";
                          `ContextLine "  hunk-2-unchanged-line";
                        ];
                    };
                  ];
            };
        ];
    }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/src/test b/src/test\n\
     --- a/src/test\n\
     +++ b/src/test\n\
     @@ -4,3 +4,3 @@ fun main() {\n\
    \   hunk-1-unchanged-line\n\
     -  hunk-1-removed-line\n\
     +  hunk-1-added-line\n\
    \   hunk-1-unchanged-line\n\
     @@ -57,2 +57,3 @@\n\
    \   hunk-2-unchanged-line\n\
     +  hunk-2-added-line\n\
    \   hunk-2-unchanged-line"
  in
  check string "same git diffs" expected git_diff

let test_serializes_multiple_hunks_with_asymmetric_change_counts () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "src/main";
              content =
                `Text
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
                        ];
                    };
                    {
                      starting_line = 8;
                      context_snippet = None;
                      lines =
                        [ `ContextLine "context"; `AddedLine "added-line"; `ContextLine "context" ];
                    };
                    {
                      starting_line = 15;
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
     @@ -1,4 +1,2 @@\n\
    \ context\n\
     -removed-line\n\
     -removed-line-2\n\
    \ context\n\
     @@ -8,2 +6,3 @@\n\
    \ context\n\
     +added-line\n\
    \ context\n\
     @@ -15,3 +14,3 @@\n\
    \ context\n\
     -removed-line\n\
     +added-line\n\
    \ context"
  in
  check string "same git diffs" expected git_diff

let test_serializes_rename () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            { path = ChangedPath { src = "src/old"; dst = "src/new" }; content = `Text [] };
        ];
    }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/src/old b/src/new\nsimilarity index 100%\nrename from src/old\nrename to src/new"
  in
  check string "same git diffs" expected git_diff

let test_serializes_rename_with_changes () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = ChangedPath { src = "src/old"; dst = "src/new" };
              content =
                `Text
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

let test_serializes_empty_created () =
  let diff : Diff.diff = { files = [ CreatedFile { path = "src/main"; content = `Text [] } ] } in

  let git_diff = DiffSerializer.serialize diff in

  let expected = "diff --git a/src/main b/src/main\nnew file mode 100644" in
  check string "same git diffs" expected git_diff

let test_serializes_created () =
  let diff : Diff.diff =
    {
      files =
        [
          CreatedFile
            {
              path = "src/main";
              content = `Text [ `AddedLine "added-line-1"; `AddedLine "added-line-2" ];
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
     @@ -0,0 +1,2 @@\n\
     +added-line-1\n\
     +added-line-2"
  in
  check string "same git diffs" expected git_diff

let test_serializes_empty_deleted () =
  let diff : Diff.diff = { files = [ DeletedFile { path = "src/main"; content = `Text [] } ] } in

  let git_diff = DiffSerializer.serialize diff in

  let expected = "diff --git a/src/main b/src/main\ndeleted file mode 100644" in
  check string "same git diffs" expected git_diff

let test_serializes_deleted () =
  let diff : Diff.diff =
    {
      files =
        [
          DeletedFile
            {
              path = "src/main";
              content = `Text [ `RemovedLine "removed-line-1"; `RemovedLine "removed-line-2" ];
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
     @@ -1,2 +0,0 @@\n\
     -removed-line-1\n\
     -removed-line-2"
  in
  check string "same git diffs" expected git_diff

let test_serializes_binary () =
  let diff : Diff.diff =
    {
      files =
        [ ChangedFile { path = Path "src/main"; content = `Binary "jusdt\nsome\ntest\ncontent" } ];
    }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected = "diff --git a/src/main b/src/main\nGIT binary patch\njusdt\nsome\ntest\ncontent" in
  check string "same git diffs" expected git_diff

let test_serializes_multiple_files () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "src/first";
              content =
                `Text
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
              path = Path "src/second";
              content =
                `Text
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
    ("containing a changed file with multiple hunks", `Quick, test_serializes_multiple_hunks);
    ( "containing multiple hunks with asymmetric number of changes per hunk",
      `Quick,
      test_serializes_multiple_hunks_with_asymmetric_change_counts );
    ("containing a simply renamed file", `Quick, test_serializes_rename);
    ("containing a renamed file with changes", `Quick, test_serializes_rename_with_changes);
    ("containing an empty created file", `Quick, test_serializes_empty_created);
    ("containing a non-empty created file", `Quick, test_serializes_created);
    ("containing an empty deleted file", `Quick, test_serializes_empty_deleted);
    ("containing a non-empty deleted file", `Quick, test_serializes_deleted);
    ("containing a changed file with binary content", `Quick, test_serializes_binary);
    ("containing multiple files", `Quick, test_serializes_multiple_files);
  ]
