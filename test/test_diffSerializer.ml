open Alcotest
open Git_split

let test_serializes_changed_file_text_content () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "src/test";
              mode_change = None;
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
     @@ -4,4 +4,3 @@ fun main() {\n\
    \   hunk-1-unchanged-line\n\
     -  hunk-1-removed-line\n\
     -  hunk-1-removed-line\n\
     +  hunk-1-added-line\n\
    \   hunk-1-unchanged-line\n\
     @@ -57,2 +56,3 @@\n\
    \   hunk-2-unchanged-line\n\
     +  hunk-2-added-line\n\
    \   hunk-2-unchanged-line"
  in
  check string "same git diffs" expected git_diff

let test_serializes_changed_file_binary_content () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "test-font.ttf";
              mode_change = None;
              content =
                `Binary
                  "literal 94372\n\
                   zcmcG$2Ur|O(g50F134|Qz%H=M!tTNnHZK{L3<7~bD4@g=B#;mwf+blwpa{yzN|r1+";
            };
        ];
    }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/test-font.ttf b/test-font.ttf\n\
     GIT binary patch\n\
     literal 94372\n\
     zcmcG$2Ur|O(g50F134|Qz%H=M!tTNnHZK{L3<7~bD4@g=B#;mwf+blwpa{yzN|r1+"
  in
  check string "same git diffs" expected git_diff

let test_serializes_changed_file_renamed () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = ChangedPath { old_path = "src/old"; new_path = "src/new" };
              mode_change = None;
              content = `Text [];
            };
        ];
    }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/src/old b/src/new\nsimilarity index 100%\nrename from src/old\nrename to src/new"
  in
  check string "same git diffs" expected git_diff

let test_serializes_changed_file_renamed_with_text_content () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = ChangedPath { old_path = "src/old"; new_path = "src/new" };
              mode_change = None;
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

let test_serializes_file_mode_change () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "script";
              mode_change = Some { old_mode = 100644; new_mode = 100755 };
              content = `Text [];
            };
        ];
    }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected = "diff --git a/script b/script\nold mode 100644\nnew mode 100755" in
  check string "same git diffs" expected git_diff

let test_serializes_file_mode_change_with_text_content () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "script";
              mode_change = Some { old_mode = 100755; new_mode = 100644 };
              content =
                `Text
                  [
                    {
                      starting_line = 1;
                      context_snippet = None;
                      lines =
                        [ `ContextLine "hello"; `AddedLine ""; `AddedLine "asd"; `AddedLine "asd" ];
                    };
                  ];
            };
        ];
    }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/script b/script\n\
     old mode 100755\n\
     new mode 100644\n\
     --- a/script\n\
     +++ b/script\n\
     @@ -1 +1,4 @@\n\
    \ hello\n\
     +\n\
     +asd\n\
     +asd"
  in
  check string "same git diffs" expected git_diff

let test_serializes_file_mode_change_with_binary_content () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "test2.bin";
              mode_change = Some { old_mode = 100755; new_mode = 100644 };
              content = `Binary "delta 6";
            };
        ];
    }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/test2.bin b/test2.bin\n\
     old mode 100755\n\
     new mode 100644\n\
     GIT binary patch\n\
     delta 6"
  in
  check string "same git diffs" expected git_diff

let test_serializes_changed_file_renamed_with_mode_change () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = ChangedPath { old_path = "script"; new_path = "scriptt" };
              mode_change = Some { old_mode = 100644; new_mode = 100755 };
              content =
                `Text
                  [
                    {
                      starting_line = 1;
                      context_snippet = None;
                      lines = [ `ContextLine "hello"; `RemovedLine ""; `AddedLine "e" ];
                    };
                  ];
            };
        ];
    }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/script b/scriptt\n\
     old mode 100644\n\
     new mode 100755\n\
     similarity index 100%\n\
     rename from script\n\
     rename to scriptt\n\
     --- a/script\n\
     +++ b/scriptt\n\
     @@ -1,2 +1,2 @@\n\
    \ hello\n\
     -\n\
     +e"
  in
  check string "same git diffs" expected git_diff

let test_serializes_empty_created_file () =
  let diff : Diff.diff =
    { files = [ CreatedFile { path = "empty-new-file.md"; mode = 100644; content = `Text [] } ] }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected = "diff --git a/empty-new-file.md b/empty-new-file.md\nnew file mode 100644" in
  check string "same git diffs" expected git_diff

let test_serializes_created_file_text_content () =
  let diff : Diff.diff =
    {
      files =
        [
          CreatedFile
            {
              path = "src/main";
              mode = 100644;
              content =
                `Text
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

let test_serializes_created_file_binary_content () =
  let diff : Diff.diff =
    { files = [ CreatedFile { path = "foo.bin"; mode = 100755; content = `Binary "literal 18" } ] }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/foo.bin b/foo.bin\nnew file mode 100755\nGIT binary patch\nliteral 18"
  in
  check string "same git diffs" expected git_diff

let test_serializes_empty_deleted_file () =
  let diff : Diff.diff =
    { files = [ DeletedFile { path = "empty-new-file.md"; mode = 100644; content = `Text [] } ] }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected = "diff --git a/empty-new-file.md b/empty-new-file.md\ndeleted file mode 100644" in
  check string "same git diffs" expected git_diff

let test_serializes_deleted_file_text_content () =
  let diff : Diff.diff =
    {
      files =
        [
          DeletedFile
            {
              path = "src/main";
              mode = 100644;
              content =
                `Text
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

let test_serializes_deleted_file_binary_content () =
  let diff : Diff.diff =
    {
      files =
        [
          DeletedFile
            { path = "foo.bin"; mode = 100755; content = `Binary "literal 0\nHcmV?d00001" };
        ];
    }
  in

  let git_diff = DiffSerializer.serialize diff in

  let expected =
    "diff --git a/foo.bin b/foo.bin\n\
     deleted file mode 100755\n\
     GIT binary patch\n\
     literal 0\n\
     HcmV?d00001"
  in
  check string "same git diffs" expected git_diff

let test_serializes_diff_with_multiple_files () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "src/first";
              mode_change = None;
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
              mode_change = None;
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

(** The purpose of this test is to cover potential regressions to calculating the starting line on the added part, as we actually calculate this from the information we have. *)
let test_serializes_multiple_hunks_with_asymmetric_change_counts () =
  let diff : Diff.diff =
    {
      files =
        [
          ChangedFile
            {
              path = Path "src/main";
              mode_change = None;
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

let diff_serializer_suite =
  [
    ("of changed with text content", `Quick, test_serializes_changed_file_text_content);
    ("of changed file with binary content", `Quick, test_serializes_changed_file_binary_content);
    ("of renamed file", `Quick, test_serializes_changed_file_renamed);
    ( "of renamed file with text content",
      `Quick,
      test_serializes_changed_file_renamed_with_text_content );
    (* It is not possible to have a renamed file with changed binary content. *)
    ("of changed file with mode change", `Quick, test_serializes_file_mode_change);
    ( "of changed file with mode change and text content",
      `Quick,
      test_serializes_file_mode_change_with_text_content );
    ( "of changed file with mode change and binary content",
      `Quick,
      test_serializes_file_mode_change_with_binary_content );
    ( "of renamed file with mode change",
      `Quick,
      test_serializes_changed_file_renamed_with_mode_change );
    ("of empty created file", `Quick, test_serializes_empty_created_file);
    ("of created file with text content", `Quick, test_serializes_created_file_text_content);
    ("of created file with binary content", `Quick, test_serializes_created_file_binary_content);
    ("of empty deleted file", `Quick, test_serializes_empty_deleted_file);
    ("of deleted file with text content", `Quick, test_serializes_deleted_file_text_content);
    ("of deleted file with binary content", `Quick, test_serializes_deleted_file_binary_content);
    ("of multiple files", `Quick, test_serializes_diff_with_multiple_files);
    ( "of a changed file with asymmetric change counts between hunks",
      `Quick,
      test_serializes_multiple_hunks_with_asymmetric_change_counts );
  ]
