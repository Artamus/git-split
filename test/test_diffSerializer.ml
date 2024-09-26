let test_serializes_changed_file_text_content () =
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
let test_serializes_changed_file_binary_content () =
              path = Path "test-font.ttf";
              mode_change = None;
              content =
                `Binary
                  "literal 94372\n\
                   zcmcG$2Ur|O(g50F134|Qz%H=M!tTNnHZK{L3<7~bD4@g=B#;mwf+blwpa{yzN|r1+";
    "diff --git a/test-font.ttf b/test-font.ttf\n\
     GIT binary patch\n\
     literal 94372\n\
     zcmcG$2Ur|O(g50F134|Qz%H=M!tTNnHZK{L3<7~bD4@g=B#;mwf+blwpa{yzN|r1+"
let test_serializes_changed_file_renamed () =
              path = ChangedPath { old_path = "src/old"; new_path = "src/new" };
              mode_change = None;
              content = `Text [];
    "diff --git a/src/old b/src/new\nsimilarity index 100%\nrename from src/old\nrename to src/new"
let test_serializes_changed_file_renamed_with_text_content () =
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
    "diff --git a/src/old b/src/new\n\
     similarity index 100%\n\
     rename from src/old\n\
     rename to src/new\n\
     --- a/src/old\n\
     +++ b/src/new\n\
     @@ -1,3 +1,3 @@\n\
let test_serializes_file_mode_change () =
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
let test_serializes_file_mode_change_with_binary_content () =
              path = Path "test2.bin";
              mode_change = Some { old_mode = 100755; new_mode = 100644 };
              content = `Binary "delta 6";
    "diff --git a/test2.bin b/test2.bin\n\
     old mode 100755\n\
     new mode 100644\n\
     GIT binary patch\n\
     delta 6"
let test_serializes_changed_file_renamed_with_mode_change () =
          ChangedFile
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

  let git_diff = DiffSerializer.serialize diff in

  let expected = "diff --git a/empty-new-file.md b/empty-new-file.md\nnew file mode 100644" in
let test_serializes_created_file_text_content () =
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
let test_serializes_created_file_binary_content () =
    { files = [ CreatedFile { path = "foo.bin"; mode = 100755; content = `Binary "literal 18" } ] }
    "diff --git a/foo.bin b/foo.bin\nnew file mode 100755\nGIT binary patch\nliteral 18"
  in
  check string "same git diffs" expected git_diff

let test_serializes_empty_deleted_file () =
  let diff : Diff.diff =
    { files = [ DeletedFile { path = "empty-new-file.md"; mode = 100644; content = `Text [] } ] }

  let git_diff = DiffSerializer.serialize diff in

  let expected = "diff --git a/empty-new-file.md b/empty-new-file.md\ndeleted file mode 100644" in
let test_serializes_deleted_file_text_content () =
          DeletedFile
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
let test_serializes_diff_with_multiple_files () =
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