open Alcotest
open Git_split.DiffParser

let diff_testable = testable pp_diff equal_diff

let test_simple_diff () =
  let raw_diff =
    "diff --git a/bin/dune b/bin/dune\n\
     index 3e8a4cb..fc8e91d 100644\n\
     --- a/bin/dune\n\
     +++ b/bin/dune\n\
     @@ -1,4 +1,4 @@\n\
    \ (executable\n\
    \  (public_name git_split)\n\
    \  (name main)\n\
     - (libraries git_split))\n\
     + (libraries git_split feather re))"
  in

  let diff = Git_split.DiffParser.parse_diff raw_diff in

  let expected : diff =
    {
      files =
        [
          DiffFile
            {
              path = "bin/dune";
              hunks =
                [
                  {
                    lines =
                      [
                        UnchangedLine "(executable";
                        UnchangedLine " (public_name git_split)";
                        UnchangedLine " (name main)";
                        RemovedLine " (libraries git_split))";
                        AddedLine " (libraries git_split feather re))";
                      ];
                  };
                ];
            };
        ];
    }
  in
  check diff_testable "same diffs" diff expected

let test_diff_with_multiple_hunks () =
  let raw_diff =
    "diff --git a/src/test b/src/test\n\
     index 5ca75dc..b20f9ac 100644\n\
     --- a/src/test\n\
     +++ b/src/test\n\
     @@ -4,8 +4,7 @@ fun main() {\n\
    \   hunk-1-unchanged-line\n\
     -  hunk-1-removed-line\n\
     -  hunk-1-removed-line\n\
     +  hunk-1-added-line\n\
    \   hunk-1-unchanged-line\n\
     @@ -57,5 +56,6 @@ fun main() {\n\
    \   hunk-2-unchanged-line\n\
     +  hunk-2-added-line\n\
    \   hunk-2-unchanged-line"
  in

  let diff = Git_split.DiffParser.parse_diff raw_diff in

  let expected : diff =
    {
      files =
        [
          DiffFile
            {
              path = "src/test";
              hunks =
                [
                  {
                    lines =
                      [
                        UnchangedLine "  hunk-1-unchanged-line";
                        RemovedLine "  hunk-1-removed-line";
                        RemovedLine "  hunk-1-removed-line";
                        AddedLine "  hunk-1-added-line";
                        UnchangedLine "  hunk-1-unchanged-line";
                      ];
                  };
                  {
                    lines =
                      [
                        UnchangedLine "  hunk-2-unchanged-line";
                        AddedLine "  hunk-2-added-line";
                        UnchangedLine "  hunk-2-unchanged-line";
                      ];
                  };
                ];
            };
        ];
    }
  in
  check diff_testable "same diffs" diff expected

let test_diff_with_separate_changes_in_same_hunk () =
  let raw_diff =
    "diff --git a/lib/tui.ml b/lib/tui.ml\n\
     index 0441c31..02b5753 100644\n\
     --- a/lib/tui.ml\n\
     +++ b/lib/tui.ml\n\
     @@ -1,14 +1,16 @@\n\
    \ open Minttea\n\n\
     -let cursor = Spices.(default |> reverse true |> build)\n\
     +let cursor_style = Spices.(default |> reverse true |> build)\n\
    \ let header = Spices.(default |> reverse true |> build)\n\n\
     +type visibility = Expanded | Collapsed\n\
     +\n\
    \ type line =\n\
    \   | Context of string\n\
    \   | Diff of string * [ `added | `removed ] * [ `included | `notincluded ]\n\n\
     -type hunk = { lines : line list; visibility : [ `expanded | `collapsed ] }\n\
     -type file = { path : string; hunks : hunk list }\n\
     +type hunk = { lines : line list; lines_visibility : visibility }\n\
     +type file = { path : string; hunks : hunk list; hunks_visibility : visibility }\n\
    \ type cursor = FileCursor of int | HunkCursor of int * int | LineCursor of int * int * int\n\
    \ type model = { files : file list; cursor : cursor }\n\
    \ type set_lines_inclusion = AllLines | SomeLines | NoLines"
  in

  let diff = Git_split.DiffParser.parse_diff raw_diff in

  let expected : diff =
    {
      files =
        [
          DiffFile
            {
              path = "lib/tui.ml";
              hunks =
                [
                  {
                    lines =
                      [
                        UnchangedLine "open Minttea";
                        RemovedLine "let cursor = Spices.(default |> reverse true |> build)";
                        AddedLine "let cursor_style = Spices.(default |> reverse true |> build)";
                        UnchangedLine "let header = Spices.(default |> reverse true |> build)";
                        AddedLine "type visibility = Expanded | Collapsed";
                        AddedLine "";
                        UnchangedLine "type line =";
                        UnchangedLine "  | Context of string";
                        UnchangedLine
                          "  | Diff of string * [ `added | `removed ] * [ `included | `notincluded \
                           ]";
                        RemovedLine
                          "type hunk = { lines : line list; visibility : [ `expanded | `collapsed \
                           ] }";
                        RemovedLine "type file = { path : string; hunks : hunk list }";
                        AddedLine "type hunk = { lines : line list; lines_visibility : visibility }";
                        AddedLine
                          "type file = { path : string; hunks : hunk list; hunks_visibility : \
                           visibility }";
                        UnchangedLine
                          "type cursor = FileCursor of int | HunkCursor of int * int | LineCursor \
                           of int * int * int";
                        UnchangedLine "type model = { files : file list; cursor : cursor }";
                        UnchangedLine "type set_lines_inclusion = AllLines | SomeLines | NoLines";
                      ];
                  };
                ];
            };
        ];
    }
  in
  check diff_testable "same diffs" diff expected

let test_diff_with_multiple_files () =
  let raw_diff =
    "diff --git a/src/test b/src/test\n\
     index 25531f2..57f2dfb 100644\n\
     --- a/src/test\n\
     +++ b/src/test\n\
     @@ -1,10 +1,19 @@\n\
     -removed-line\n\
     -removed-line\n\
     +added-line\n\
     +added-line\n\
     diff --git a/src/file b/src/file\n\
     new file mode 100644\n\
     index 0000000..47d9444\n\
     --- /dev/null\n\
     +++ b/src/file\n\
     @@ -0,0 +1,2 @@\n\
     +added-line\n\
     + added-line"
  in

  let diff = Git_split.DiffParser.parse_diff raw_diff in

  let expected : diff =
    {
      files =
        [
          DiffFile
            {
              path = "src/test";
              hunks =
                [
                  {
                    lines =
                      [
                        RemovedLine "removed-line";
                        RemovedLine "removed-line";
                        AddedLine "added-line";
                        AddedLine "added-line";
                      ];
                  };
                ];
            };
          DiffFile
            {
              path = "src/file";
              hunks = [ { lines = [ AddedLine "added-line"; AddedLine " added-line" ] } ];
            };
        ];
    }
  in
  check diff_testable "same diffs" diff expected

let test_diff_with_renamed_file () =
  let raw_diff =
    "diff --git a/lib/minttea_tui.ml b/lib/mintteaTui.ml\n\
     similarity index 100%\n\
     rename from lib/minttea_tui.ml\n\
     rename to lib/mintteaTui.ml"
  in

  let diff = Git_split.DiffParser.parse_diff raw_diff in

  let expected : diff =
    { files = [ RenamedFile { old_path = "lib/minttea_tui.ml"; new_path = "lib/mintteaTui.ml" } ] }
  in
  check diff_testable "same diffs" diff expected

let suite =
  [
    ("parses a simple diff", `Quick, test_simple_diff);
    ("parses a diff with multiple hunks in file", `Quick, test_diff_with_multiple_hunks);
    ( "parses a diff with separate non-adjacent changes in the same hunk",
      `Quick,
      test_diff_with_separate_changes_in_same_hunk );
    ("parses a diff with multiple files", `Quick, test_diff_with_multiple_files);
    ("parses a diff with a file rename", `Quick, test_diff_with_renamed_file);
  ]

let () = Alcotest.run "git-split" [ ("Diff parsing", suite) ]
