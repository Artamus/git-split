open Alcotest
open Git_split.Model

let diff_testable =
  (* Can also use Fmt.Dump to create pretty prints. *)
  let pp ppf data =
    let diff_str = show_diff data in
    Fmt.pf ppf "%s" diff_str
  in
  let equal a b = equal_diff a b in
  testable pp equal

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

let suite =
  [
    ("parses a simple diff", `Quick, test_simple_diff);
    ("parses a diff with multiple hunks in file", `Quick, test_diff_with_multiple_hunks);
  ]

let () = Alcotest.run "git-split" [ ("Diff parsing", suite) ]
