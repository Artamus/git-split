let () = Alcotest.run "git-split" [ ("Diff parsing", Test_diffParser.diff_parser_suite) ]
