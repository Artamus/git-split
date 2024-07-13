let () =
  Alcotest.run "git-split"
    [
      ("Parses git diff", Test_diffParser.diff_parser_suite);
      ("Serializes Diff", Test_diffSerializer.diff_serializer_suite);
      ("Converts Diff to TUI model", Test_tuiModelOfDiff.tui_model_of_diff_suite);
    ]
