let () =
  Alcotest.run "git-split"
    [
      ("Parses git diff", Test_diffParser.diff_parser_suite);
      ("Serializes Diff", Test_diffSerializer.diff_serializer_suite);
      ("Converts Diff to TUI model", Test_tuiModelOfDiff.tui_model_of_diff_suite);
      ("Converts TUI model to Diff", Test_diffOfTuiModel.diff_of_tui_model_suite);
      ("TuiModel.prev", Test_tuiModel_prev.prev_suite);
      ("TuiModel.next", Test_tuiModel_next.next_suite);
      ("TuiModel.up", Test_tuiModel_up.up_suite);
      ("TuiModel.collapse", Test_tuiModel_collapse.collapse_suite);
    ]
