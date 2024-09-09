let () =
  Alcotest.run "git-split"
    [
      ("DiffParser.parse diff", Test_diffParser.diff_parser_suite);
      ("DiffSerializer.serialize diff", Test_diffSerializer.diff_serializer_suite);
      ("Tui.model_of_diff", Test_tuiModelOfDiff.tui_model_of_diff_suite);
      ("Tui.diff_of_model", Test_diffOfTuiModel.diff_of_tui_model_suite);
      ("TuiModel.prev", Test_tuiModel_prev.prev_suite);
      ("TuiModel.next", Test_tuiModel_next.next_suite);
      ("TuiModel.up", Test_tuiModel_up.up_suite);
      ("TuiModel.collapse", Test_tuiModel_collapse.collapse_suite);
      ("TuiModel.expand", Test_tuiModel_expand.expand_suite);
      ("TuiModel.toggle_inclusion", Test_tuiModel_toggleInclusion.toggle_inclusion_suite);
    ]
