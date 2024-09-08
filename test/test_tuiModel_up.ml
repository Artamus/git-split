open Alcotest
open Git_split

let tui_model_testable = testable TuiModel.pp_model TuiModel.equal_model

let test_file_noop () =
  let file : TuiTypes.file =
    {
      path = FilePath "src/main";
      visibility = Collapsed;
      hunks =
        [
          {
            starting_line = 1;
            context_snippet = None;
            visibility = Collapsed;
            lines = [ Diff ("code", `added, `included) ];
          };
        ];
    }
  in
  let model : TuiModel.model = TuiModel.File (Zipper.Zip ([], file, [])) in

  let up_model = TuiModel.up model in

  check tui_model_testable "same TUI models" model up_model

let test_hunk_to_file () =
  let first_hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Collapsed;
      lines = [ Diff ("code", `added, `included) ];
    }
  in
  let second_hunk : TuiTypes.hunk =
    {
      starting_line = 15;
      context_snippet = None;
      visibility = Collapsed;
      lines = [ Diff ("code", `added, `included) ];
    }
  in
  let file : TuiTypes.file =
    { path = FilePath "src/main"; visibility = Collapsed; hunks = [ first_hunk; second_hunk ] }
  in
  let model : TuiModel.model =
    TuiModel.Hunk (Zipper.Zip ([], file, []), Zipper.Zip ([ first_hunk ], second_hunk, []))
  in

  let up_model = TuiModel.up model in

  let expected : TuiModel.model = TuiModel.File (Zipper.Zip ([], file, [])) in
  check tui_model_testable "same TUI models" expected up_model

let test_line_to_hunk () =
  let first_line : TuiTypes.line = Diff ("code", `added, `included) in
  let second_line : TuiTypes.line = Diff ("code2", `added, `included) in
  let hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Collapsed;
      lines = [ first_line; second_line ];
    }
  in
  let file : TuiTypes.file =
    { path = FilePath "src/main"; visibility = Collapsed; hunks = [ hunk ] }
  in
  let model : TuiModel.model =
    TuiModel.Line
      ( Zipper.Zip ([], file, []),
        Zipper.Zip ([], hunk, []),
        LineZipper.Zip ([ first_line ], second_line, []) )
  in

  let up_model = TuiModel.up model in

  let expected : TuiModel.model =
    TuiModel.Hunk (Zipper.Zip ([], file, []), Zipper.Zip ([], hunk, []))
  in
  check tui_model_testable "same TUI models" expected up_model

let up_suite =
  [
    ("keeps the cursor on the same file", `Quick, test_file_noop);
    ("moves the cursor from a hunk to its file", `Quick, test_hunk_to_file);
    ("moves the cursor from a line to its hunk", `Quick, test_line_to_hunk);
  ]
