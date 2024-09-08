open Alcotest
open Git_split

let tui_model_testable = testable TuiModel.pp_model TuiModel.equal_model

let test_file () =
  let model : TuiModel.model =
    TuiModel.File
      (Zipper.Zip
         ( [],
           {
             path = FilePath "src/main";
             visibility = Expanded;
             hunks =
               [
                 {
                   starting_line = 1;
                   context_snippet = None;
                   visibility = Expanded;
                   lines = [ Diff ("code", `added, `included) ];
                 };
               ];
           },
           [] ))
  in

  let up_model = TuiModel.collapse model in

  let expected =
    TuiModel.File
      (Zipper.Zip
         ( [],
           {
             path = FilePath "src/main";
             visibility = Collapsed;
             hunks =
               [
                 {
                   starting_line = 1;
                   context_snippet = None;
                   visibility = Expanded;
                   lines = [ Diff ("code", `added, `included) ];
                 };
               ];
           },
           [] ))
  in
  check tui_model_testable "same TUI models" expected up_model

let test_hunk () =
  let line : TuiTypes.line = Diff ("code", `added, `included) in
  let hunk : TuiTypes.hunk =
    { starting_line = 1; context_snippet = None; visibility = Expanded; lines = [ line ] }
  in
  let model : TuiModel.model =
    TuiModel.Hunk
      ( Zipper.Zip ([], { path = FilePath "src/main"; visibility = Expanded; hunks = [ hunk ] }, []),
        Zipper.Zip ([], hunk, []) )
  in

  let up_model = TuiModel.collapse model in

  let expected_hunk : TuiTypes.hunk =
    { starting_line = 1; context_snippet = None; visibility = Collapsed; lines = [ line ] }
  in
  let expected =
    TuiModel.Hunk
      ( Zipper.Zip
          ([], { path = FilePath "src/main"; visibility = Expanded; hunks = [ expected_hunk ] }, []),
        Zipper.Zip ([], expected_hunk, []) )
  in
  check tui_model_testable "same TUI models" expected up_model

let test_line_noop () =
  let line : TuiTypes.line = Diff ("code", `added, `included) in
  let hunk : TuiTypes.hunk =
    { starting_line = 1; context_snippet = None; visibility = Expanded; lines = [ line ] }
  in
  let model : TuiModel.model =
    TuiModel.Line
      ( Zipper.Zip ([], { path = FilePath "src/main"; visibility = Expanded; hunks = [ hunk ] }, []),
        Zipper.Zip ([], hunk, []),
        LineZipper.Zip ([], line, []) )
  in

  let up_model = TuiModel.collapse model in

  check tui_model_testable "same TUI models" model up_model

let collapse_suite =
  [
    ("collapses file", `Quick, test_file);
    ("collapses hunk", `Quick, test_hunk);
    ("doesn't collapse line", `Quick, test_line_noop);
  ]
