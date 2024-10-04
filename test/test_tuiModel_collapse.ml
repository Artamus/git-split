open Alcotest
open Git_split

let tui_model_testable = testable TuiModel.pp_model TuiModel.equal_model

(* These tests define the raw model types in variables so that they are easily reusable in the zippers.
   This is necessary because the way the zippers are used creates data duplication and this prevents mismatches between the raw data and the one in zippers. *)

let test_file () =
  let file : TuiTypes.file =
    {
      path = Path "src/main";
      kind = ChangedFile;
      mode = None;
      content =
        Text
          {
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
          };
    }
  in
  let model : TuiModel.model = TuiModel.File (Zipper.Zip ([], file, [])) in

  let collapse_model = TuiModel.collapse model in

  let expected_file =
    {
      file with
      content =
        Text
          {
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
          };
    }
  in
  let expected = TuiModel.File (Zipper.Zip ([], expected_file, [])) in
  check tui_model_testable "same TUI models" expected collapse_model

let test_hunk () =
  let line : TuiTypes.line = Diff ("code", `added, `included) in
  let hunk : TuiTypes.hunk =
    { starting_line = 1; context_snippet = None; visibility = Expanded; lines = [ line ] }
  in
  let model : TuiModel.model =
    TuiModel.Hunk
      ( Zipper.Zip
          ( [],
            {
              path = Path "src/main";
              kind = ChangedFile;
              mode = None;
              content = Text { visibility = Expanded; hunks = [ hunk ] };
            },
            [] ),
        Zipper.Zip ([], hunk, []) )
  in

  let collapse_model = TuiModel.collapse model in

  let expected_hunk : TuiTypes.hunk = { hunk with visibility = Collapsed } in
  let expected =
    TuiModel.Hunk
      ( Zipper.Zip
          ( [],
            {
              path = Path "src/main";
              kind = ChangedFile;
              mode = None;
              content = Text { visibility = Expanded; hunks = [ expected_hunk ] };
            },
            [] ),
        Zipper.Zip ([], expected_hunk, []) )
  in
  check tui_model_testable "same TUI models" expected collapse_model

let test_line_noop () =
  let line : TuiTypes.line = Diff ("code", `added, `included) in
  let hunk : TuiTypes.hunk =
    { starting_line = 1; context_snippet = None; visibility = Expanded; lines = [ line ] }
  in
  let model : TuiModel.model =
    TuiModel.Line
      ( Zipper.Zip
          ( [],
            {
              path = Path "src/main";
              kind = ChangedFile;
              mode = None;
              content = Text { visibility = Expanded; hunks = [ hunk ] };
            },
            [] ),
        Zipper.Zip ([], hunk, []),
        LineZipper.Zip ([], line, []) )
  in

  let collapse_model = TuiModel.collapse model in

  check tui_model_testable "same TUI models" model collapse_model

let collapse_suite =
  [
    ("collapses file", `Quick, test_file);
    ("collapses hunk", `Quick, test_hunk);
    ("doesn't collapse line", `Quick, test_line_noop);
  ]
