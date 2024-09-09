open Alcotest
open Git_split

let tui_model_testable = testable TuiModel.pp_model TuiModel.equal_model

(* These tests define the raw model types in variables so that they are easily reusable in the zippers.
   This is necessary because the way the zippers are used creates data duplication and this prevents mismatches between the raw data and the one in zippers. *)

let test_file () =
  let model : TuiModel.model =
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
                   visibility = Collapsed;
                   lines = [ Diff ("code", `added, `included) ];
                 };
               ];
           },
           [] ))
  in

  let expand_model = TuiModel.expand model in

  let expected =
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
  check tui_model_testable "same TUI models" expected expand_model

let test_hunk () =
  let hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Collapsed;
      lines = [ Diff ("code", `added, `included) ];
    }
  in
  let model : TuiModel.model =
    TuiModel.Hunk
      ( Zipper.Zip ([], { path = FilePath "src/main"; visibility = Expanded; hunks = [ hunk ] }, []),
        Zipper.Zip ([], hunk, []) )
  in

  let expand_model = TuiModel.expand model in

  let expected_hunk : TuiTypes.hunk = { hunk with visibility = Expanded } in
  let expected =
    TuiModel.Hunk
      ( Zipper.Zip
          ([], { path = FilePath "src/main"; visibility = Expanded; hunks = [ expected_hunk ] }, []),
        Zipper.Zip ([], expected_hunk, []) )
  in
  check tui_model_testable "same TUI models" expected expand_model

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

  let expand_model = TuiModel.expand model in

  check tui_model_testable "same TUI models" model expand_model

let expand_suite =
  [
    ("expands file", `Quick, test_file);
    ("expands hunk", `Quick, test_hunk);
    ("doesn't expand line", `Quick, test_line_noop);
  ]
