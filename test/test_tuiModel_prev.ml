open Alcotest
open Git_split

let tui_model_testable = testable TuiModel.pp_model TuiModel.equal_model

(* These tests define the raw model types in variables so that they are easily reusable in the zippers.
   This is necessary because the way the zippers are used creates data duplication and this prevents mismatches between the raw data and the one in zippers. *)

let test_to_last_file () =
  let first_file : TuiTypes.file =
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
  let second_file : TuiTypes.file =
    {
      path = FilePath "src/test";
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
  let model : TuiModel.model = TuiModel.File (Zipper.Zip ([], first_file, [ second_file ])) in

  let prev_model = TuiModel.prev model in

  let expected = TuiModel.File (Zipper.Zip ([ first_file ], second_file, [])) in
  check tui_model_testable "same TUI models" expected prev_model

let test_to_last_file_last_hunk () =
  let first_file : TuiTypes.file =
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
  let second_file_hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Collapsed;
      lines = [ Diff ("code", `added, `included) ];
    }
  in
  let second_file : TuiTypes.file =
    { path = FilePath "src/test"; visibility = Expanded; hunks = [ second_file_hunk ] }
  in
  let model : TuiModel.model = TuiModel.File (Zipper.Zip ([], first_file, [ second_file ])) in

  let prev_model = TuiModel.prev model in

  let expected =
    TuiModel.Hunk
      (Zipper.Zip ([ first_file ], second_file, []), Zipper.Zip ([], second_file_hunk, []))
  in
  check tui_model_testable "same TUI models" expected prev_model

let test_to_last_file_last_hunk_last_line () =
  let first_file : TuiTypes.file =
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
  let second_file_line : TuiTypes.line = Diff ("code", `added, `included) in
  let second_file_hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Expanded;
      lines = [ second_file_line ];
    }
  in
  let second_file : TuiTypes.file =
    { path = FilePath "src/test"; visibility = Expanded; hunks = [ second_file_hunk ] }
  in
  let model : TuiModel.model = TuiModel.File (Zipper.Zip ([], first_file, [ second_file ])) in

  let prev_model = TuiModel.prev model in

  let expected =
    TuiModel.Line
      ( Zipper.Zip ([ first_file ], second_file, []),
        Zipper.Zip ([], second_file_hunk, []),
        LineZipper.Zip ([], second_file_line, []) )
  in
  check tui_model_testable "same TUI models" expected prev_model

let test_hunk_to_file () =
  let hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Collapsed;
      lines = [ Diff ("code", `added, `included) ];
    }
  in
  let file : TuiTypes.file =
    { path = FilePath "src/main"; visibility = Collapsed; hunks = [ hunk ] }
  in
  let model : TuiModel.model =
    TuiModel.Hunk (Zipper.Zip ([], file, []), Zipper.Zip ([], hunk, []))
  in

  let prev_model = TuiModel.prev model in

  let expected = TuiModel.File (Zipper.Zip ([], file, [])) in
  check tui_model_testable "same TUI models" expected prev_model

let test_hunk_to_hunk () =
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
      lines = [ Diff ("code2", `added, `included) ];
    }
  in
  let file : TuiTypes.file =
    { path = FilePath "src/main"; visibility = Expanded; hunks = [ first_hunk; second_hunk ] }
  in
  let model : TuiModel.model =
    TuiModel.Hunk (Zipper.Zip ([], file, []), Zipper.Zip ([ first_hunk ], second_hunk, []))
  in

  let prev_model = TuiModel.prev model in

  let expected =
    TuiModel.Hunk (Zipper.Zip ([], file, []), Zipper.Zip ([], first_hunk, [ second_hunk ]))
  in
  check tui_model_testable "same TUI models" expected prev_model

let test_hunk_to_line () =
  let first_hunk_line : TuiTypes.line = Diff ("code", `added, `included) in
  let first_hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Expanded;
      lines = [ Diff ("code", `added, `included) ];
    }
  in
  let second_hunk : TuiTypes.hunk =
    {
      starting_line = 15;
      context_snippet = None;
      visibility = Collapsed;
      lines = [ Diff ("code2", `added, `included) ];
    }
  in
  let file : TuiTypes.file =
    { path = FilePath "src/main"; visibility = Expanded; hunks = [ first_hunk; second_hunk ] }
  in
  let model : TuiModel.model =
    TuiModel.Hunk (Zipper.Zip ([], file, []), Zipper.Zip ([ first_hunk ], second_hunk, []))
  in

  let prev_model = TuiModel.prev model in

  let expected =
    TuiModel.Line
      ( Zipper.Zip ([], file, []),
        Zipper.Zip ([], first_hunk, [ second_hunk ]),
        LineZipper.Zip ([], first_hunk_line, []) )
  in
  check tui_model_testable "same TUI models" expected prev_model

let test_line_to_hunk () =
  let line : TuiTypes.line = Diff ("code", `added, `included) in
  let hunk : TuiTypes.hunk =
    { starting_line = 1; context_snippet = None; visibility = Expanded; lines = [ line ] }
  in
  let file : TuiTypes.file =
    { path = FilePath "src/main"; visibility = Expanded; hunks = [ hunk ] }
  in
  let model : TuiModel.model =
    TuiModel.Line
      (Zipper.Zip ([], file, []), Zipper.Zip ([], hunk, []), LineZipper.Zip ([], line, []))
  in

  let prev_model = TuiModel.prev model in

  let expected = TuiModel.Hunk (Zipper.Zip ([], file, []), Zipper.Zip ([], hunk, [])) in
  check tui_model_testable "same TUI models" expected prev_model

let test_line_to_line () =
  let first_line : TuiTypes.line = Diff ("code", `added, `included) in
  let second_line : TuiTypes.line = Diff ("code2", `added, `included) in
  let hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Expanded;
      lines = [ first_line; second_line ];
    }
  in
  let file : TuiTypes.file =
    { path = FilePath "src/main"; visibility = Expanded; hunks = [ hunk ] }
  in
  let model : TuiModel.model =
    TuiModel.Line
      ( Zipper.Zip ([], file, []),
        Zipper.Zip ([], hunk, []),
        LineZipper.Zip ([ first_line ], second_line, []) )
  in

  let prev_model = TuiModel.prev model in

  let expected =
    TuiModel.Line
      ( Zipper.Zip ([], file, []),
        Zipper.Zip ([], hunk, []),
        LineZipper.Zip ([], first_line, [ second_line ]) )
  in
  check tui_model_testable "same TUI models" expected prev_model

let prev_suite =
  [
    ("moves the cursor from the first file to the last file", `Quick, test_to_last_file);
    ( "moves the cursor from the first file to the last file's last hunk",
      `Quick,
      test_to_last_file_last_hunk );
    ( "moves the cursor from the first file to the last hunk's last line",
      `Quick,
      test_to_last_file_last_hunk_last_line );
    ("moves the cursor from a hunk to its file", `Quick, test_hunk_to_file);
    ("moves the cursor from a hunk to the previous hunk", `Quick, test_hunk_to_hunk);
    ("moves the cursor from a hunk to the previous hunk's last line", `Quick, test_hunk_to_line);
    ("moves the cursor from a line to its hunk", `Quick, test_line_to_hunk);
    ("moves the cursor from a line to the previous line", `Quick, test_line_to_line);
  ]
