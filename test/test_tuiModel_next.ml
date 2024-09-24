open Alcotest
open Git_split

let tui_model_testable = testable TuiModel.pp_model TuiModel.equal_model

(* These tests define the raw model types in variables so that they are easily reusable in the zippers.
   This is necessary because the way the zippers are used creates data duplication and this prevents mismatches between the raw data and the one in zippers. *)

let test_file_to_next_file () =
  let first_file : TuiTypes.file =
    {
      path = Path "src/main";
      visibility = Collapsed;
      content =
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
      path = Path "src/test";
      visibility = Collapsed;
      content =
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

  let next_model = TuiModel.next model in

  let expected = TuiModel.File (Zipper.Zip ([ first_file ], second_file, [])) in
  check tui_model_testable "same TUI models" expected next_model

let test_file_to_hunk () =
  let hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Collapsed;
      lines = [ Diff ("code", `added, `included) ];
    }
  in
  let file : TuiTypes.file =
    { path = Path "src/main"; visibility = Expanded; content = [ hunk ] }
  in
  let model : TuiModel.model = TuiModel.File (Zipper.Zip ([], file, [])) in

  let next_model = TuiModel.next model in

  let expected = TuiModel.Hunk (Zipper.Zip ([], file, []), Zipper.Zip ([], hunk, [])) in
  check tui_model_testable "same TUI models" expected next_model

let test_hunk_to_next_hunk () =
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
      starting_line = 1;
      context_snippet = None;
      visibility = Collapsed;
      lines = [ Diff ("code", `added, `included) ];
    }
  in
  let file : TuiTypes.file =
    { path = Path "src/main"; visibility = Expanded; content = [ first_hunk; second_hunk ] }
  in
  let model : TuiModel.model =
    TuiModel.Hunk (Zipper.Zip ([], file, []), Zipper.Zip ([], first_hunk, [ second_hunk ]))
  in

  let next_model = TuiModel.next model in

  let expected =
    TuiModel.Hunk (Zipper.Zip ([], file, []), Zipper.Zip ([ first_hunk ], second_hunk, []))
  in
  check tui_model_testable "same TUI models" expected next_model

let test_hunk_to_line () =
  let line : TuiTypes.line = Diff ("code", `added, `included) in
  let hunk : TuiTypes.hunk =
    { starting_line = 1; context_snippet = None; visibility = Expanded; lines = [ line ] }
  in
  let file : TuiTypes.file =
    { path = Path "src/main"; visibility = Expanded; content = [ hunk ] }
  in
  let model : TuiModel.model =
    TuiModel.Hunk (Zipper.Zip ([], file, []), Zipper.Zip ([], hunk, []))
  in

  let next_model = TuiModel.next model in

  let expected =
    TuiModel.Line
      (Zipper.Zip ([], file, []), Zipper.Zip ([], hunk, []), LineZipper.Zip ([], line, []))
  in
  check tui_model_testable "same TUI models" expected next_model

let test_line_to_next_line () =
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
    { path = Path "src/main"; visibility = Expanded; content = [ hunk ] }
  in
  let model : TuiModel.model =
    TuiModel.Line
      ( Zipper.Zip ([], file, []),
        Zipper.Zip ([], hunk, []),
        LineZipper.Zip ([], first_line, [ second_line ]) )
  in

  let next_model = TuiModel.next model in

  let expected =
    TuiModel.Line
      ( Zipper.Zip ([], file, []),
        Zipper.Zip ([], hunk, []),
        LineZipper.Zip ([ first_line ], second_line, []) )
  in
  check tui_model_testable "same TUI models" expected next_model

let test_hunk_last_line_to_next_hunk () =
  let line : TuiTypes.line = Diff ("code", `added, `included) in
  let first_hunk : TuiTypes.hunk =
    { starting_line = 1; context_snippet = None; visibility = Expanded; lines = [ line ] }
  in
  let second_hunk : TuiTypes.hunk =
    { starting_line = 15; context_snippet = None; visibility = Collapsed; lines = [] }
  in
  let file : TuiTypes.file =
    { path = Path "src/main"; visibility = Expanded; content = [ first_hunk; second_hunk ] }
  in
  let model : TuiModel.model =
    TuiModel.Line
      ( Zipper.Zip ([], file, []),
        Zipper.Zip ([], first_hunk, [ second_hunk ]),
        LineZipper.Zip ([], line, []) )
  in

  let next_model = TuiModel.next model in

  let expected =
    TuiModel.Hunk (Zipper.Zip ([], file, []), Zipper.Zip ([ first_hunk ], second_hunk, []))
  in
  check tui_model_testable "same TUI models" expected next_model

let test_last_hunk_last_line_to_next_file () =
  let first_file_line : TuiTypes.line = Diff ("code", `added, `included) in
  let first_file_hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Expanded;
      lines = [ first_file_line ];
    }
  in
  let first_file : TuiTypes.file =
    { path = Path "src/main"; visibility = Expanded; content = [ first_file_hunk ] }
  in
  let second_file : TuiTypes.file =
    { path = Path "src/test"; visibility = Collapsed; content = [] }
  in
  let model : TuiModel.model =
    TuiModel.Line
      ( Zipper.Zip ([], first_file, [ second_file ]),
        Zipper.Zip ([], first_file_hunk, []),
        LineZipper.Zip ([], first_file_line, []) )
  in

  let next_model = TuiModel.next model in

  let expected = TuiModel.File (Zipper.Zip ([ first_file ], second_file, [])) in
  check tui_model_testable "same TUI models" expected next_model

let test_last_file_to_first_file () =
  let first_file : TuiTypes.file =
    { path = Path "src/main"; visibility = Expanded; content = [] }
  in
  let last_file : TuiTypes.file =
    { path = Path "src/test"; visibility = Collapsed; content = [] }
  in
  let model : TuiModel.model = TuiModel.File (Zipper.Zip ([ first_file ], last_file, [])) in

  let next_model = TuiModel.next model in

  let expected = TuiModel.File (Zipper.Zip ([], first_file, [ last_file ])) in
  check tui_model_testable "same TUI models" expected next_model

let test_last_file_last_hunk_to_first_file () =
  let first_file : TuiTypes.file =
    { path = Path "src/main"; visibility = Expanded; content = [] }
  in
  let last_file_hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Collapsed;
      lines = [ Diff ("code", `added, `included) ];
    }
  in
  let last_file : TuiTypes.file =
    { path = Path "src/test"; visibility = Collapsed; content = [ last_file_hunk ] }
  in
  let model : TuiModel.model =
    TuiModel.Hunk (Zipper.Zip ([ first_file ], last_file, []), Zipper.Zip ([], last_file_hunk, []))
  in

  let next_model = TuiModel.next model in

  let expected = TuiModel.File (Zipper.Zip ([], first_file, [ last_file ])) in
  check tui_model_testable "same TUI models" expected next_model

let test_last_file_last_hunk_last_line_to_first_file () =
  let first_file : TuiTypes.file =
    { path = Path "src/main"; visibility = Expanded; content = [] }
  in
  let last_file_hunk_line : TuiTypes.line = Diff ("code", `added, `included) in
  let last_file_hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Collapsed;
      lines = [ last_file_hunk_line ];
    }
  in
  let last_file : TuiTypes.file =
    { path = Path "src/test"; visibility = Collapsed; content = [ last_file_hunk ] }
  in
  let model : TuiModel.model =
    TuiModel.Line
      ( Zipper.Zip ([ first_file ], last_file, []),
        Zipper.Zip ([], last_file_hunk, []),
        LineZipper.Zip ([], last_file_hunk_line, []) )
  in

  let next_model = TuiModel.next model in

  let expected = TuiModel.File (Zipper.Zip ([], first_file, [ last_file ])) in
  check tui_model_testable "same TUI models" expected next_model

let next_suite =
  [
    ("moves the cursor from a file to the next file", `Quick, test_file_to_next_file);
    ("moves the cursor from a file to its first hunk", `Quick, test_file_to_hunk);
    ("moves the cursor from a hunk to the next hunk", `Quick, test_hunk_to_next_hunk);
    ("moves the cursor from a hunk to its first line", `Quick, test_hunk_to_line);
    ("moves the cursor from a line to the next line", `Quick, test_line_to_next_line);
    ( "moves the cursor from the last line of a hunk to the next hunk",
      `Quick,
      test_hunk_last_line_to_next_hunk );
    ( "moves the cursor from the last line of the last hunk of a file to the next file",
      `Quick,
      test_last_hunk_last_line_to_next_file );
    ("moves the cursor from the last file to the first file", `Quick, test_last_file_to_first_file);
    ( "moves the cursor from the last file's last hunk to the first file",
      `Quick,
      test_last_file_last_hunk_to_first_file );
    ( "moves the cursor from the last file's last hunk's last line to the first file",
      `Quick,
      test_last_file_last_hunk_last_line_to_first_file );
  ]
