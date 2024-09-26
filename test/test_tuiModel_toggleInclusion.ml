open Alcotest
open Git_split

let tui_model_testable = testable TuiModel.pp_model TuiModel.equal_model

(* These tests define the raw model types in variables so that they are easily reusable in the zippers.
   This is necessary because the way the zippers are used creates data duplication and this prevents mismatches between the raw data and the one in zippers. *)

let test_toggle_selected_line () =
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
    { path = Path "src/main"; content = Text { visibility = Expanded; hunks = [ hunk ] } }
  in
  let model : TuiModel.model =
    TuiModel.Line
      ( Zipper.Zip ([], file, []),
        Zipper.Zip ([], hunk, []),
        LineZipper.Zip ([], first_line, [ second_line ]) )
  in

  let toggle_model = TuiModel.toggle_inclusion model in

  let unselected_line : TuiTypes.line = Diff ("code", `added, `notincluded) in
  let expected_hunk : TuiTypes.hunk = { hunk with lines = [ unselected_line; second_line ] } in
  let expected =
    TuiModel.Line
      ( Zipper.Zip
          ( [],
            {
              path = Path "src/main";
              content = Text { visibility = Expanded; hunks = [ expected_hunk ] };
            },
            [] ),
        Zipper.Zip ([], expected_hunk, []),
        LineZipper.Zip ([], unselected_line, [ second_line ]) )
  in
  check tui_model_testable "same TUI models" expected toggle_model

let test_toggle_unselected_line () =
  let unselected_line : TuiTypes.line = Diff ("code", `added, `notincluded) in
  let second_line : TuiTypes.line = Diff ("code2", `added, `included) in
  let hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Expanded;
      lines = [ unselected_line; second_line ];
    }
  in
  let file : TuiTypes.file =
    { path = Path "src/main"; content = Text { visibility = Expanded; hunks = [ hunk ] } }
  in
  let model : TuiModel.model =
    TuiModel.Line
      ( Zipper.Zip ([], file, []),
        Zipper.Zip ([], hunk, []),
        LineZipper.Zip ([], unselected_line, [ second_line ]) )
  in

  let toggle_model = TuiModel.toggle_inclusion model in

  let selected_line : TuiTypes.line = Diff ("code", `added, `included) in
  let expected_hunk : TuiTypes.hunk = { hunk with lines = [ selected_line; second_line ] } in
  let expected =
    TuiModel.Line
      ( Zipper.Zip
          ( [],
            {
              path = Path "src/main";
              content = Text { visibility = Expanded; hunks = [ expected_hunk ] };
            },
            [] ),
        Zipper.Zip ([], expected_hunk, []),
        LineZipper.Zip ([], selected_line, [ second_line ]) )
  in
  check tui_model_testable "same TUI models" expected toggle_model

let test_toggle_selected_hunk () =
  let hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Expanded;
      lines = [ Diff ("code", `added, `included); Diff ("code2", `added, `included) ];
    }
  in
  let file : TuiTypes.file =
    { path = Path "src/main"; content = Text { visibility = Expanded; hunks = [ hunk ] } }
  in
  let model : TuiModel.model =
    TuiModel.Hunk (Zipper.Zip ([], file, []), Zipper.Zip ([], hunk, []))
  in

  let toggle_model = TuiModel.toggle_inclusion model in

  let expected_hunk : TuiTypes.hunk =
    {
      hunk with
      lines = [ Diff ("code", `added, `notincluded); Diff ("code2", `added, `notincluded) ];
    }
  in
  let expected =
    TuiModel.Hunk
      ( Zipper.Zip
          ( [],
            {
              path = Path "src/main";
              content = Text { visibility = Expanded; hunks = [ expected_hunk ] };
            },
            [] ),
        Zipper.Zip ([], expected_hunk, []) )
  in
  check tui_model_testable "same TUI models" expected toggle_model

let test_toggle_unselected_hunk () =
  let hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Expanded;
      lines = [ Diff ("code", `added, `notincluded); Diff ("code2", `added, `notincluded) ];
    }
  in
  let file : TuiTypes.file =
    { path = Path "src/main"; content = Text { visibility = Expanded; hunks = [ hunk ] } }
  in
  let model : TuiModel.model =
    TuiModel.Hunk (Zipper.Zip ([], file, []), Zipper.Zip ([], hunk, []))
  in

  let toggle_model = TuiModel.toggle_inclusion model in

  let expected_hunk : TuiTypes.hunk =
    { hunk with lines = [ Diff ("code", `added, `included); Diff ("code2", `added, `included) ] }
  in
  let expected =
    TuiModel.Hunk
      ( Zipper.Zip
          ( [],
            {
              path = Path "src/main";
              content = Text { visibility = Expanded; hunks = [ expected_hunk ] };
            },
            [] ),
        Zipper.Zip ([], expected_hunk, []) )
  in
  check tui_model_testable "same TUI models" expected toggle_model

let test_toggle_partially_selected_hunk () =
  let hunk : TuiTypes.hunk =
    {
      starting_line = 1;
      context_snippet = None;
      visibility = Expanded;
      lines = [ Diff ("code", `added, `included); Diff ("code2", `added, `notincluded) ];
    }
  in
  let file : TuiTypes.file =
    { path = Path "src/main"; content = Text { visibility = Expanded; hunks = [ hunk ] } }
  in
  let model : TuiModel.model =
    TuiModel.Hunk (Zipper.Zip ([], file, []), Zipper.Zip ([], hunk, []))
  in

  let toggle_model = TuiModel.toggle_inclusion model in

  let expected_hunk : TuiTypes.hunk =
    { hunk with lines = [ Diff ("code", `added, `included); Diff ("code2", `added, `included) ] }
  in
  let expected =
    TuiModel.Hunk
      ( Zipper.Zip
          ( [],
            {
              path = Path "src/main";
              content = Text { visibility = Expanded; hunks = [ expected_hunk ] };
            },
            [] ),
        Zipper.Zip ([], expected_hunk, []) )
  in
  check tui_model_testable "same TUI models" expected toggle_model

let test_toggle_selected_file () =
  let file : TuiTypes.file =
    {
      path = Path "src/main";
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
                {
                  starting_line = 15;
                  context_snippet = None;
                  visibility = Expanded;
                  lines = [ Diff ("code2", `added, `included) ];
                };
              ];
          };
    }
  in
  let model : TuiModel.model = TuiModel.File (Zipper.Zip ([], file, [])) in

  let toggle_model = TuiModel.toggle_inclusion model in

  let expected =
    TuiModel.File
      (Zipper.Zip
         ( [],
           {
             path = Path "src/main";
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
                         lines = [ Diff ("code", `added, `notincluded) ];
                       };
                       {
                         starting_line = 15;
                         context_snippet = None;
                         visibility = Expanded;
                         lines = [ Diff ("code2", `added, `notincluded) ];
                       };
                     ];
                 };
           },
           [] ))
  in
  check tui_model_testable "same TUI models" expected toggle_model

let test_toggle_unselected_file () =
  let file : TuiTypes.file =
    {
      path = Path "src/main";
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
                  lines = [ Diff ("code", `added, `notincluded) ];
                };
                {
                  starting_line = 15;
                  context_snippet = None;
                  visibility = Expanded;
                  lines = [ Diff ("code2", `added, `notincluded) ];
                };
              ];
          };
    }
  in
  let model : TuiModel.model = TuiModel.File (Zipper.Zip ([], file, [])) in

  let toggle_model = TuiModel.toggle_inclusion model in

  let expected =
    TuiModel.File
      (Zipper.Zip
         ( [],
           {
             path = Path "src/main";
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
                       {
                         starting_line = 15;
                         context_snippet = None;
                         visibility = Expanded;
                         lines = [ Diff ("code2", `added, `included) ];
                       };
                     ];
                 };
           },
           [] ))
  in
  check tui_model_testable "same TUI models" expected toggle_model

let test_toggle_partially_selected_file () =
  let file : TuiTypes.file =
    {
      path = Path "src/main";
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
                {
                  starting_line = 15;
                  context_snippet = None;
                  visibility = Expanded;
                  lines = [ Diff ("code2", `added, `notincluded) ];
                };
              ];
          };
    }
  in
  let model : TuiModel.model = TuiModel.File (Zipper.Zip ([], file, [])) in

  let toggle_model = TuiModel.toggle_inclusion model in

  let expected =
    TuiModel.File
      (Zipper.Zip
         ( [],
           {
             path = Path "src/main";
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
                       {
                         starting_line = 15;
                         context_snippet = None;
                         visibility = Expanded;
                         lines = [ Diff ("code2", `added, `included) ];
                       };
                     ];
                 };
           },
           [] ))
  in
  check tui_model_testable "same TUI models" expected toggle_model

let toggle_inclusion_suite =
  [
    ("unselects a selected line", `Quick, test_toggle_selected_line);
    ("selects an unselected line", `Quick, test_toggle_unselected_line);
    ("unselects all lines in a selected hunk", `Quick, test_toggle_selected_hunk);
    ("selects all lines in an unselected hunk", `Quick, test_toggle_unselected_hunk);
    ("selects all lines in a partially selected hunk", `Quick, test_toggle_partially_selected_hunk);
    ("unselects all lines in a selected file", `Quick, test_toggle_selected_file);
    ("selects all lines in an unselected file", `Quick, test_toggle_unselected_file);
    ("selects all lines in a partially selected file", `Quick, test_toggle_partially_selected_file);
  ]
