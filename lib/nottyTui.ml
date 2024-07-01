open Notty
open Notty_unix

let last_item list = List.hd @@ List.rev list
let last_idx list = List.length list - 1

type visibility = Expanded | Collapsed

type line =
  | Context of string
  | Diff of string * [ `added | `removed ] * [ `included | `notincluded ]

type hunk = { lines : line list; lines_visibility : visibility }
type file = { path : string; hunks : hunk list; hunks_visibility : visibility }
type cursor = FileCursor of int | HunkCursor of int * int | LineCursor of int * int * int
type model = { files : file list; cursor : cursor }
type lines_included = AllLines | SomeLines | NoLines

let hunk_lines_included hunk =
  let all_lines_included =
    hunk.lines
    |> List.filter (fun line -> match line with Diff _ -> true | Context _ -> false)
    |> List.for_all (fun line ->
           match line with Diff (_, _, included) -> included = `included | Context _ -> true)
  in
  let any_line_included =
    hunk.lines
    |> List.exists (fun line ->
           match line with Diff (_, _, included) -> included = `included | Context _ -> false)
  in
  if all_lines_included then AllLines else if any_line_included then SomeLines else NoLines

let file_lines_included file =
  let all_lines_included =
    file.hunks
    |> List.map (fun hunk -> hunk_lines_included hunk)
    |> List.for_all (fun hunk_lines_inclusion -> hunk_lines_inclusion = AllLines)
  in
  let any_line_included =
    file.hunks
    |> List.map (fun hunk -> hunk_lines_included hunk)
    |> List.exists (fun hunk_lines_inclusion ->
           hunk_lines_inclusion = AllLines || hunk_lines_inclusion = SomeLines)
  in
  if all_lines_included then AllLines else if any_line_included then SomeLines else NoLines

exception Line_should_exist of string

let find_first_diff_line_idx lines =
  let idx_opt =
    lines |> List.find_index (fun line -> match line with Diff _ -> true | Context _ -> false)
  in
  match idx_opt with
  | None -> raise (Line_should_exist "Lines of a hunk must have at least one diff line.")
  | Some idx -> idx

let find_last_diff_line_idx lines =
  let idx_opt =
    lines |> List.rev
    |> List.find_index (fun line -> match line with Diff _ -> true | Context _ -> false)
  in
  match idx_opt with
  | None -> raise (Line_should_exist "Lines of a hunk must have at least one diff line.")
  | Some idx -> List.length lines - idx - 1

let find_prev_diff_line_idx lines line =
  let diff_lines =
    lines |> List.filter (fun line -> match line with Diff _ -> true | Context _ -> false)
  in
  match diff_lines |> List.find_index (fun diff_line -> diff_line = line) with
  | None ->
      raise
        (Line_should_exist "Diff lines are a subset of hunk's lines, so line should be in both.")
  | Some 0 -> None
  | Some idx ->
      let prev_diff_line = List.nth diff_lines (idx - 1) in
      lines |> List.find_index (fun line -> line = prev_diff_line)

let find_next_diff_line_idx lines line =
  let diff_lines =
    lines |> List.filter (fun line -> match line with Diff _ -> true | Context _ -> false)
  in
  match diff_lines |> List.find_index (fun diff_line -> diff_line = line) with
  | None ->
      raise
        (Line_should_exist "Diff lines are a subset of hunk's lines, so line should be in both.")
  | Some idx when idx = last_idx diff_lines -> None
  | Some idx ->
      let prev_diff_line = List.nth diff_lines (idx + 1) in
      lines |> List.find_index (fun line -> line = prev_diff_line)

let initial_model =
  {
    cursor = FileCursor 0;
    files =
      [
        {
          path = "src/TestMain";
          hunks_visibility = Collapsed;
          hunks =
            [
              {
                lines =
                  [
                    Context "code";
                    Diff ("code 2", `removed, `included);
                    Diff ("code 3", `added, `included);
                    Context "code 4";
                  ];
                lines_visibility = Expanded;
              };
              {
                lines =
                  [
                    Context "code 5";
                    Context "code 6";
                    Context "code 7";
                    Diff ("code 7.5", `removed, `included);
                    Diff ("code 7.9", `added, `included);
                    Context "code 8";
                  ];
                lines_visibility = Expanded;
              };
            ];
        };
        {
          path = "src/MainFile";
          hunks_visibility = Collapsed;
          hunks =
            [
              {
                lines =
                  [
                    Context "code";
                    Diff ("code 2", `removed, `included);
                    Diff ("code 3", `added, `included);
                    Context "code 4";
                  ];
                lines_visibility = Expanded;
              };
              {
                lines =
                  [
                    Context "code 5";
                    Context "code 6";
                    Context "code 7";
                    Diff ("code 7.5", `removed, `included);
                    Diff ("code 7.9", `added, `included);
                    Context "code 8";
                  ];
                lines_visibility = Expanded;
              };
            ];
        };
        {
          path = "src/YetAnotherFile";
          hunks_visibility = Collapsed;
          hunks =
            [
              {
                lines =
                  [
                    Diff ("These", `added, `included);
                    Diff ("Are", `added, `included);
                    Diff ("All", `added, `included);
                    Diff ("Added", `added, `included);
                    Diff ("Lines", `added, `included);
                    Diff ("Because", `added, `included);
                    Diff ("This", `added, `included);
                    Diff ("Is", `added, `included);
                    Diff ("A", `added, `included);
                    Diff ("New", `added, `included);
                    Diff ("File", `added, `included);
                  ];
                lines_visibility = Expanded;
              };
            ];
        };
      ];
  }

let update event model =
  match event with
  | `Key (`Arrow `Up, _) | `Key (`ASCII 'k', _) ->
      let cursor =
        match model.cursor with
        | FileCursor 0 -> (
            let last_file_idx = last_idx model.files in
            let last_file = last_item model.files in
            match last_file.hunks_visibility with
            | Collapsed -> FileCursor last_file_idx
            | Expanded -> (
                let last_hunk = last_item last_file.hunks in
                match last_hunk.lines_visibility with
                | Collapsed -> HunkCursor (last_file_idx, last_idx last_file.hunks)
                | Expanded ->
                    let hunk = last_item last_file.hunks in
                    let line_idx = find_last_diff_line_idx hunk.lines in
                    LineCursor (last_file_idx, last_idx last_file.hunks, line_idx)))
        | FileCursor c_file_idx -> (
            let prev_file = List.nth model.files (c_file_idx - 1) in
            match prev_file.hunks_visibility with
            | Collapsed -> FileCursor (c_file_idx - 1)
            | Expanded -> (
                let last_hunk = last_item prev_file.hunks in
                match last_hunk.lines_visibility with
                | Collapsed -> HunkCursor (c_file_idx - 1, last_idx prev_file.hunks)
                | Expanded ->
                    let line_idx = find_last_diff_line_idx last_hunk.lines in
                    LineCursor (c_file_idx - 1, last_idx prev_file.hunks, line_idx)))
        | HunkCursor (c_file_idx, 0) -> FileCursor c_file_idx
        | HunkCursor (c_file_idx, c_hunk_idx) -> (
            let file = List.nth model.files c_file_idx in
            let prev_hunk = List.nth file.hunks (c_hunk_idx - 1) in
            match prev_hunk.lines_visibility with
            | Collapsed -> HunkCursor (c_file_idx, c_hunk_idx - 1)
            | Expanded ->
                let line_idx = find_last_diff_line_idx prev_hunk.lines in
                LineCursor (c_file_idx, c_hunk_idx - 1, line_idx))
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx) -> (
            let hunk = List.nth (List.nth model.files c_file_idx).hunks c_hunk_idx in
            let line = List.nth hunk.lines c_line_idx in
            let prev_diff_line_idx = find_prev_diff_line_idx hunk.lines line in
            match prev_diff_line_idx with
            | None -> HunkCursor (c_file_idx, c_hunk_idx)
            | Some idx -> LineCursor (c_file_idx, c_hunk_idx, idx))
      in
      { model with cursor }
  | `Key (`Arrow `Down, _) | `Key (`ASCII 'j', _) ->
      let last_file_idx = last_idx model.files in
      let cursor =
        match model.cursor with
        | FileCursor c_file_idx when c_file_idx = last_file_idx -> (
            let last_file = last_item model.files in
            match last_file.hunks_visibility with
            | Collapsed -> FileCursor 0
            | Expanded -> HunkCursor (c_file_idx, 0))
        | FileCursor c_idx -> (
            let file = List.nth model.files c_idx in
            match file.hunks_visibility with
            | Collapsed -> FileCursor (c_idx + 1)
            | Expanded -> HunkCursor (c_idx, 0))
        | HunkCursor (c_file_idx, c_hunk_idx)
          when c_hunk_idx = last_idx (List.nth model.files c_file_idx).hunks -> (
            let hunk = List.nth (List.nth model.files c_file_idx).hunks c_hunk_idx in
            match hunk.lines_visibility with
            | Expanded ->
                let first_diff_line = find_first_diff_line_idx hunk.lines in
                LineCursor (c_file_idx, c_hunk_idx, first_diff_line)
            | Collapsed ->
                if c_file_idx = last_file_idx then FileCursor 0 else FileCursor (c_file_idx + 1))
        | HunkCursor (c_file_idx, c_hunk_idx) -> (
            let hunk = List.nth (List.nth model.files c_file_idx).hunks c_hunk_idx in
            match hunk.lines_visibility with
            | Collapsed -> HunkCursor (c_file_idx, c_hunk_idx + 1)
            | Expanded ->
                let first_diff_line = find_first_diff_line_idx hunk.lines in
                LineCursor (c_file_idx, c_hunk_idx, first_diff_line))
        (* Cursor is at last file -> last hunk -> line. *)
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx)
          when c_file_idx = last_file_idx
               && c_hunk_idx = last_idx (List.nth model.files c_file_idx).hunks -> (
            let hunk = List.nth (List.nth model.files c_file_idx).hunks c_hunk_idx in
            let curr_line = List.nth hunk.lines c_line_idx in
            let next_diff_line_idx = find_next_diff_line_idx hunk.lines curr_line in
            match next_diff_line_idx with
            | None -> FileCursor 0
            | Some idx -> LineCursor (c_file_idx, c_hunk_idx, idx))
        (* Cursor is at some file -> last hunk -> line. *)
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx)
          when c_hunk_idx = last_idx (List.nth model.files c_file_idx).hunks -> (
            let hunk = List.nth (List.nth model.files c_file_idx).hunks c_hunk_idx in
            let curr_line = List.nth hunk.lines c_line_idx in
            let next_diff_line_idx = find_next_diff_line_idx hunk.lines curr_line in
            match next_diff_line_idx with
            | None -> FileCursor (c_file_idx + 1)
            | Some idx -> LineCursor (c_file_idx, c_hunk_idx, idx))
        (* General some line case. *)
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx) -> (
            let hunk = List.nth (List.nth model.files c_file_idx).hunks c_hunk_idx in
            let curr_line = List.nth hunk.lines c_line_idx in
            let next_diff_line_idx = find_next_diff_line_idx hunk.lines curr_line in
            match next_diff_line_idx with
            | None -> HunkCursor (c_file_idx, c_hunk_idx + 1) (* No next diff line. *)
            | Some idx -> LineCursor (c_file_idx, c_hunk_idx, idx))
      in
      { model with cursor }
  | `Key (`Arrow `Right, _) | `Key (`ASCII 'l', _) -> (
      match model.cursor with
      | FileCursor c_file_idx ->
          let files =
            model.files
            |> List.mapi (fun file_idx file ->
                   let hunks_visibility =
                     if file_idx = c_file_idx then Expanded else file.hunks_visibility
                   in
                   { file with hunks_visibility })
          in
          { model with files }
      | HunkCursor (c_file_idx, c_hunk_idx) ->
          let files =
            model.files
            |> List.mapi (fun file_idx file ->
                   let hunks =
                     file.hunks
                     |> List.mapi (fun hunk_idx hunk ->
                            let lines_visibility =
                              if hunk_idx = c_hunk_idx && file_idx = c_file_idx then Expanded
                              else hunk.lines_visibility
                            in
                            { hunk with lines_visibility })
                   in
                   { file with hunks })
          in
          { model with files }
      | LineCursor _ -> model)
  | `Key (`Arrow `Left, _) | `Key (`ASCII 'h', _) -> (
      match model.cursor with
      | FileCursor c_file_idx ->
          let files =
            model.files
            |> List.mapi (fun file_idx file ->
                   let hunks_visibility =
                     if file_idx = c_file_idx then Collapsed else file.hunks_visibility
                   in
                   { file with hunks_visibility })
          in
          { model with files }
      | HunkCursor (c_file_idx, c_hunk_idx) ->
          let files =
            model.files
            |> List.mapi (fun file_idx file ->
                   let hunks =
                     file.hunks
                     |> List.mapi (fun hunk_idx hunk ->
                            let lines_visibility =
                              if hunk_idx = c_hunk_idx && file_idx = c_file_idx then Collapsed
                              else hunk.lines_visibility
                            in
                            { hunk with lines_visibility })
                   in
                   { file with hunks })
          in
          { model with files }
      | LineCursor _ -> model)
  | `Key (`ASCII ' ', _) | `Key (`Enter, _) ->
      let toggle_line include_status =
        match include_status with `included -> `notincluded | `notincluded -> `included
      in

      let toggle_group = function
        | NoLines -> `included
        | SomeLines -> `included
        | AllLines -> `notincluded
      in

      let files =
        match model.cursor with
        | FileCursor c_file_idx ->
            model.files
            |> List.mapi (fun file_idx file ->
                   let hunks =
                     if file_idx <> c_file_idx then file.hunks
                     else
                       let file_lines_inclusion = file_lines_included file in
                       let included_in_diff = toggle_group file_lines_inclusion in
                       file.hunks
                       |> List.map (fun hunk ->
                              let lines =
                                hunk.lines
                                |> List.map (fun line ->
                                       match line with
                                       | Context _ -> line
                                       | Diff (contents, diff_type, _) ->
                                           Diff (contents, diff_type, included_in_diff))
                              in
                              { hunk with lines })
                   in
                   { file with hunks })
        | HunkCursor (c_file_idx, c_hunk_idx) ->
            model.files
            |> List.mapi (fun file_idx file ->
                   let hunks =
                     if file_idx <> c_file_idx then file.hunks
                     else
                       file.hunks
                       |> List.mapi (fun hunk_idx hunk ->
                              let hunk_lines_inclusion = hunk_lines_included hunk in
                              let included_in_diff = toggle_group hunk_lines_inclusion in
                              let lines =
                                if hunk_idx <> c_hunk_idx then hunk.lines
                                else
                                  hunk.lines
                                  |> List.map (fun line ->
                                         match line with
                                         | Context _ -> line
                                         | Diff (contents, diff_type, _) ->
                                             Diff (contents, diff_type, included_in_diff))
                              in
                              { hunk with lines })
                   in
                   { file with hunks })
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx) ->
            model.files
            |> List.mapi (fun file_idx file ->
                   let hunks =
                     if file_idx <> c_file_idx then file.hunks
                     else
                       file.hunks
                       |> List.mapi (fun hunk_idx hunk ->
                              let lines =
                                if hunk_idx <> c_hunk_idx then hunk.lines
                                else
                                  hunk.lines
                                  |> List.mapi (fun line_idx line ->
                                         if line_idx <> c_line_idx then line
                                         else
                                           match line with
                                           | Context _ -> line
                                           | Diff (contents, diff_type, included) ->
                                               Diff (contents, diff_type, toggle_line included))
                              in
                              { hunk with lines })
                   in
                   { file with hunks })
      in
      { model with files }
  | _ -> model

let render_line line cursor file_idx hunk_idx line_idx =
  let style =
    if cursor = LineCursor (file_idx, hunk_idx, line_idx) then A.st A.reverse else A.empty
  in

  let included_prefix =
    match line with
    | Context _ -> "   "
    | Diff (_, _, `included) -> "[x]"
    | Diff (_, _, `notincluded) -> "[ ]"
  in

  let line_kind_prefix =
    match line with Context _ -> " " | Diff (_, `removed, _) -> "-" | Diff (_, `added, _) -> "+"
  in

  let line_content =
    match line with Context content -> content | Diff (content, _, _) -> content
  in

  let colour =
    match line with
    | Context _ -> A.gray 10
    | Diff (_, `added, _) -> A.green
    | Diff (_, `removed, _) -> A.red
  in

  I.strf ~attr:A.(fg colour ++ style) "%s %s %s" included_prefix line_kind_prefix line_content
  |> I.hpad 4 0

let render_hunk hunk cursor file_idx hunk_idx =
  let lines =
    match hunk.lines_visibility with
    | Collapsed -> []
    | Expanded ->
        hunk.lines
        |> List.mapi (fun line_idx line -> render_line line cursor file_idx hunk_idx line_idx)
  in

  let style = if cursor = HunkCursor (file_idx, hunk_idx) then A.st A.reverse else A.empty in
  let hunk_included_marker =
    match hunk_lines_included hunk with AllLines -> "x" | SomeLines -> "~" | NoLines -> " "
  in
  let hunk_line =
    I.strf ~attr:style "[%s] Hunk %d" hunk_included_marker (hunk_idx + 1) |> I.hpad 2 0
  in
  hunk_line :: lines

let render_file file_idx file cursor =
  let hunk_lines =
    match file.hunks_visibility with
    | Collapsed -> []
    | Expanded ->
        file.hunks
        |> List.mapi (fun hunk_idx hunk -> render_hunk hunk cursor file_idx hunk_idx)
        |> List.flatten
  in

  let content = file.path in
  let style = if cursor = FileCursor file_idx then A.st A.reverse else A.empty in
  let file_included_marker =
    match file_lines_included file with AllLines -> "x" | SomeLines -> "~" | NoLines -> " "
  in
  let file_line = I.strf ~attr:style "[%s] %s" file_included_marker content in
  file_line :: hunk_lines

let with_cursor files =
  files
  |> List.mapi (fun file_idx file ->
         let hunks =
           match file.hunks_visibility with
           | Collapsed -> []
           | Expanded ->
               file.hunks
               |> List.mapi (fun hunk_idx hunk ->
                      let lines =
                        match hunk.lines_visibility with
                        | Collapsed -> []
                        | Expanded ->
                            hunk.lines
                            |> List.mapi (fun line_idx _line ->
                                   LineCursor (file_idx, hunk_idx, line_idx))
                      in
                      HunkCursor (file_idx, hunk_idx) :: lines)
               |> List.flatten
         in
         FileCursor file_idx :: hunks)
  |> List.flatten

let view model viewport_height =
  let maybe_cursor_index =
    with_cursor model.files |> List.find_index (fun item -> item = model.cursor)
  in
  let cursor_index = match maybe_cursor_index with None -> 0 | Some idx -> idx in

  let terminal_lines =
    model.files
    |> List.mapi (fun file_idx file -> render_file file_idx file model.cursor)
    |> List.flatten
  in

  let cursor_top_lines = (viewport_height - 1) / 2 in
  let cursor_bottom_lines = viewport_height - cursor_top_lines in
  let top = cursor_index - cursor_top_lines in
  let bottom = cursor_index + cursor_bottom_lines in
  let start_line, end_line =
    match (top, bottom) with
    | top, _ when top < 0 -> (0, viewport_height)
    | _, bottom when bottom >= List.length terminal_lines ->
        (List.length terminal_lines - viewport_height, List.length terminal_lines)
    | top, bottom -> (top, bottom)
  in

  let visible_lines =
    terminal_lines |> List.filteri (fun idx _ -> idx >= start_line && idx < end_line)
  in

  I.vcat visible_lines

let rec ui_loop t state =
  let _, h = Term.size t in
  Term.image t (view state h);
  match Term.event t with
  | `Key (`Escape, _) | `Key (`ASCII 'q', _) -> state
  | event ->
      let new_state = update event state in
      ui_loop t new_state

let run initial_model =
  let term = Term.create ~dispose:true ~mouse:false () in
  let final_model = ui_loop term initial_model in
  Term.release term;
  final_model
