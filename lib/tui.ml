open Minttea

let cursor_style = Spices.(default |> reverse true |> build)
let header = Spices.(default |> reverse true |> build)

type visibility = Expanded | Collapsed

type line =
  | Context of string
  | Diff of string * [ `added | `removed ] * [ `included | `notincluded ]

type hunk = { lines : line list; lines_visibility : visibility }
type file = { path : string; hunks : hunk list; hunks_visibility : visibility }
type cursor = FileCursor of int | HunkCursor of int * int | LineCursor of int * int * int
type model = { files : file list; cursor : cursor }
type set_lines_inclusion = AllLines | SomeLines | NoLines

let get_hunk files file_idx hunk_idx =
  let file = List.nth files file_idx in
  List.nth file.hunks hunk_idx

let hunk_lines_inclusion hunk =
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

let file_lines_inclusion file =
  let all_lines_included =
    file.hunks
    |> List.map (fun hunk -> hunk_lines_inclusion hunk)
    |> List.for_all (fun hunk_lines_inclusion -> hunk_lines_inclusion = AllLines)
  in
  let any_line_included =
    file.hunks
    |> List.map (fun hunk -> hunk_lines_inclusion hunk)
    |> List.exists (fun hunk_lines_inclusion ->
           hunk_lines_inclusion = AllLines || hunk_lines_inclusion = SomeLines)
  in
  if all_lines_included then AllLines else if any_line_included then SomeLines else NoLines

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

let init _model = Command.Seq [ Command.Enter_alt_screen; Command.Hide_cursor ]

let find_previous_diff_line lines line_idx =
  let diff_lines =
    List.filter (fun line -> match line with Diff _ -> true | Context _ -> false) lines
  in
  let current_line = List.nth lines line_idx in
  match List.find_index (fun line -> line = current_line) diff_lines with
  | None -> None
  | Some 0 -> None
  | Some idx ->
      let prev_diff_line = List.nth diff_lines (idx - 1) in
      List.find_index (fun line -> line = prev_diff_line) lines

let find_next_diff_line lines line_idx =
  let diff_lines =
    List.filter (fun line -> match line with Diff _ -> true | Context _ -> false) lines
  in
  let current_line = List.nth lines line_idx in
  match List.find_index (fun line -> line = current_line) diff_lines with
  | None -> None
  | Some idx when idx = List.length diff_lines - 1 -> None
  | Some idx ->
      let prev_diff_line = List.nth diff_lines (idx + 1) in
      List.find_index (fun line -> line = prev_diff_line) lines

let update event model =
  match event with
  (* if we press `q` or the escape key, we exit. *)
  | Event.KeyDown ((Key "q" | Escape), _modifier) ->
      (model, Command.Seq [ Command.Exit_alt_screen; Command.Quit ])
  (* if we press up or `k`, we move up in the list. *)
  | Event.KeyDown ((Up | Key "k"), _modifier) ->
      let cursor =
        match model.cursor with
        | FileCursor 0 -> FileCursor (List.length model.files - 1)
        | FileCursor file_idx -> FileCursor (file_idx - 1)
        | HunkCursor (file_idx, 0) -> FileCursor file_idx
        | HunkCursor (file_idx, hunk_idx) -> HunkCursor (file_idx, hunk_idx - 1)
        | LineCursor (file_idx, hunk_idx, line_idx) -> (
            let hunk_lines = (get_hunk model.files file_idx hunk_idx).lines in
            let prev_diff_line_idx = find_previous_diff_line hunk_lines line_idx in
            match prev_diff_line_idx with
            | None -> HunkCursor (file_idx, hunk_idx) (* Already at the first diff_line. *)
            | Some idx -> LineCursor (file_idx, hunk_idx, idx))
      in

      ({ model with cursor }, Command.Noop)
  (* if we press down or `j`, we move down in the list. *)
  | Event.KeyDown ((Down | Key "j"), _modifier) ->
      let last_file_idx = List.length model.files - 1 in
      let cursor =
        match model.cursor with
        | FileCursor file_idx when file_idx = last_file_idx -> FileCursor 0
        | FileCursor file_idx -> FileCursor (file_idx + 1)
        | HunkCursor (file_idx, hunk_idx)
          when let last_hunk_idx = List.length (List.nth model.files file_idx).hunks - 1 in
               hunk_idx = last_hunk_idx ->
            if file_idx = last_file_idx then FileCursor 0 else FileCursor (file_idx + 1)
        | HunkCursor (file_idx, hunk_idx) -> HunkCursor (file_idx, hunk_idx + 1)
        | LineCursor (file_idx, hunk_idx, line_idx) -> (
            let hunk_lines = (get_hunk model.files file_idx hunk_idx).lines in
            let next_diff_line_idx = find_next_diff_line hunk_lines line_idx in
            match next_diff_line_idx with
            | None ->
                (* Already at the last diff_line. *)
                let last_hunk_idx = List.length (List.nth model.files file_idx).hunks - 1 in
                if hunk_idx <> last_hunk_idx then HunkCursor (file_idx, hunk_idx + 1)
                else if file_idx <> last_file_idx then FileCursor (file_idx + 1)
                else FileCursor 0
            | Some idx -> LineCursor (file_idx, hunk_idx, idx))
      in
      ({ model with cursor }, Command.Noop)
  (* if we press right or `l`, we expand the current item and move to the first subitem. *)
  | Event.KeyDown ((Right | Key "l"), _modifier) ->
      let cursor =
        match model.cursor with
        | FileCursor file_idx -> HunkCursor (file_idx, 0)
        | HunkCursor (file_idx, hunk_idx) -> (
            let lines = (get_hunk model.files file_idx hunk_idx).lines in
            let first_diff_line =
              lines
              |> List.find_index (fun line -> match line with Diff _ -> true | Context _ -> false)
            in
            match first_diff_line with
            | None -> HunkCursor (file_idx, hunk_idx)
            | Some line_idx -> LineCursor (file_idx, hunk_idx, line_idx))
        | LineCursor _ -> model.cursor
      in
      let files =
        match model.cursor with
        | LineCursor _ -> model.files
        | FileCursor c_file_idx -> (
            let file = List.nth model.files c_file_idx in
            match file.hunks_visibility with
            | Expanded -> model.files
            | Collapsed ->
                model.files
                |> List.mapi (fun file_idx file ->
                       if file_idx <> c_file_idx then file
                       else { file with hunks_visibility = Expanded }))
        | HunkCursor (c_file_idx, c_hunk_idx) -> (
            let hunk = get_hunk model.files c_file_idx c_hunk_idx in
            match hunk.lines_visibility with
            | Expanded -> model.files
            | Collapsed ->
                model.files
                |> List.mapi (fun file_idx file ->
                       if file_idx <> c_file_idx then file
                       else
                         let hunks =
                           file.hunks
                           |> List.mapi (fun hunk_idx hunk ->
                                  let lines_visibility =
                                    if hunk_idx = c_hunk_idx then Expanded
                                    else hunk.lines_visibility
                                  in
                                  { hunk with lines_visibility })
                         in
                         { file with hunks }))
      in
      ({ files; cursor }, Command.Noop)
  (* if we press left or `h`, we move left in the list *)
  | Event.KeyDown ((Left | Key "h"), _modifier) ->
      let cursor =
        match model.cursor with
        | FileCursor _ -> model.cursor
        | HunkCursor (file_idx, hunk_idx) -> (
            let hunk = List.nth (List.nth model.files file_idx).hunks hunk_idx in
            match hunk.lines_visibility with
            | Expanded -> HunkCursor (file_idx, hunk_idx)
            | Collapsed -> FileCursor file_idx)
        | LineCursor (file_idx, hunk_idx, _) -> HunkCursor (file_idx, hunk_idx)
      in

      let files =
        match model.cursor with
        | LineCursor _ -> model.files
        | FileCursor c_file_idx -> (
            let file = List.nth model.files c_file_idx in
            match file.hunks_visibility with
            | Collapsed -> model.files
            | Expanded ->
                model.files
                |> List.mapi (fun file_idx file ->
                       if file_idx <> c_file_idx then file
                       else { file with hunks_visibility = Collapsed }))
        | HunkCursor (c_file_idx, c_hunk_idx) -> (
            let hunk = List.nth (List.nth model.files c_file_idx).hunks c_hunk_idx in
            match hunk.lines_visibility with
            | Collapsed -> model.files
            | Expanded ->
                model.files
                |> List.mapi (fun file_idx file ->
                       if file_idx <> c_file_idx then file
                       else
                         let hunks =
                           file.hunks
                           |> List.mapi (fun hunk_idx hunk ->
                                  let lines_visibility =
                                    if hunk_idx = c_hunk_idx then Collapsed
                                    else hunk.lines_visibility
                                  in
                                  { hunk with lines_visibility })
                         in
                         { file with hunks }))
      in
      ({ files; cursor }, Command.Noop)
  (* when we press enter or space we toggle the item in the list
     that the cursor points to *)
  | Event.KeyDown ((Enter | Space), _modifier) ->
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
                       let file_lines_inclusion = file_lines_inclusion file in
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
                              let hunk_lines_inclusion = hunk_lines_inclusion hunk in
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
      ({ model with files }, Command.Noop)
  (* for all other events, we do nothing *)
  | _ -> (model, Command.Noop)

let render_line line cursor file_idx hunk_idx line_idx =
  let included_prefix =
    match line with
    | Context _ -> "   "
    | Diff (_, _, `included) -> "[x]"
    | Diff (_, _, `notincluded) -> "[ ]"
  in

  let line_kind_prefix =
    match line with Context _ -> " " | Diff (_, `removed, _) -> "-" | Diff (_, `added, _) -> "+"
  in

  let raw_content =
    match line with Context content -> content | Diff (content, _, _) -> content
  in

  let line_content = Format.sprintf "%s %s %s" included_prefix line_kind_prefix raw_content in

  let maybe_highlighted_line =
    if cursor = LineCursor (file_idx, hunk_idx, line_idx) then cursor_style "%s" line_content
    else line_content
  in

  Format.sprintf "\t\t%s" maybe_highlighted_line

let render_hunk hunk cursor file_idx hunk_idx =
  let code_lines =
    match hunk.lines_visibility with
    | Collapsed -> []
    | Expanded ->
        hunk.lines
        |> List.mapi (fun line_idx line -> render_line line cursor file_idx hunk_idx line_idx)
  in

  let is_hunk_included_marker =
    match hunk_lines_inclusion hunk with AllLines -> "x" | SomeLines -> "~" | NoLines -> " "
  in
  let hunk_header_content =
    Format.sprintf "\t[%s] Hunk %d" is_hunk_included_marker (hunk_idx + 1)
  in
  let hunk_header =
    if cursor = HunkCursor (file_idx, hunk_idx) then cursor_style "%s" hunk_header_content
    else hunk_header_content
  in
  hunk_header :: code_lines

let render_file file cursor file_idx =
  let hunk_lines =
    match file.hunks_visibility with
    | Collapsed -> []
    | Expanded ->
        file.hunks
        |> List.mapi (fun hunk_idx hunk -> render_hunk hunk cursor file_idx hunk_idx)
        |> List.flatten
  in

  let is_file_included_marker =
    match file_lines_inclusion file with AllLines -> "x" | SomeLines -> "~" | NoLines -> " "
  in

  let file_line_content = Format.sprintf "[%s] %s" is_file_included_marker file.path in
  let file_line =
    if cursor = FileCursor file_idx then cursor_style "%s" file_line_content else file_line_content
  in
  file_line :: hunk_lines

let view model =
  let header =
    header "%s"
      "Use the arrow keys to navigate, space to select/unselect, c to confirm and ESC to exit."
  in
  let lines =
    model.files
    |> List.mapi (fun file_idx file -> render_file file model.cursor file_idx)
    |> List.flatten |> String.concat "\n"
  in
  (* and we send the UI for rendering! *)
  Format.sprintf {|%s

%s
|} header lines

let app = Minttea.app ~init ~update ~view ()
