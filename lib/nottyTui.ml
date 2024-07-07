open Notty
open Notty_unix

let last_item list = List.hd @@ List.rev list
let last_idx list = List.length list - 1

type visibility = Expanded | Collapsed

type line =
  | Context of string
  | Diff of string * [ `added | `removed ] * [ `included | `notincluded ]

type hunk = { lines : line list; lines_visibility : visibility }
type changed_file = { path : string; hunks : hunk list; hunks_visibility : visibility }

type renamed_file = {
  old_path : string;
  new_path : string;
  included : [ `included | `notincluded ];
}

type file = RenamedFile of renamed_file | ChangedFile of changed_file
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
        ChangedFile
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
        ChangedFile
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
        ChangedFile
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
        RenamedFile
          { old_path = "lib/nottui_tui.ml"; new_path = "lib/nottuiTui.ml"; included = `included };
      ];
  }

exception Illegal_state of string

let get_changed_file_exn = function
  | RenamedFile _ -> raise (Illegal_state "Hunk cursor cannot be on a renamed file.")
  | ChangedFile file -> file

let update event model =
  match event with
  | `Key (`Arrow `Up, _) | `Key (`ASCII 'k', _) ->
      let cursor =
        match model.cursor with
        | FileCursor 0 -> (
            let last_file_idx = last_idx model.files in
            let last_file = last_item model.files in
            match last_file with
            | RenamedFile _ -> FileCursor last_file_idx
            | ChangedFile changed_file -> (
                match changed_file.hunks_visibility with
                | Collapsed -> FileCursor last_file_idx
                | Expanded -> (
                    let last_hunk = last_item changed_file.hunks in
                    match last_hunk.lines_visibility with
                    | Collapsed -> HunkCursor (last_file_idx, last_idx changed_file.hunks)
                    | Expanded ->
                        let hunk = last_item changed_file.hunks in
                        let line_idx = find_last_diff_line_idx hunk.lines in
                        LineCursor (last_file_idx, last_idx changed_file.hunks, line_idx))))
        | FileCursor c_file_idx -> (
            let prev_file = List.nth model.files (c_file_idx - 1) in
            match prev_file with
            | RenamedFile _ -> FileCursor (c_file_idx - 1)
            | ChangedFile changed_file -> (
                match changed_file.hunks_visibility with
                | Collapsed -> FileCursor (c_file_idx - 1)
                | Expanded -> (
                    let last_hunk = last_item changed_file.hunks in
                    match last_hunk.lines_visibility with
                    | Collapsed -> HunkCursor (c_file_idx - 1, last_idx changed_file.hunks)
                    | Expanded ->
                        let line_idx = find_last_diff_line_idx last_hunk.lines in
                        LineCursor (c_file_idx - 1, last_idx changed_file.hunks, line_idx))))
        | HunkCursor (c_file_idx, 0) -> FileCursor c_file_idx
        | HunkCursor (c_file_idx, c_hunk_idx) -> (
            let file = get_changed_file_exn @@ List.nth model.files c_file_idx in
            let prev_hunk = List.nth file.hunks (c_hunk_idx - 1) in
            match prev_hunk.lines_visibility with
            | Collapsed -> HunkCursor (c_file_idx, c_hunk_idx - 1)
            | Expanded ->
                let line_idx = find_last_diff_line_idx prev_hunk.lines in
                LineCursor (c_file_idx, c_hunk_idx - 1, line_idx))
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx) -> (
            let file = get_changed_file_exn @@ List.nth model.files c_file_idx in
            let hunk = List.nth file.hunks c_hunk_idx in
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
            match last_file with
            | RenamedFile _ -> FileCursor 0
            | ChangedFile changed_file -> (
                match changed_file.hunks_visibility with
                | Collapsed -> FileCursor 0
                | Expanded -> HunkCursor (c_file_idx, 0)))
        | FileCursor c_idx -> (
            let file = List.nth model.files c_idx in
            match file with
            | RenamedFile _ -> FileCursor (c_idx + 1)
            | ChangedFile changed_file -> (
                match changed_file.hunks_visibility with
                | Collapsed -> FileCursor (c_idx + 1)
                | Expanded -> HunkCursor (c_idx, 0)))
        | HunkCursor (c_file_idx, c_hunk_idx)
          when c_hunk_idx = last_idx (get_changed_file_exn (List.nth model.files c_file_idx)).hunks
          -> (
            let file = get_changed_file_exn (List.nth model.files c_file_idx) in
            let hunk = List.nth file.hunks c_hunk_idx in
            match hunk.lines_visibility with
            | Expanded ->
                let first_diff_line = find_first_diff_line_idx hunk.lines in
                LineCursor (c_file_idx, c_hunk_idx, first_diff_line)
            | Collapsed ->
                if c_file_idx = last_file_idx then FileCursor 0 else FileCursor (c_file_idx + 1))
        | HunkCursor (c_file_idx, c_hunk_idx) -> (
            let file = get_changed_file_exn (List.nth model.files c_file_idx) in
            let hunk = List.nth file.hunks c_hunk_idx in
            match hunk.lines_visibility with
            | Collapsed -> HunkCursor (c_file_idx, c_hunk_idx + 1)
            | Expanded ->
                let first_diff_line = find_first_diff_line_idx hunk.lines in
                LineCursor (c_file_idx, c_hunk_idx, first_diff_line))
        (* Cursor is at last file -> last hunk -> line. *)
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx)
          when c_file_idx = last_file_idx
               && c_hunk_idx
                  = last_idx (get_changed_file_exn (List.nth model.files c_file_idx)).hunks -> (
            let file = get_changed_file_exn (List.nth model.files c_file_idx) in
            let hunk = List.nth file.hunks c_hunk_idx in
            let curr_line = List.nth hunk.lines c_line_idx in
            let next_diff_line_idx = find_next_diff_line_idx hunk.lines curr_line in
            match next_diff_line_idx with
            | None -> FileCursor 0
            | Some idx -> LineCursor (c_file_idx, c_hunk_idx, idx))
        (* Cursor is at some file -> last hunk -> line. *)
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx)
          when c_hunk_idx = last_idx (get_changed_file_exn (List.nth model.files c_file_idx)).hunks
          -> (
            let file = get_changed_file_exn (List.nth model.files c_file_idx) in
            let hunk = List.nth file.hunks c_hunk_idx in
            let curr_line = List.nth hunk.lines c_line_idx in
            let next_diff_line_idx = find_next_diff_line_idx hunk.lines curr_line in
            match next_diff_line_idx with
            | None -> FileCursor (c_file_idx + 1)
            | Some idx -> LineCursor (c_file_idx, c_hunk_idx, idx))
        (* General some line case. *)
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx) -> (
            let file = get_changed_file_exn (List.nth model.files c_file_idx) in
            let hunk = List.nth file.hunks c_hunk_idx in
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
                   match file with
                   | RenamedFile _ -> file
                   | ChangedFile changed_file ->
                       let hunks_visibility =
                         if file_idx = c_file_idx then Expanded else changed_file.hunks_visibility
                       in
                       ChangedFile { changed_file with hunks_visibility })
          in
          { model with files }
      | HunkCursor (c_file_idx, c_hunk_idx) ->
          let files =
            model.files
            |> List.mapi (fun file_idx file ->
                   match file with
                   | RenamedFile _ -> file
                   | ChangedFile changed_file ->
                       let hunks =
                         changed_file.hunks
                         |> List.mapi (fun hunk_idx hunk ->
                                let lines_visibility =
                                  if hunk_idx = c_hunk_idx && file_idx = c_file_idx then Expanded
                                  else hunk.lines_visibility
                                in
                                { hunk with lines_visibility })
                       in
                       ChangedFile { changed_file with hunks })
          in
          { model with files }
      | LineCursor _ -> model)
  | `Key (`Arrow `Left, _) | `Key (`ASCII 'h', _) -> (
      match model.cursor with
      | FileCursor c_file_idx ->
          let files =
            model.files
            |> List.mapi (fun file_idx file ->
                   match file with
                   | RenamedFile _ -> file
                   | ChangedFile changed_file ->
                       let hunks_visibility =
                         if file_idx = c_file_idx then Collapsed else changed_file.hunks_visibility
                       in
                       ChangedFile { changed_file with hunks_visibility })
          in
          { model with files }
      | HunkCursor (c_file_idx, c_hunk_idx) ->
          let files =
            model.files
            |> List.mapi (fun file_idx file ->
                   match file with
                   | RenamedFile _ -> file
                   | ChangedFile changed_file ->
                       let hunks =
                         changed_file.hunks
                         |> List.mapi (fun hunk_idx hunk ->
                                let lines_visibility =
                                  if hunk_idx = c_hunk_idx && file_idx = c_file_idx then Collapsed
                                  else hunk.lines_visibility
                                in
                                { hunk with lines_visibility })
                       in
                       ChangedFile { changed_file with hunks })
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
                   match file with
                   | RenamedFile renamed_file ->
                       if file_idx <> c_file_idx then file
                       else
                         let included = toggle_line renamed_file.included in
                         RenamedFile { renamed_file with included }
                   | ChangedFile changed_file ->
                       let hunks =
                         if file_idx <> c_file_idx then changed_file.hunks
                         else
                           let file_lines_inclusion = file_lines_included changed_file in
                           let included_in_diff = toggle_group file_lines_inclusion in
                           changed_file.hunks
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
                       ChangedFile { changed_file with hunks })
        | HunkCursor (c_file_idx, c_hunk_idx) ->
            model.files
            |> List.mapi (fun file_idx file ->
                   match file with
                   | RenamedFile _ -> file
                   | ChangedFile changed_file ->
                       let hunks =
                         if file_idx <> c_file_idx then changed_file.hunks
                         else
                           changed_file.hunks
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
                       ChangedFile { changed_file with hunks })
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx) ->
            model.files
            |> List.mapi (fun file_idx file ->
                   match file with
                   | RenamedFile renamed_file -> RenamedFile renamed_file
                   | ChangedFile changed_file ->
                       let hunks =
                         if file_idx <> c_file_idx then changed_file.hunks
                         else
                           changed_file.hunks
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
                       ChangedFile { changed_file with hunks })
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

let render_renamed_file file cursor file_idx =
  let style = if cursor = FileCursor file_idx then A.st A.reverse else A.empty in
  let file_included_marker = match file.included with `included -> "x" | `notincluded -> " " in
  let file_line =
    I.strf
      ~attr:A.(fg yellow ++ style)
      "[%s] %s -> %s" file_included_marker file.old_path file.new_path
  in
  [ file_line ]

let render_changed_file file cursor file_idx =
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

let render_file file cursor file_idx =
  match file with
  | RenamedFile renamed_file -> render_renamed_file renamed_file cursor file_idx
  | ChangedFile changed_file -> render_changed_file changed_file cursor file_idx

let with_cursor files =
  files
  |> List.mapi (fun file_idx file ->
         match file with
         | RenamedFile _ -> [ FileCursor file_idx ]
         | ChangedFile changed_file ->
             let hunks =
               match changed_file.hunks_visibility with
               | Collapsed -> []
               | Expanded ->
                   changed_file.hunks
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
    |> List.mapi (fun file_idx file -> render_file file model.cursor file_idx)
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

let tui_line_of_diff_line = function
  | Diff.UnchangedLine content -> Context content
  | Diff.RemovedLine content -> Diff (content, `removed, `included)
  | Diff.AddedLine content -> Diff (content, `added, `included)

let model_of_diff (diff : Diff.diff) =
  let files =
    diff.files
    |> List.map (fun (file : Diff.diff_file) : file ->
           match file with
           | Diff.RenamedFile renamed_file ->
               RenamedFile
                 {
                   old_path = renamed_file.old_path;
                   new_path = renamed_file.new_path;
                   included = `included;
                 }
           | Diff.DiffFile changed_file ->
               let hunks =
                 changed_file.hunks
                 |> List.map (fun (hunk : Diff.hunk) : hunk ->
                        let lines =
                          hunk.lines |> List.map (fun line -> tui_line_of_diff_line line)
                        in
                        { lines; lines_visibility = Expanded })
               in
               ChangedFile { hunks; hunks_visibility = Collapsed; path = changed_file.path })
  in
  { files; cursor = FileCursor 0 }

let diff_of_model model : Diff.diff =
  let files =
    model.files
    |> List.filter (fun file ->
           match file with
           | RenamedFile renamed_file -> renamed_file.included = `included
           | ChangedFile changed_file -> file_lines_included changed_file <> NoLines)
    |> List.map (fun file ->
           match file with
           | ChangedFile changed_file ->
               let hunks =
                 changed_file.hunks
                 |> List.filter (fun hunk -> hunk_lines_included hunk <> NoLines)
                 |> List.map (fun (hunk : hunk) : Diff.hunk ->
                        let lines =
                          hunk.lines
                          |> List.filter (fun (line : line) ->
                                 match line with Diff (_, _, `included) -> false | _ -> true)
                          |> List.map (fun line ->
                                 match line with
                                 | Context content -> Diff.UnchangedLine content
                                 | Diff (content, `removed, _) -> Diff.RemovedLine content
                                 | Diff (content, `added, _) -> Diff.AddedLine content)
                        in
                        { lines })
               in
               Diff.DiffFile { path = changed_file.path; hunks }
           | RenamedFile { old_path; new_path; _ } ->
               Diff.RenamedFile { old_path; new_path; hunks = [] })
  in
  { files }
