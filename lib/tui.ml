open Notty
open Notty_unix

let last_item list = List.hd @@ List.rev list
let last_idx list = List.length list - 1

type visibility = Expanded | Collapsed [@@deriving show, eq]
type context_line = [ `Context of string ] [@@deriving show, eq]

type diff_line = [ `Diff of string * [ `added | `removed ] * [ `included | `notincluded ] ]
[@@deriving show, eq]

type line = [ context_line | diff_line ]

type line_with_context = { context : context_line list; diff_line : diff_line }
[@@deriving show, eq]

type hunk = {
  starting_line : int;
  context_snippet : string option;
  lines_visibility : visibility;
  lines : line_with_context list;
  post_context : context_line list;
}
[@@deriving show, eq]

type path = FilePath of string | ChangedPath of { old_path : string; new_path : string }
[@@deriving show, eq]

(* TODO: Do I want to support separating renaming and changes? If I do, the file itself has to support being included or excluded. *)
type file = { path : path; hunks_visibility : visibility; hunks : hunk list } [@@deriving show, eq]

type cursor = FileCursor of int | HunkCursor of int * int | LineCursor of int * int * int
[@@deriving show, eq]

type model = { files : file list; cursor : cursor } [@@deriving show, eq]
type lines_included = AllLines | SomeLines | NoLines

let hunk_diffs_included hunk =
  let all_diffs_included =
    hunk.lines
    |> List.for_all (fun diff ->
           match diff.diff_line with `Diff (_, _, `included) -> true | _ -> false)
  in
  let any_diff_included =
    hunk.lines
    |> List.exists (fun diff ->
           match diff.diff_line with `Diff (_, _, `included) -> true | _ -> false)
  in
  if all_diffs_included then AllLines else if any_diff_included then SomeLines else NoLines

let file_lines_included file =
  let all_lines_included =
    file.hunks
    |> List.map (fun hunk -> hunk_diffs_included hunk)
    |> List.for_all (fun hunk_lines_inclusion -> hunk_lines_inclusion = AllLines)
  in
  let any_line_included =
    file.hunks
    |> List.map (fun hunk -> hunk_diffs_included hunk)
    |> List.exists (fun hunk_lines_inclusion ->
           hunk_lines_inclusion = AllLines || hunk_lines_inclusion = SomeLines)
  in
  if all_lines_included then AllLines else if any_line_included then SomeLines else NoLines

let initial_model : model =
  {
    cursor = FileCursor 0;
    files =
      [
        {
          path = FilePath "src/TestMain";
          hunks_visibility = Collapsed;
          hunks =
            [
              {
                starting_line = 1;
                context_snippet = None;
                lines_visibility = Expanded;
                lines =
                  [
                    {
                      context = [ `Context "code" ];
                      diff_line = `Diff ("code 2", `removed, `included);
                    };
                    { context = []; diff_line = `Diff ("code 3", `added, `included) };
                  ];
                post_context = [ `Context "code 4" ];
              };
              {
                starting_line = 15;
                context_snippet = None;
                lines_visibility = Expanded;
                lines =
                  [
                    {
                      context = [ `Context "code 5"; `Context "code 6"; `Context "code 7" ];
                      diff_line = `Diff ("code 7.5", `removed, `included);
                    };
                    { context = []; diff_line = `Diff ("code 7.9", `added, `included) };
                  ];
                post_context = [ `Context "code 8" ];
              };
            ];
        };
        {
          path = FilePath "src/MainFile";
          hunks_visibility = Collapsed;
          hunks =
            [
              {
                starting_line = 1;
                context_snippet = None;
                lines_visibility = Expanded;
                lines =
                  [
                    {
                      context = [ `Context "code" ];
                      diff_line = `Diff ("code 2", `removed, `included);
                    };
                    { context = []; diff_line = `Diff ("code 3", `added, `included) };
                  ];
                post_context = [ `Context "code 4" ];
              };
              {
                starting_line = 20;
                context_snippet = None;
                lines_visibility = Expanded;
                lines =
                  [
                    {
                      context = [ `Context "code 5"; `Context "code 6"; `Context "code 7" ];
                      diff_line = `Diff ("code 7.5", `removed, `included);
                    };
                    { context = []; diff_line = `Diff ("code 7.9", `added, `included) };
                  ];
                post_context = [ `Context "code 8" ];
              };
            ];
        };
        {
          path = FilePath "src/YetAnotherFile";
          hunks_visibility = Collapsed;
          hunks =
            [
              {
                starting_line = 1;
                context_snippet = None;
                lines_visibility = Expanded;
                lines =
                  [
                    { context = []; diff_line = `Diff ("These", `added, `included) };
                    { context = []; diff_line = `Diff ("Are", `added, `included) };
                    { context = []; diff_line = `Diff ("All", `added, `included) };
                    { context = []; diff_line = `Diff ("Added", `added, `included) };
                    { context = []; diff_line = `Diff ("Lines", `added, `included) };
                    { context = []; diff_line = `Diff ("Because", `added, `included) };
                    { context = []; diff_line = `Diff ("This", `added, `included) };
                    { context = []; diff_line = `Diff ("Is", `added, `included) };
                    { context = []; diff_line = `Diff ("A", `added, `included) };
                    { context = []; diff_line = `Diff ("New", `added, `included) };
                    { context = []; diff_line = `Diff ("File", `added, `included) };
                  ];
                post_context = [];
              };
            ];
        };
        {
          path = ChangedPath { old_path = "lib/nottui_tui.ml"; new_path = "lib/nottuiTui.ml" };
          hunks_visibility = Collapsed;
          hunks =
            [
              {
                starting_line = 1;
                context_snippet = None;
                lines_visibility = Expanded;
                lines =
                  [
                    {
                      context = [ `Context "code" ];
                      diff_line = `Diff ("code 2", `removed, `included);
                    };
                    { context = []; diff_line = `Diff ("code 3", `added, `included) };
                  ];
                post_context = [ `Context "code 4" ];
              };
            ];
        };
      ];
  }

exception Illegal_state of string

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
                    let line_idx = last_idx hunk.lines in
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
                    let line_idx = last_idx last_hunk.lines in
                    LineCursor (c_file_idx - 1, last_idx prev_file.hunks, line_idx)))
        | HunkCursor (c_file_idx, 0) -> FileCursor c_file_idx
        | HunkCursor (c_file_idx, c_hunk_idx) -> (
            let file = List.nth model.files c_file_idx in
            let prev_hunk = List.nth file.hunks (c_hunk_idx - 1) in
            match prev_hunk.lines_visibility with
            | Collapsed -> HunkCursor (c_file_idx, c_hunk_idx - 1)
            | Expanded ->
                let line_idx = last_idx prev_hunk.lines in
                LineCursor (c_file_idx, c_hunk_idx - 1, line_idx))
        | LineCursor (c_file_idx, c_hunk_idx, 0) -> HunkCursor (c_file_idx, c_hunk_idx)
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx) ->
            LineCursor (c_file_idx, c_hunk_idx, c_line_idx - 1)
      in
      { model with cursor }
  | `Key (`Arrow `Down, _) | `Key (`ASCII 'j', _) ->
      let last_file_idx = last_idx model.files in
      let cursor =
        match model.cursor with
        | FileCursor c_file_idx when c_file_idx = last_file_idx -> (
            let last_file = last_item model.files in
            match last_file.hunks_visibility with
            | Expanded -> HunkCursor (c_file_idx, 0)
            | Collapsed -> FileCursor 0)
        | FileCursor c_idx -> (
            let file = List.nth model.files c_idx in
            match file.hunks_visibility with
            | Expanded -> HunkCursor (c_idx, 0)
            | Collapsed -> FileCursor (c_idx + 1))
        | HunkCursor (c_file_idx, c_hunk_idx)
          when c_hunk_idx = last_idx (List.nth model.files c_file_idx).hunks -> (
            let file = List.nth model.files c_file_idx in
            let hunk = List.nth file.hunks c_hunk_idx in
            match hunk.lines_visibility with
            | Expanded -> LineCursor (c_file_idx, c_hunk_idx, 0)
            | Collapsed ->
                if c_file_idx = last_file_idx then FileCursor 0 else FileCursor (c_file_idx + 1))
        | HunkCursor (c_file_idx, c_hunk_idx) -> (
            let file = List.nth model.files c_file_idx in
            let hunk = List.nth file.hunks c_hunk_idx in
            match hunk.lines_visibility with
            | Expanded -> LineCursor (c_file_idx, c_hunk_idx, 0)
            | Collapsed -> HunkCursor (c_file_idx, c_hunk_idx + 1))
        (* Cursor is at last file -> last hunk -> last line. *)
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx)
          when c_file_idx = last_file_idx
               && c_hunk_idx = last_idx (List.nth model.files c_file_idx).hunks
               && c_line_idx
                  = last_idx (List.nth (List.nth model.files c_file_idx).hunks c_hunk_idx).lines ->
            FileCursor 0
        (* Cursor is at some file -> last hunk -> last line. *)
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx)
          when c_hunk_idx = last_idx (List.nth model.files c_file_idx).hunks
               && c_line_idx
                  = last_idx (List.nth (List.nth model.files c_file_idx).hunks c_hunk_idx).lines ->
            FileCursor (c_file_idx + 1)
        (* Cursor is at some file -> some hunk -> last line. *)
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx)
          when c_line_idx
               = last_idx (List.nth (List.nth model.files c_file_idx).hunks c_hunk_idx).lines ->
            HunkCursor (c_file_idx, c_hunk_idx + 1)
        (* Cursor is at some file -> some hunk -> some line. *)
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx) ->
            LineCursor (c_file_idx, c_hunk_idx, c_line_idx + 1)
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
                                |> List.map (fun diff ->
                                       match diff.diff_line with
                                       | `Diff (content, added, _) ->
                                           let diff_line =
                                             `Diff (content, added, included_in_diff)
                                           in
                                           { diff with diff_line })
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
                              let hunk_lines_inclusion = hunk_diffs_included hunk in
                              let included_in_diff = toggle_group hunk_lines_inclusion in
                              let lines =
                                if hunk_idx <> c_hunk_idx then hunk.lines
                                else
                                  hunk.lines
                                  |> List.map (fun diff ->
                                         match diff.diff_line with
                                         | `Diff (content, added, _) ->
                                             let diff_line =
                                               `Diff (content, added, included_in_diff)
                                             in
                                             { diff with diff_line })
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
                                  |> List.mapi (fun line_idx diff ->
                                         if line_idx = c_line_idx then diff
                                         else
                                           let diff_line =
                                             match diff.diff_line with
                                             | `Diff (content, added, included) ->
                                                 `Diff (content, added, toggle_line included)
                                           in
                                           { diff with diff_line })
                              in
                              { hunk with lines })
                   in
                   { file with hunks })
      in
      { model with files }
  | _ -> model

let render_individual_line line is_cursor =
  let style = if is_cursor then A.st A.reverse else A.empty in

  let included_prefix =
    match line with
    | `Context _ -> "   "
    | `Diff (_, _, `included) -> "[x]"
    | `Diff (_, _, `notincluded) -> "[ ]"
  in

  let line_kind_prefix =
    match line with
    | `Context _ -> " "
    | `Diff (_, `removed, _) -> "-"
    | `Diff (_, `added, _) -> "+"
  in

  let line_content =
    match line with `Context content -> content | `Diff (content, _, _) -> content
  in

  let colour =
    match line with
    | `Context _ -> A.gray 10
    | `Diff (_, `added, _) -> A.green
    | `Diff (_, `removed, _) -> A.red
  in

  I.strf ~attr:A.(fg colour ++ style) "%s %s %s" included_prefix line_kind_prefix line_content
  |> I.hpad 4 0

let render_line_with_context line cursor file_idx hunk_idx line_idx =
  let is_cursor = cursor = LineCursor (file_idx, hunk_idx, line_idx) in

  let rendered_context_lines =
    line.context |> List.map (fun line -> render_individual_line line false)
  in
  let rendered_line = render_individual_line line.diff_line is_cursor in

  rendered_context_lines @ [ rendered_line ]

let render_hunk hunk cursor file_idx hunk_idx : image list =
  let lines =
    match hunk.lines_visibility with
    | Collapsed -> []
    | Expanded ->
        hunk.lines
        |> List.mapi (fun line_idx line_with_context ->
               render_line_with_context line_with_context cursor file_idx hunk_idx line_idx)
        |> List.flatten
  in

  let style = if cursor = HunkCursor (file_idx, hunk_idx) then A.st A.reverse else A.empty in
  let hunk_included_marker =
    match hunk_diffs_included hunk with AllLines -> "x" | SomeLines -> "~" | NoLines -> " "
  in
  let hunk_line =
    I.strf ~attr:style "[%s] Hunk %d" hunk_included_marker (hunk_idx + 1) |> I.hpad 2 0
  in
  hunk_line :: lines

let render_file file cursor file_idx : image list =
  let hunk_lines =
    match file.hunks_visibility with
    | Collapsed -> []
    | Expanded ->
        file.hunks
        |> List.mapi (fun hunk_idx hunk -> render_hunk hunk cursor file_idx hunk_idx)
        |> List.flatten
  in

  let style = if cursor = FileCursor file_idx then A.st A.reverse else A.empty in
  let file_included_marker =
    match file_lines_included file with AllLines -> "x" | SomeLines -> "~" | NoLines -> " "
  in
  let file_line =
    match file.path with
    | FilePath path -> I.strf ~attr:style "[%s] %s" file_included_marker path
    | ChangedPath changed_path ->
        I.strf
          ~attr:A.(fg yellow ++ style)
          "[%s] %s -> %s" file_included_marker changed_path.old_path changed_path.new_path
  in
  file_line :: hunk_lines

let with_cursor files : cursor list =
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
                            |> List.mapi (fun line_idx _ ->
                                   LineCursor (file_idx, hunk_idx, line_idx))
                      in
                      [ HunkCursor (file_idx, hunk_idx) ] @ lines)
               |> List.flatten
         in
         [ FileCursor file_idx ] @ hunks)
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
  | `Key (`ASCII 'c', _) -> (* TODO: Do not allow this if nothing is selected. *) Some state
  | `Key (`Escape, _) | `Key (`ASCII 'q', _) -> None
  | event ->
      let new_state = update event state in
      ui_loop t new_state

let run initial_model =
  let term = Term.create ~dispose:true ~mouse:false () in
  let final_model = ui_loop term initial_model in
  Term.release term;
  final_model

let tui_diff_lines diff_lines =
  let rec diff_lines_with_context context = function
    | [] -> invalid_arg ""
    | `ContextLine _ :: [] -> []
    | `RemovedLine content :: [] ->
        [ { context; diff_line = `Diff (content, `removed, `included) } ]
    | `AddedLine content :: [] -> [ { context; diff_line = `Diff (content, `added, `included) } ]
    | `ContextLine content :: tl -> diff_lines_with_context (`Context content :: context) tl
    | `RemovedLine content :: tl ->
        let diff_line = { context; diff_line = `Diff (content, `removed, `included) } in
        diff_line :: diff_lines_with_context [] tl
    | `AddedLine content :: tl ->
        let diff_line = { context; diff_line = `Diff (content, `added, `included) } in
        diff_line :: diff_lines_with_context [] tl
  in
  diff_lines_with_context [] diff_lines

let tui_dangling_context diff_lines =
  let rec dangling_context context = function
    | [] -> context
    | `AddedLine _ :: _ | `RemovedLine _ :: _ -> context
    | `ContextLine content :: tl -> dangling_context (`Context content :: context) tl
  in
  dangling_context [] (List.rev diff_lines)

let model_of_diff (diff : Diff.diff) =
  let files =
    diff.files
    |> List.map (fun (file : Diff.file) : file ->
           match file with
           | Diff.DeletedFile deleted_file ->
               let lines = deleted_file.lines |> tui_diff_lines in
               {
                 path = FilePath deleted_file.path;
                 hunks =
                   [
                     {
                       starting_line = 1;
                       context_snippet = None;
                       lines_visibility = Expanded;
                       lines;
                       post_context = [];
                     };
                   ];
                 hunks_visibility = Collapsed;
               }
           | Diff.CreatedFile created_file ->
               let lines = created_file.lines |> tui_diff_lines in
               {
                 path = FilePath created_file.path;
                 hunks =
                   [
                     {
                       starting_line = 1;
                       context_snippet = None;
                       lines_visibility = Expanded;
                       lines;
                       post_context = [];
                     };
                   ];
                 hunks_visibility = Collapsed;
               }
           | Diff.ChangedFile changed_file ->
               let hunks =
                 changed_file.hunks
                 |> List.map (fun (hunk : Diff.hunk) : hunk ->
                        let lines = hunk.lines |> tui_diff_lines in
                        let post_context = tui_dangling_context hunk.lines in
                        {
                          starting_line = hunk.starting_line;
                          context_snippet = hunk.context_snippet;
                          lines_visibility = Expanded;
                          lines;
                          post_context;
                        })
               in
               { path = FilePath changed_file.path; hunks; hunks_visibility = Collapsed }
           | Diff.RenamedFile renamed_file ->
               let hunks =
                 renamed_file.hunks
                 |> List.map (fun (hunk : Diff.hunk) : hunk ->
                        let lines = hunk.lines |> tui_diff_lines in
                        let post_context = tui_dangling_context hunk.lines in
                        {
                          starting_line = hunk.starting_line;
                          context_snippet = hunk.context_snippet;
                          lines_visibility = Expanded;
                          lines;
                          post_context;
                        })
               in
               {
                 path =
                   ChangedPath
                     { old_path = renamed_file.old_path; new_path = renamed_file.new_path };
                 hunks;
                 hunks_visibility = Collapsed;
               })
  in
  { files; cursor = FileCursor 0 }

let map_diff_line = function
  | `Diff (content, `added, `included) -> Some (`AddedLine content)
  | `Diff (_, `added, `notincluded) -> None
  | `Diff (content, `removed, `included) -> Some (`RemovedLine content)
  | `Diff (content, `removed, `notincluded) -> Some (`ContextLine content)

let map_context = function `Context content -> `ContextLine content

let map_diff diff =
  let context_lines = diff.context |> List.map map_context in
  let diff_line =
    diff.diff_line |> map_diff_line |> Option.fold ~none:[] ~some:(fun diff_line -> [ diff_line ])
  in
  context_lines @ diff_line

let diff_of_model model : Diff.diff =
  let files =
    model.files
    |> List.filter (fun file -> file_lines_included file <> NoLines)
    |> List.map (fun file ->
           let is_created_file =
             List.length file.hunks = 1
             &&
             let hunk = List.hd file.hunks in
             hunk.starting_line = 1
             &&
             let hunk = List.hd file.hunks in
             hunk.lines
             |> List.for_all (fun diff ->
                    match diff.diff_line with `Diff (_, `added, _) -> true | _ -> false)
           in
           let is_deleted_file =
             List.length file.hunks = 1
             &&
             let hunk = List.hd file.hunks in
             hunk.starting_line = 1
             &&
             let hunk = List.hd file.hunks in
             hunk.lines
             |> List.for_all (fun diff ->
                    match diff.diff_line with `Diff (_, `removed, _) -> true | _ -> false)
           in
           let hunks =
             file.hunks
             |> List.filter (fun hunk -> hunk_diffs_included hunk <> NoLines)
             |> List.map (fun (hunk : hunk) : Diff.hunk ->
                    let diffs_lines = hunk.lines |> List.map map_diff |> List.flatten in
                    let trailing_context_lines = hunk.post_context |> List.map map_context in
                    {
                      starting_line = hunk.starting_line;
                      context_snippet = hunk.context_snippet;
                      lines = diffs_lines @ trailing_context_lines;
                    })
           in
           if is_created_file then
             let hunk = List.hd file.hunks in
             let lines =
               hunk.lines |> List.map map_diff |> List.flatten
               |> List.filter_map (fun line ->
                      match line with `AddedLine content -> Some (`AddedLine content) | _ -> None)
             in
             let path =
               match file.path with
               | FilePath path -> path
               | _ -> raise (Illegal_state "created file cannot have changed path")
             in
             Diff.CreatedFile { path; lines }
           else if is_deleted_file then
             let hunk = List.hd file.hunks in
             let lines =
               hunk.lines |> List.map map_diff |> List.flatten
               |> List.filter_map (fun line ->
                      match line with
                      | `RemovedLine content -> Some (`RemovedLine content)
                      | _ -> None)
             in
             let path =
               match file.path with
               | FilePath path -> path
               | _ -> raise (Illegal_state "deleted file cannot have changed path")
             in
             Diff.DeletedFile { path; lines }
           else
             match file.path with
             | FilePath path -> Diff.ChangedFile { path; hunks }
             | ChangedPath changed_path ->
                 Diff.RenamedFile
                   { old_path = changed_path.old_path; new_path = changed_path.new_path; hunks })
  in
  { files }
