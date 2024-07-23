open Notty
open Notty_unix

let last_item list = List.hd @@ List.rev list
let last_idx list = List.length list - 1

type visibility = Expanded | Collapsed [@@deriving show, eq]
type context_line = string [@@deriving show, eq]
type diff_line = string * [ `added | `removed ] * [ `included | `notincluded ] [@@deriving show, eq]
type line = Context of context_line | Diff of diff_line

type line_list =
  | Last of (diff_line * context_line list)
  | ( :: ) of (context_line list * diff_line) * line_list
[@@deriving show, eq]

let rec last_index = function Last _ -> 0 | ( :: ) (_, tl) -> 1 + last_index tl

let rec nth idx line_list =
  match line_list with
  | Last item -> if idx = 0 then Last item else invalid_arg "line_list.nth"
  | ( :: ) (item, tl) -> if idx = 0 then ( :: ) (item, tl) else nth (idx - 1) tl

type hunk = {
  starting_line : int;
  context_snippet : string option;
  lines : line_list;
  lines_visibility : visibility;
}
[@@deriving show, eq]

type path = FilePath of string | ChangedPath of { old_path : string; new_path : string }
[@@deriving show, eq]

(* TODO: Do I want to support separating renaming and changes? If I do, the file itself has to support being included or excluded. *)
type file = { path : path; hunks : hunk list; hunks_visibility : visibility } [@@deriving show, eq]

type cursor = FileCursor of int | HunkCursor of int * int | LineCursor of int * int * int
[@@deriving show, eq]

type model = { files : file list; cursor : cursor } [@@deriving show, eq]
type lines_included = AllLines | SomeLines | NoLines

let rec all_included = function
  | Last ((_, _, `included), _) -> true
  | ( :: ) ((_, (_, _, `included)), tl) -> all_included tl
  | _ -> false

let rec any_included = function
  | Last ((_, _, `included), _) -> true
  | Last _ -> false
  | ( :: ) ((_, (_, _, `included)), _) -> true
  | ( :: ) (_, tl) -> any_included tl

let hunk_lines_included hunk =
  let all_lines_included = all_included hunk.lines in
  let any_line_included = any_included hunk.lines in
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

let initial_model =
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
                lines =
                  ([ "code" ], ("code 2", `removed, `included))
                  :: Last (("code 3", `added, `included), [ "code 4" ]);
                lines_visibility = Expanded;
              };
              {
                starting_line = 15;
                context_snippet = None;
                lines =
                  ([ "code 5"; "code 6"; "code 7" ], ("code 7.5", `removed, `included))
                  :: Last (("code 7.9", `added, `included), [ "code 8" ]);
                lines_visibility = Expanded;
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
                lines =
                  ([ "code" ], ("code 2", `removed, `included))
                  :: Last (("code 3", `added, `included), [ "code 4" ]);
                lines_visibility = Expanded;
              };
              {
                starting_line = 20;
                context_snippet = None;
                lines =
                  ([ "code 5"; "code 6"; "code 7" ], ("code 7.5", `removed, `included))
                  :: Last (("code 7.9", `added, `included), [ "code 8" ]);
                lines_visibility = Expanded;
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
                lines =
                  ([], ("These", `added, `included))
                  :: ([], ("Are", `added, `included))
                  :: ([], ("All", `added, `included))
                  :: ([], ("Added", `added, `included))
                  :: ([], ("Lines", `added, `included))
                  :: ([], ("Because", `added, `included))
                  :: ([], ("This", `added, `included))
                  :: ([], ("Is", `added, `included))
                  :: ([], ("A", `added, `included))
                  :: ([], ("New", `added, `included))
                  :: Last (("File", `added, `included), []);
                lines_visibility = Expanded;
                context_snippet = None;
              };
            ];
        };
        {
          path = ChangedPath { old_path = "lib/nottui_tui.ml"; new_path = "lib/nottuiTui.ml" };
          hunks =
            [
              {
                starting_line = 1;
                context_snippet = None;
                lines =
                  ([ "code" ], ("code 2", `removed, `included))
                  :: Last (("code 3", `added, `included), [ "code 4" ]);
                lines_visibility = Expanded;
              };
            ];
          hunks_visibility = Collapsed;
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
                    let line_idx = last_index hunk.lines in
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
                    let line_idx = last_index last_hunk.lines in
                    LineCursor (c_file_idx - 1, last_idx prev_file.hunks, line_idx)))
        | HunkCursor (c_file_idx, 0) -> FileCursor c_file_idx
        | HunkCursor (c_file_idx, c_hunk_idx) -> (
            let file = List.nth model.files c_file_idx in
            let prev_hunk = List.nth file.hunks (c_hunk_idx - 1) in
            match prev_hunk.lines_visibility with
            | Collapsed -> HunkCursor (c_file_idx, c_hunk_idx - 1)
            | Expanded ->
                let line_idx = last_index prev_hunk.lines in
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
               && c_hunk_idx = last_idx (model.files |> List.rev |> List.hd).hunks
               && c_line_idx
                  = last_index
                      ((model.files |> List.rev |> List.hd).hunks |> List.rev |> List.hd).lines ->
            FileCursor 0
        (* Cursor is at some file -> last hunk -> last line. *)
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx)
          when c_hunk_idx = last_idx (List.nth model.files c_file_idx).hunks
               && c_line_idx
                  = last_index
                      ((List.nth model.files c_file_idx).hunks |> List.rev |> List.hd).lines ->
            FileCursor (c_file_idx + 1)
        (* Cursor is at some file -> some hunk -> last line. *)
        | LineCursor (c_file_idx, c_hunk_idx, c_line_idx)
          when c_line_idx
               = last_index
                   ((model.files |> List.rev |> List.hd).hunks |> List.rev |> List.hd).lines ->
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
      let toggle_included include_status =
        match include_status with `included -> `notincluded | `notincluded -> `included
      in

      let toggle_group = function
        | NoLines -> `included
        | SomeLines -> `included
        | AllLines -> `notincluded
      in

      let rec toggle_line_inclusion inclusion = function
        | Last ((content, added, _), context_lines) ->
            Last ((content, added, inclusion), context_lines)
        | ( :: ) ((context, (content, added, _)), tl) ->
            ( :: ) ((context, (content, added, inclusion)), toggle_line_inclusion inclusion tl)
      in

      let rec toggle_line idx = function
        | Last ((content, added, included), context_lines) ->
            if idx = 0 then Last ((content, added, toggle_included included), context_lines)
            else invalid_arg "fuck_line"
        | ( :: ) (((context, (content, added, inclusion)) as t), tl) ->
            if idx = 0 then ( :: ) ((context, (content, added, toggle_included inclusion)), tl)
            else ( :: ) (t, toggle_line (idx - 1) tl)
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
                              let lines = toggle_line_inclusion included_in_diff hunk.lines in
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
                                else toggle_line_inclusion included_in_diff hunk.lines
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
                                else toggle_line c_line_idx hunk.lines
                              in
                              { hunk with lines })
                   in
                   { file with hunks })
      in
      { model with files }
  | _ -> model

let render_line line is_cursor =
  let style = if is_cursor then A.st A.reverse else A.empty in

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

let rec render_lines line_list cursor file_idx hunk_idx line_idx : image list =
  let is_cursor = cursor = LineCursor (file_idx, hunk_idx, line_idx) in
  match line_list with
  | Last (diff, context) ->
      let context_images =
        context
        |> List.map (fun c -> Context c)
        |> List.map (fun context_line -> render_line context_line false)
      in
      let diff_image = render_line (Diff diff) is_cursor in
      diff_image :: context_images
  | ( :: ) ((context, diff), tl) ->
      let context_images =
        context
        |> List.map (fun c -> Context c)
        |> List.map (fun context_line -> render_line context_line false)
      in
      let diff_image = render_line (Diff diff) is_cursor in
      context_images @ (diff_image :: render_lines tl cursor file_idx hunk_idx (line_idx + 1))

let render_hunk hunk cursor file_idx hunk_idx : image list =
  let lines =
    match hunk.lines_visibility with
    | Collapsed -> []
    | Expanded -> render_lines hunk.lines cursor file_idx hunk_idx 0
  in

  let style = if cursor = HunkCursor (file_idx, hunk_idx) then A.st A.reverse else A.empty in
  let hunk_included_marker =
    match hunk_lines_included hunk with AllLines -> "x" | SomeLines -> "~" | NoLines -> " "
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

let rec line_cursor line_list line_idx : int list =
  match line_list with
  | Last _ -> [ line_idx ]
  | ( :: ) (_, tl) -> line_idx :: line_cursor tl (line_idx + 1)

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
                            let fuck = line_cursor hunk.lines 0 in
                            let foo = List.map (fun a -> LineCursor (file_idx, hunk_idx, a)) fuck in
                            foo
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

let tui_lines_of_diff_lines diff_lines =
  let rec foo diff_lines (diff, context) =
    match diff_lines with
    | [] -> invalid_arg ""
    | `ContextLine content :: [] ->
        let tail_context : context_line list = content :: context in
        Last (Option.get diff, List.rev tail_context)
    | `RemovedLine content :: [] ->
        (List.rev context, Option.get diff) :: Last ((content, `removed, `included), [])
    | `AddedLine content :: [] ->
        (List.rev context, Option.get diff) :: Last ((content, `added, `included), [])
    | `ContextLine content :: tl -> foo tl (diff, content :: context)
    | `RemovedLine content :: tl ->
        let item = (List.rev context, (content, `removed, `included)) in
        item :: foo tl (None, [])
    | `AddedLine content :: tl ->
        let item = (List.rev context, (content, `removed, `included)) in
        item :: foo tl (None, [])
  in
  foo diff_lines (None, [])

let model_of_diff (diff : Diff.diff) =
  let files =
    diff.files
    |> List.map (fun (file : Diff.file) : file ->
           match file with
           | Diff.DeletedFile deleted_file ->
               let lines = deleted_file.lines |> tui_lines_of_diff_lines in
               {
                 path = FilePath deleted_file.path;
                 hunks =
                   [
                     {
                       starting_line = 1;
                       context_snippet = None;
                       lines;
                       lines_visibility = Expanded;
                     };
                   ];
                 hunks_visibility = Collapsed;
               }
           | Diff.CreatedFile created_file ->
               let lines = created_file.lines |> tui_lines_of_diff_lines in
               {
                 path = FilePath created_file.path;
                 hunks =
                   [
                     {
                       starting_line = 1;
                       context_snippet = None;
                       lines;
                       lines_visibility = Expanded;
                     };
                   ];
                 hunks_visibility = Collapsed;
               }
           | Diff.ChangedFile changed_file ->
               let hunks =
                 changed_file.hunks
                 |> List.map (fun (hunk : Diff.hunk) : hunk ->
                        let lines = hunk.lines |> tui_lines_of_diff_lines in
                        {
                          starting_line = hunk.starting_line;
                          context_snippet = hunk.context_snippet;
                          lines;
                          lines_visibility = Expanded;
                        })
               in
               { path = FilePath changed_file.path; hunks; hunks_visibility = Collapsed }
           | Diff.RenamedFile renamed_file ->
               let hunks =
                 renamed_file.hunks
                 |> List.map (fun (hunk : Diff.hunk) : hunk ->
                        let lines = hunk.lines |> tui_lines_of_diff_lines in
                        {
                          starting_line = hunk.starting_line;
                          context_snippet = hunk.context_snippet;
                          lines;
                          lines_visibility = Expanded;
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

let rec all_lines_add = function
  | Last ((_, `removed, _), _) -> false
  | Last _ -> true
  | (_, (_, `removed, _)) :: _ -> false
  | _ :: tl -> all_lines_add tl

let rec all_lines_remove = function
  | Last ((_, `added, _), _) -> false
  | Last _ -> true
  | (_, (_, `added, _)) :: _ -> false
  | _ :: tl -> all_lines_remove tl

let to_context_line context : Diff.line = `ContextLine context

let rec map_lines line_list : Diff.line list =
  match line_list with
  | Last ((content, `added, `included), context) ->
      let tail_context_lines = List.map to_context_line context in
      `AddedLine content :: tail_context_lines
  | Last ((content, `removed, `included), context) ->
      let tail_context_lines = List.map to_context_line context in
      `RemovedLine content :: tail_context_lines
  | Last ((content, `removed, `notincluded), context) ->
      let tail_context_lines = List.map to_context_line context in
      `ContextLine content :: tail_context_lines
  | ( :: ) ((context, (content, `added, `included)), tl) ->
      let lines = List.map to_context_line context @ [ `AddedLine content ] in
      lines @ map_lines tl
  | ( :: ) ((context, (content, `removed, `included)), tl) ->
      let lines = List.map to_context_line context @ [ `RemovedLine content ] in
      lines @ map_lines tl
  | _ -> []

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
             hunk.lines |> all_lines_add
           in
           let is_deleted_file =
             List.length file.hunks = 1
             &&
             let hunk = List.hd file.hunks in
             hunk.starting_line = 1
             &&
             let hunk = List.hd file.hunks in
             hunk.lines |> all_lines_remove
           in
           let hunks =
             file.hunks
             |> List.filter (fun hunk -> hunk_lines_included hunk <> NoLines)
             |> List.map (fun (hunk : hunk) : Diff.hunk ->
                    let lines = hunk.lines |> map_lines in
                    {
                      starting_line = hunk.starting_line;
                      context_snippet = hunk.context_snippet;
                      lines;
                    })
           in
           if is_created_file then
             let hunk = List.hd file.hunks in
             let lines =
               hunk.lines |> map_lines
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
               hunk.lines |> map_lines
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
