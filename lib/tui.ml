open Minttea

let dark_gray = Spices.color "245"
let highlight = Spices.(default |> reverse true |> build)
let header_bg = Spices.color "000"
let header = Spices.(default |> reverse true |> build)

type included_in_diff = Included | NotIncluded

type line =
  | Context of string
  | Removed of string * included_in_diff
  | Added of string * included_in_diff

type hunk = { lines : line list; included_in_diff : included_in_diff }
type file = { path : string; hunks : hunk list; included_in_diff : included_in_diff }
type cursor = FileCursor of int
type model = { files : file list; cursor : cursor }

let initial_model =
  {
    cursor = FileCursor 0;
    files =
      [
        {
          path = "src/TestMain";
          hunks =
            [
              {
                lines =
                  [
                    Context "code";
                    Removed ("code 2", Included);
                    Added ("code 3", Included);
                    Context "code 4";
                  ];
                included_in_diff = Included;
              };
              {
                lines =
                  [
                    Context "code";
                    Removed ("code 2", Included);
                    Added ("code 3", Included);
                    Context "code 4";
                  ];
                included_in_diff = Included;
              };
            ];
          included_in_diff = Included;
        };
        {
          path = "src/MainFile";
          hunks =
            [
              {
                lines =
                  [
                    Context "code";
                    Removed ("code 2", Included);
                    Added ("code 3", Included);
                    Context "code 4";
                  ];
                included_in_diff = Included;
              };
              {
                lines =
                  [
                    Context "code";
                    Removed ("code 2", Included);
                    Added ("code 3", Included);
                    Context "code 4";
                  ];
                included_in_diff = Included;
              };
            ];
          included_in_diff = Included;
        };
        {
          path = "src/YetAnotherFile";
          hunks =
            [
              {
                lines =
                  [
                    Context "code";
                    Removed ("code 2", Included);
                    Added ("code 3", Included);
                    Context "code 4";
                  ];
                included_in_diff = Included;
              };
              {
                lines =
                  [
                    Context "code";
                    Removed ("code 2", Included);
                    Added ("code 3", Included);
                    Context "code 4";
                  ];
                included_in_diff = Included;
              };
            ];
          included_in_diff = Included;
        };
      ];
  }

let init _model = Command.Seq [ Command.Enter_alt_screen; Command.Hide_cursor ]

let update event model =
  match event with
  (* if we press `q` or the escape key, we exit. *)
  | Event.KeyDown ((Key "q" | Escape), _modifier) ->
      (model, Command.Seq [ Command.Exit_alt_screen; Command.Quit ])
  (* if we press up or `k`, we move up in the list. *)
  | Event.KeyDown ((Up | Key "k"), _modifier) ->
      let cursor_pos = match model.cursor with FileCursor index -> index in
      let new_cursor_pos = if cursor_pos = 0 then List.length model.files - 1 else cursor_pos - 1 in
      let cursor = FileCursor new_cursor_pos in
      ({ model with cursor }, Command.Noop)
  (* if we press down or `j`, we move down in the list. *)
  | Event.KeyDown ((Down | Key "j"), _modifier) ->
      let cursor_pos = match model.cursor with FileCursor index -> index in
      let new_cursor_pos = if cursor_pos = List.length model.files - 1 then 0 else cursor_pos + 1 in
      let cursor = FileCursor new_cursor_pos in
      ({ model with cursor }, Command.Noop)
  (* if we press right or `l`, we expand the current item and move to the first subitem. *)
  | Event.KeyDown ((Right | Key "l"), _modifier) -> (model, Command.Noop)
  (* if we press left or `h`, we move left in the list *)
  | Event.KeyDown ((Left | Key "h"), _modifier) -> (model, Command.Noop)
  (* when we press enter or space we toggle the item in the list
     that the cursor points to *)
  | Event.KeyDown ((Enter | Space), _modifier) ->
      let toggle include_status =
        match include_status with Included -> NotIncluded | NotIncluded -> Included
      in
      let files =
        List.mapi
          (fun idx { path; hunks; included_in_diff } ->
            let cursor_pos = match model.cursor with FileCursor index -> index in
            let status = if idx = cursor_pos then toggle included_in_diff else included_in_diff in
            { path; hunks; included_in_diff = status })
          model.files
      in
      ({ model with files }, Command.Noop)
  (* for all other events, we do nothing *)
  | _ -> (model, Command.Noop)

let view model =
  (* we create our options by mapping over them *)
  let lines =
    model.files
    |> List.mapi (fun idx { path; hunks; included_in_diff } ->
           (* Per file context. *)
           let bla =
             List.mapi
               (fun idx { lines; included_in_diff } ->
                 (* Per hunk context. *)
                 let code_lines =
                   List.map
                     (fun line ->
                       (* Per line context. *)
                       let is_line_included_marker =
                         match line with
                         | Context _ -> "   "
                         | Removed (_, included_in_diff) | Added (_, included_in_diff) ->
                             let checkmark = if included_in_diff = Included then "x" else "" in
                             Format.sprintf "[%s]" checkmark
                       in

                       let line_content =
                         match line with
                         | Context content -> content
                         | Removed (content, _) | Added (content, _) -> content
                       in

                       "\t\t" ^ is_line_included_marker ^ Format.sprintf " %s" line_content)
                     lines
                 in

                 let is_hunk_included_marker = if included_in_diff = Included then "x" else "" in
                 let hunk_header =
                   Format.sprintf "\t[%s] Hunk %d" is_hunk_included_marker (idx + 1)
                 in
                 hunk_header :: code_lines)
               hunks
             |> List.flatten
           in

           let is_file_included_marker = if included_in_diff = Included then "x" else "" in
           let file_line = Format.sprintf "[%s] %s" is_file_included_marker path in
           let cursor_pos = match model.cursor with FileCursor index -> index in
           let maybe_highlighted_file_line =
             if cursor_pos = idx then highlight "%s" file_line else file_line
           in
           maybe_highlighted_file_line :: bla)
    |> List.flatten |> String.concat "\n"
  in
  let header =
    header "%s"
      "Use the arrow keys to navigate, space to select/unselect, c to confirm and ESC to exit."
  in
  (* and we send the UI for rendering! *)
  Format.sprintf {|%s
  
%s

      |} header lines

let app = Minttea.app ~init ~update ~view ()
