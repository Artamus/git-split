open Minttea

let cursor = Spices.(default |> reverse true |> build)
let header = Spices.(default |> reverse true |> build)

type included_in_diff = Included | NotIncluded
type line = Removed of string * included_in_diff | Added of string * included_in_diff
type hunk = { pre_context : string list; diff_lines : line list; post_context : string list }
type file = { path : string; hunks : hunk list }
type cursor = FileCursor of int | HunkCursor of int * int | LineCursor of int * int * int
type model = { files : file list; cursor : cursor }
type set_lines_inclusion = AllLines | SomeLines | NoLines

let hunk_lines_inclusion hunk =
  let all_lines_included =
    hunk.diff_lines
    |> List.for_all (fun line ->
           let line_included =
             match line with
             | Removed (_, line_included) -> line_included
             | Added (_, line_included) -> line_included
           in
           line_included = Included)
  in
  let any_line_included =
    hunk.diff_lines
    |> List.exists (fun line ->
           let line_included =
             match line with
             | Removed (_, line_included) -> line_included
             | Added (_, line_included) -> line_included
           in
           line_included = Included)
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
          hunks =
            [
              {
                pre_context = [ "code" ];
                diff_lines = [ Removed ("code 2", Included); Added ("code 3", Included) ];
                post_context = [ "code 4" ];
              };
              {
                pre_context = [ "code 5"; "code 6"; "code 7" ];
                diff_lines = [ Removed ("code 7.5", Included); Added ("code 7.9", Included) ];
                post_context = [ "code 8" ];
              };
            ];
        };
        {
          path = "src/MainFile";
          hunks =
            [
              {
                pre_context = [ "code" ];
                diff_lines = [ Removed ("code 2", Included); Added ("code 3", Included) ];
                post_context = [ "code 4" ];
              };
              {
                pre_context = [ "code 5"; "code 6"; "code 7" ];
                diff_lines = [ Removed ("code 7.5", Included); Added ("code 7.9", Included) ];
                post_context = [ "code 8" ];
              };
            ];
        };
        {
          path = "src/YetAnotherFile";
          hunks =
            [
              {
                pre_context = [];
                diff_lines =
                  [
                    Added ("These", Included);
                    Added ("Are", Included);
                    Added ("All", Included);
                    Added ("Added", Included);
                    Added ("Lines", Included);
                    Added ("Because", Included);
                    Added ("This", Included);
                    Added ("Is", Included);
                    Added ("A", Included);
                    Added ("New", Included);
                    Added ("File", Included);
                  ];
                post_context = [];
              };
            ];
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
      let cursor =
        match model.cursor with
        | FileCursor 0 -> FileCursor (List.length model.files - 1)
        | FileCursor file_idx -> FileCursor (file_idx - 1)
        | HunkCursor (file_idx, 0) -> FileCursor file_idx
        | HunkCursor (file_idx, hunk_idx) -> HunkCursor (file_idx, hunk_idx - 1)
        | LineCursor (file_idx, hunk_idx, 0) -> HunkCursor (file_idx, hunk_idx)
        | LineCursor (file_idx, hunk_idx, line_idx) -> LineCursor (file_idx, hunk_idx, line_idx - 1)
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
        | LineCursor (file_idx, hunk_idx, line_idx)
        (* TODO: This is some nasty code, there is definitely a better way. *)
          when let last_line_idx =
                 List.length (List.nth (List.nth model.files file_idx).hunks hunk_idx).diff_lines
                 - 1
               in
               line_idx = last_line_idx ->
            let last_hunk_idx = List.length (List.nth model.files file_idx).hunks - 1 in
            if hunk_idx = last_hunk_idx then
              if file_idx = last_file_idx then FileCursor 0 else FileCursor (file_idx + 1)
            else HunkCursor (file_idx, hunk_idx + 1)
        | LineCursor (file_idx, hunk_idx, line_idx) -> LineCursor (file_idx, hunk_idx, line_idx + 1)
      in
      ({ model with cursor }, Command.Noop)
  (* if we press right or `l`, we expand the current item and move to the first subitem. *)
  | Event.KeyDown ((Right | Key "l"), _modifier) ->
      let cursor =
        match model.cursor with
        | FileCursor file_idx -> HunkCursor (file_idx, 0)
        | HunkCursor (file_idx, hunk_idx) -> LineCursor (file_idx, hunk_idx, 0)
        | LineCursor _ -> model.cursor
      in
      ({ model with cursor }, Command.Noop)
  (* if we press left or `h`, we move left in the list *)
  | Event.KeyDown ((Left | Key "h"), _modifier) ->
      let cursor =
        match model.cursor with
        | FileCursor _ -> model.cursor
        | HunkCursor (file_idx, _) -> FileCursor file_idx
        | LineCursor (file_idx, hunk_idx, _) -> HunkCursor (file_idx, hunk_idx)
      in
      ({ model with cursor }, Command.Noop)
  (* when we press enter or space we toggle the item in the list
     that the cursor points to *)
  | Event.KeyDown ((Enter | Space), _modifier) ->
      let toggle include_status =
        match include_status with Included -> NotIncluded | NotIncluded -> Included
      in

      let toggle_group = function
        | NoLines -> Included
        | SomeLines -> Included
        | AllLines -> NotIncluded
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
                              let diff_lines =
                                hunk.diff_lines
                                |> List.map (fun line ->
                                       match line with
                                       | Removed (contents, _) ->
                                           Removed (contents, included_in_diff)
                                       | Added (contents, _) -> Added (contents, included_in_diff))
                              in
                              { hunk with diff_lines })
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
                              let diff_lines =
                                if hunk_idx <> c_hunk_idx then hunk.diff_lines
                                else
                                  hunk.diff_lines
                                  |> List.map (fun line ->
                                         match line with
                                         | Removed (contents, _) ->
                                             Removed (contents, included_in_diff)
                                         | Added (contents, _) -> Added (contents, included_in_diff))
                              in
                              { hunk with diff_lines })
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
                              let diff_lines =
                                if hunk_idx <> c_hunk_idx then hunk.diff_lines
                                else
                                  hunk.diff_lines
                                  |> List.mapi (fun line_idx line ->
                                         if line_idx <> c_line_idx then line
                                         else
                                           match line with
                                           | Removed (contents, included_in_diff) ->
                                               Removed (contents, toggle included_in_diff)
                                           | Added (contents, included_in_diff) ->
                                               Added (contents, toggle included_in_diff))
                              in
                              { hunk with diff_lines })
                   in
                   { file with hunks })
      in
      ({ model with files }, Command.Noop)
  (* for all other events, we do nothing *)
  | _ -> (model, Command.Noop)

let view model =
  let lines =
    model.files
    |> List.mapi (fun file_idx file ->
           (* Per file context. *)
           let code_hunks =
             file.hunks
             |> List.mapi (fun hunk_idx hunk ->
                    (* Per hunk context. *)
                    let pre_context_lines =
                      hunk.pre_context |> List.map (fun context_line -> "\t\t    " ^ context_line)
                    in

                    let code_lines =
                      hunk.diff_lines
                      |> List.mapi (fun line_idx line ->
                             (* Per line context. *)
                             let line_content =
                               match line with
                               | Removed (content, included_in_diff)
                               | Added (content, included_in_diff) ->
                                   let checkmark =
                                     if included_in_diff = Included then "x" else " "
                                   in
                                   Format.sprintf "[%s] %s" checkmark content
                             in

                             let line =
                               if model.cursor = LineCursor (file_idx, hunk_idx, line_idx) then
                                 cursor "%s" line_content
                               else line_content
                             in

                             Format.sprintf "\t\t%s" line)
                    in

                    let post_context_lines =
                      hunk.post_context |> List.map (fun context_line -> "\t\t    " ^ context_line)
                    in

                    let is_hunk_included_marker =
                      match hunk_lines_inclusion hunk with
                      | AllLines -> "x"
                      | SomeLines -> "~"
                      | NoLines -> " "
                    in
                    let hunk_header_content =
                      Format.sprintf "\t[%s] Hunk %d" is_hunk_included_marker (hunk_idx + 1)
                    in
                    let hunk_header =
                      if model.cursor = HunkCursor (file_idx, hunk_idx) then
                        cursor "%s" hunk_header_content
                      else hunk_header_content
                    in
                    hunk_header :: List.concat [ pre_context_lines; code_lines; post_context_lines ])
             |> List.flatten
           in

           let is_file_included_marker =
             match file_lines_inclusion file with
             | AllLines -> "x"
             | SomeLines -> "~"
             | NoLines -> " "
           in

           let file_line_content = Format.sprintf "[%s] %s" is_file_included_marker file.path in
           let file_line =
             if model.cursor = FileCursor file_idx then cursor "%s" file_line_content
             else file_line_content
           in
           file_line :: code_hunks)
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
