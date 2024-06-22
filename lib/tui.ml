open Minttea

let dark_gray = Spices.color "245"
let highlight = Spices.(default |> reverse true |> build)
let header_bg = Spices.color "000"
let header = Spices.(default |> reverse true |> build)

type included_in_diff = Included | NotIncluded
type line = Removed of string * included_in_diff | Added of string * included_in_diff

type hunk = {
  pre_context : string list;
  diff_lines : line list;
  post_context : string list;
  included_in_diff : included_in_diff;
}

(* TODO: I think only diff lines should be included or excluded. *)
type file = { path : string; hunks : hunk list; included_in_diff : included_in_diff }
type cursor = FileCursor of int | HunkCursor of int * int
type model = { files : file list; cursor : cursor }

let initial_model =
  {
    cursor = FileCursor 0;
    files =
      [
        {
          path = "src/TestMain";
          included_in_diff = Included;
          hunks =
            [
              {
                pre_context = [ "code" ];
                diff_lines = [ Removed ("code 2", Included); Added ("code 3", Included) ];
                post_context = [ "code 4" ];
                included_in_diff = Included;
              };
              {
                pre_context = [ "code 5"; "code 6"; "code 7" ];
                diff_lines = [ Removed ("code 7.5", Included); Added ("code 7.9", Included) ];
                post_context = [ "code 8" ];
                included_in_diff = Included;
              };
            ];
        };
        {
          path = "src/MainFile";
          included_in_diff = Included;
          hunks =
            [
              {
                pre_context = [ "code" ];
                diff_lines = [ Removed ("code 2", Included); Added ("code 3", Included) ];
                post_context = [ "code 4" ];
                included_in_diff = Included;
              };
              {
                pre_context = [ "code 5"; "code 6"; "code 7" ];
                diff_lines = [ Removed ("code 7.5", Included); Added ("code 7.9", Included) ];
                post_context = [ "code 8" ];
                included_in_diff = Included;
              };
            ];
        };
        {
          path = "src/YetAnotherFile";
          included_in_diff = Included;
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
                included_in_diff = Included;
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
      in
      ({ model with cursor }, Command.Noop)
  (* if we press right or `l`, we expand the current item and move to the first subitem. *)
  | Event.KeyDown ((Right | Key "l"), _modifier) ->
      let cursor =
        match model.cursor with
        | FileCursor file_idx -> HunkCursor (file_idx, 0)
        | HunkCursor _ -> model.cursor
      in
      ({ model with cursor }, Command.Noop)
  (* if we press left or `h`, we move left in the list *)
  | Event.KeyDown ((Left | Key "h"), _modifier) ->
      let cursor =
        match model.cursor with
        | FileCursor file_idx -> FileCursor file_idx
        | HunkCursor (file_idx, _) -> FileCursor file_idx
      in
      ({ model with cursor }, Command.Noop)
  (* when we press enter or space we toggle the item in the list
     that the cursor points to *)
  | Event.KeyDown ((Enter | Space), _modifier) ->
      let toggle include_status =
        match include_status with Included -> NotIncluded | NotIncluded -> Included
      in
      let files =
        model.files
        |> List.mapi (fun file_idx { path; hunks; included_in_diff = file_included_in_diff } ->
               let included_in_diff =
                 match model.cursor with
                 | FileCursor c_file_idx ->
                     if c_file_idx = file_idx then toggle file_included_in_diff
                     else file_included_in_diff
                 | HunkCursor _ -> file_included_in_diff
               in

               let hunks =
                 hunks
                 |> List.mapi
                      (fun
                        hunk_idx
                        {
                          pre_context;
                          diff_lines;
                          post_context;
                          included_in_diff = hunk_included_in_diff;
                        }
                      ->
                        let included_in_diff =
                          match model.cursor with
                          | HunkCursor (c_file_idx, c_hunk_idx) ->
                              if c_file_idx = file_idx && c_hunk_idx = hunk_idx then
                                toggle hunk_included_in_diff
                              else hunk_included_in_diff
                          | FileCursor _ -> hunk_included_in_diff
                        in
                        { pre_context; diff_lines; post_context; included_in_diff })
               in

               { path; hunks; included_in_diff })
      in
      ({ model with files }, Command.Noop)
  (* for all other events, we do nothing *)
  | _ -> (model, Command.Noop)

let view model =
  let lines =
    model.files
    |> List.mapi (fun file_idx { path; hunks; included_in_diff } ->
           (* Per file context. *)
           let hunks =
             hunks
             |> List.mapi
                  (fun hunk_idx { pre_context; diff_lines; post_context; included_in_diff } ->
                    (* Per hunk context. *)

                    (* TODO: DRY pre- and post-rendering. *)
                    let pre_context_lines =
                      pre_context |> List.map (fun context_line -> "\t\t   " ^ context_line)
                    in

                    let code_lines =
                      diff_lines
                      |> List.map (fun line ->
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

                             Format.sprintf "\t\t%s" line_content)
                    in

                    let post_context_lines =
                      post_context |> List.map (fun context_line -> "\t\t   " ^ context_line)
                    in

                    let is_hunk_included_marker = if included_in_diff = Included then "x" else " " in
                    let hunk_header_content =
                      Format.sprintf "\t[%s] Hunk %d" is_hunk_included_marker (hunk_idx + 1)
                    in
                    let hunk_header =
                      if model.cursor = HunkCursor (file_idx, hunk_idx) then
                        highlight "%s" hunk_header_content
                      else hunk_header_content
                    in
                    hunk_header :: List.concat [ pre_context_lines; code_lines; post_context_lines ])
             |> List.flatten
           in

           let is_file_included_marker = if included_in_diff = Included then "x" else " " in
           let file_line_content = Format.sprintf "[%s] %s" is_file_included_marker path in
           let file_line =
             if model.cursor = FileCursor file_idx then highlight "%s" file_line_content
             else file_line_content
           in
           file_line :: hunks)
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
