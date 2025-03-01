open Notty
open Notty_unix
open TuiTypes

let ( let* ) = Result.bind

module List = struct
  include List

  let is_empty = function [] -> true | _ -> false
end

let update event model =
  match event with
  | `Key (`Arrow `Up, _) | `Key (`ASCII 'k', _) -> TuiModel.prev model
  | `Key (`Arrow `Down, _) | `Key (`ASCII 'j', _) -> TuiModel.next model
  | `Key (`Arrow `Left, _) | `Key (`ASCII 'h', _) ->
      if TuiModel.is_expanded model then TuiModel.collapse model else TuiModel.up model
  | `Key (`Arrow `Right, _) | `Key (`ASCII 'l', _) ->
      if TuiModel.is_expanded model then TuiModel.next model else TuiModel.expand model
  | `Key (`ASCII ' ', _) | `Key (`Enter, _) -> TuiModel.toggle_inclusion model
  | _ -> model

let help_style = A.fg (A.rgb_888 ~r:98 ~g:98 ~b:98)
let binary_file_color = A.rgb_888 ~r:51 ~g:153 ~b:255
let hunk_style = A.fg (A.rgb_888 ~r:204 ~g:153 ~b:0)

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
  (* Notty cannot render '\t', so we replace it with two spaces. *)
  let line_content = line_content |> String.split_on_char '\t' |> String.concat "  " in

  let colour =
    match line with
    | Context _ -> A.gray 10
    | Diff (_, `added, _) -> A.green
    | Diff (_, `removed, _) -> A.red
  in

  I.strf ~attr:A.(fg colour ++ style) "%s %s %s" included_prefix line_kind_prefix line_content
  |> I.hpad 7 0

let render_hunk (hunk : hunk) (hunk_idx : int) is_cursor lines : image list =
  let lines =
    Option.value lines
      ~default:
        (match hunk.visibility with
        | Collapsed -> []
        | Expanded -> hunk.lines |> List.map (fun line -> render_line line false))
  in

  let style = if is_cursor then A.st A.reverse else A.empty in
  let hunk_included_marker =
    match TuiModel.hunk_lines_included hunk with
    | AllLines -> "x"
    | SomeLines -> "~"
    | NoLines -> " "
  in
  let hunk_header =
    I.strf ~attr:A.(hunk_style ++ style) "[%s] Hunk %d" hunk_included_marker (hunk_idx + 1)
  in

  let hunk_collapsible_marker = match hunk.visibility with Expanded -> "-" | Collapsed -> "+" in
  let collapsible_indicator = I.strf ~attr:help_style "(%s)" hunk_collapsible_marker in

  let hunk_line = I.( <|> ) collapsible_indicator hunk_header |> I.hpad 2 0 in
  hunk_line :: lines

let render_file (file : file) is_cursor hunk_lines : image list =
  let hunk_lines =
    Option.value hunk_lines
      ~default:
        (match file.content with
        | Text { visibility = Collapsed; _ } -> []
        | Text { visibility = Expanded; hunks } ->
            hunks
            |> List.mapi (fun hunk_idx hunk -> render_hunk hunk hunk_idx false None)
            |> List.flatten
        | Binary _ -> [])
  in

  let style = if is_cursor then A.st A.reverse else A.empty in

  let file_included_marker =
    match file.content with
    | Text { hunks; _ } -> (
        match TuiModel.file_lines_included hunks with
        | AllLines -> "[x]"
        | SomeLines -> "[~]"
        | NoLines -> "[ ]")
    | Binary (_, `included) -> "[x]"
    | _ -> "[ ]"
  in

  let file_collapsible_marker =
    match file.content with
    | Binary _ -> "   "
    | Text { hunks; _ } when List.length hunks = 0 -> "   "
    | Text { visibility = Expanded; _ } -> "(-)"
    | Text { visibility = Collapsed; _ } -> "(+)"
  in
  let collapsible_indicator = I.strf ~attr:help_style "%s" file_collapsible_marker in

  let file_path =
    match file.path with
    | Path path -> " " ^ path
    | ChangedPath changed_path ->
        Printf.sprintf " %s -> %s" changed_path.old_path changed_path.new_path
  in

  let binary_marker = match file.content with Binary _ -> " (binary file)" | Text _ -> "" in

  let mode_change =
    match file.mode with
    | Some (ChangedMode changed_mode) ->
        Printf.sprintf " (mode %d -> %d)" changed_mode.old_mode changed_mode.new_mode
    | _ -> ""
  in

  let file_header =
    I.strf ~attr:style "%s%s%s%s" file_included_marker file_path binary_marker mode_change
  in

  let file_line = I.( <|> ) collapsible_indicator file_header in
  file_line :: hunk_lines

let render_model = function
  | TuiModel.File (Zip (ls, cursor, rs)) ->
      let left_files =
        ls |> List.rev |> List.map (fun file -> render_file file false None) |> List.flatten
      in
      let right_files = rs |> List.map (fun file -> render_file file false None) |> List.flatten in
      let cursor_file = render_file cursor true None in
      left_files @ cursor_file @ right_files
  | TuiModel.Hunk (Zip (files_ls, file_cursor, files_rs), Zip (ls, cursor, rs)) ->
      let left_files =
        files_ls |> List.rev |> List.map (fun file -> render_file file false None) |> List.flatten
      in
      let right_files =
        files_rs |> List.map (fun file -> render_file file false None) |> List.flatten
      in
      let left_hunks =
        ls |> List.rev
        |> List.mapi (fun hunk_idx hunk -> render_hunk hunk hunk_idx false None)
        |> List.flatten
      in
      let right_start_idx = List.length ls + 1 in
      let right_hunks =
        rs
        |> List.mapi (fun hunk_idx hunk -> render_hunk hunk (right_start_idx + hunk_idx) false None)
        |> List.flatten
      in
      let cursor_hunk = render_hunk cursor (List.length ls) true None in
      let cursor_file =
        render_file file_cursor false (Some (left_hunks @ cursor_hunk @ right_hunks))
      in
      left_files @ cursor_file @ right_files
  | TuiModel.Line
      ( Zip (files_ls, file_cursor, files_rs),
        Zip (hunks_ls, hunk_cursor, hunks_rs),
        Zip (ls, cursor, rs) ) ->
      let left_files =
        files_ls |> List.rev |> List.map (fun file -> render_file file false None) |> List.flatten
      in
      let right_files =
        files_rs |> List.map (fun file -> render_file file false None) |> List.flatten
      in
      let left_hunks =
        hunks_ls |> List.rev
        |> List.mapi (fun hunk_idx hunk -> render_hunk hunk hunk_idx false None)
        |> List.flatten
      in
      let right_hunks =
        hunks_rs
        |> List.mapi (fun hunk_idx hunk ->
               render_hunk hunk (List.length hunks_ls + 1 + hunk_idx) false None)
        |> List.flatten
      in
      let left_lines = ls |> List.rev |> List.map (fun line -> render_line line false) in
      let right_lines = rs |> List.map (fun line -> render_line line false) in
      let cursor_line = render_line cursor true in
      let cursor_hunk =
        render_hunk hunk_cursor (List.length hunks_ls) false
          (Some (left_lines @ (cursor_line :: right_lines)))
      in
      let cursor_file =
        render_file file_cursor false (Some (left_hunks @ cursor_hunk @ right_hunks))
      in
      left_files @ cursor_file @ right_files

let count_hunk_visible_lines (hunk : hunk) =
  let num_visible_lines =
    match hunk.visibility with Collapsed -> 0 | Expanded -> List.length hunk.lines
  in
  num_visible_lines + 1

let count_file_visible_lines file =
  match file.content with
  | Binary _ -> 0
  | Text { visibility = Collapsed; _ } -> 0
  | Text { visibility = Expanded; hunks } ->
      List.fold_left (fun acc hunk -> acc + count_hunk_visible_lines hunk) 1 hunks

let cursor_index = function
  | TuiModel.File (Zip (ls, _, _)) ->
      List.fold_left (fun acc file -> acc + count_file_visible_lines file) 1 ls
  | TuiModel.Hunk (Zip (files_ls, _, _), Zip (ls, _, _)) ->
      let pre_files_line_count =
        List.fold_left (fun acc file -> acc + count_file_visible_lines file) 1 files_ls
      in
      let pre_hunks_line_count =
        List.fold_left (fun acc hunk -> acc + count_hunk_visible_lines hunk) 1 ls
      in
      pre_files_line_count + pre_hunks_line_count
  | TuiModel.Line (Zip (files_ls, _, _), Zip (hunks_ls, _, _), Zip (ls, _, _)) ->
      let pre_files_line_count =
        List.fold_left (fun acc file -> acc + count_file_visible_lines file) 1 files_ls
      in
      let pre_hunks_line_count =
        List.fold_left (fun acc hunk -> acc + count_hunk_visible_lines hunk) 1 hunks_ls
      in
      let pre_lines_count = List.length ls in
      pre_files_line_count + pre_hunks_line_count + pre_lines_count + 1

let view model viewport_height =
  let help_line =
    I.string help_style "↑/k:up ↓/j:down ←/h:collapse →/l:expand ↵/space:toggle c:confirm q:quit"
  in

  (* The top help line will always be visible. *)
  let viewport_height = viewport_height - 1 in

  let terminal_lines = render_model model in
  let cursor_index = cursor_index model in

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

  I.vcat (help_line :: visible_lines)

let any_lines_selected file_z =
  Zipper.to_list file_z
  |> List.exists (fun file ->
         match file.content with
         | Binary _ -> false
         | Text { hunks; _ } -> (
             match TuiModel.file_lines_included hunks with
             | AllLines | SomeLines -> true
             | _ -> false))

let rec ui_loop t state =
  let _, h = Term.size t in
  Term.image t (view state h);
  match Term.event t with
  | `Key (`ASCII 'c', _) ->
      let any_lines_selected =
        match state with
        | File file_z -> any_lines_selected file_z
        | Hunk (file_z, _) -> any_lines_selected file_z
        | Line (file_z, _, _) -> any_lines_selected file_z
      in
      if any_lines_selected then Some state else ui_loop t state
  | `Key (`Escape, _) | `Key (`ASCII 'q', _) -> None
  | event ->
      let new_state = update event state in
      ui_loop t new_state

let run initial_model =
  let term = Term.create ~dispose:true ~mouse:false () in
  let final_model = ui_loop term initial_model in
  Term.release term;
  final_model

let tui_line_of_diff_line = function
  | `ContextLine content -> Context content
  | `RemovedLine content -> Diff (content, `removed, `included)
  | `AddedLine content -> Diff (content, `added, `included)

let model_of_diff (diff : Diff.diff) =
  let files =
    diff.files
    |> List.filter_map (fun (file : Diff.file) : file option ->
           match file with
           | DeletedFile deleted_file -> (
               let mode = Some (Mode deleted_file.mode) in
               match deleted_file.content with
               | `Binary content ->
                   Some
                     {
                       path = Path deleted_file.path;
                       kind = DeletedFile;
                       mode;
                       content = Binary (content, `included);
                     }
               | `Text removed_lines ->
                   let lines = removed_lines |> List.map tui_line_of_diff_line in
                   Some
                     {
                       path = Path deleted_file.path;
                       kind = DeletedFile;
                       mode;
                       content =
                         Text
                           {
                             visibility = Collapsed;
                             hunks =
                               (if List.is_empty lines then []
                                else
                                  [
                                    {
                                      starting_line = 1;
                                      context_snippet = None;
                                      visibility = Expanded;
                                      lines;
                                    };
                                  ]);
                           };
                     })
           | CreatedFile created_file -> (
               let mode = Some (Mode created_file.mode) in
               match created_file.content with
               | `Binary content ->
                   Some
                     {
                       path = Path created_file.path;
                       kind = CreatedFile;
                       mode;
                       content = Binary (content, `included);
                     }
               | `Text added_lines ->
                   let lines = added_lines |> List.map tui_line_of_diff_line in
                   Some
                     {
                       path = Path created_file.path;
                       kind = CreatedFile;
                       mode;
                       content =
                         Text
                           {
                             visibility = Collapsed;
                             hunks =
                               (if List.is_empty lines then []
                                else
                                  [
                                    {
                                      starting_line = 1;
                                      context_snippet = None;
                                      visibility = Expanded;
                                      lines;
                                    };
                                  ]);
                           };
                     })
           | ChangedFile changed_file -> (
               let mode =
                 changed_file.mode_change
                 |> Option.map (fun (changed_mode : Diff.mode_change) ->
                        ChangedMode
                          { old_mode = changed_mode.old_mode; new_mode = changed_mode.new_mode })
               in
               let path =
                 match changed_file.path with
                 | Path path -> Path path
                 | ChangedPath { old_path; new_path } -> ChangedPath { old_path; new_path }
               in
               match changed_file.content with
               | `Binary content ->
                   let content = Binary (content, `included) in
                   Some { path; kind = ChangedFile; mode; content }
               | `Text changed_hunks ->
                   let hunks =
                     changed_hunks
                     |> List.map (fun (hunk : Diff.hunk) : hunk ->
                            let lines = hunk.lines |> List.map tui_line_of_diff_line in
                            {
                              starting_line = hunk.starting_line;
                              context_snippet = hunk.context_snippet;
                              visibility = Expanded;
                              lines;
                            })
                   in
                   Some
                     {
                       path;
                       kind = ChangedFile;
                       mode;
                       content = Text { visibility = Collapsed; hunks };
                     }))
  in
  Zipper.from_list files |> Option.map (fun file_z -> TuiModel.File file_z)

(* We update the state attached to files eagerly, therefore we can ignore the inner zippers. *)
let model_files = function
  | TuiModel.File (Zip (ls, cursor, rs)) -> List.rev ls @ (cursor :: rs)
  | TuiModel.Hunk (Zip (ls, cursor, rs), _) -> List.rev ls @ (cursor :: rs)
  | TuiModel.Line (Zip (ls, cursor, rs), _, _) -> List.rev ls @ (cursor :: rs)

let diff_line_of_model_line = function
  | Context content -> Some (`ContextLine content)
  | Diff (content, `added, `included) -> Some (`AddedLine content)
  | Diff (_, `added, `notincluded) -> None
  | Diff (content, `removed, `included) -> Some (`RemovedLine content)
  | Diff (content, `removed, `notincluded) -> Some (`ContextLine content)

let non_empty_file file =
  match file.content with
  | Text { hunks; _ } -> TuiModel.file_lines_included hunks <> NoLines
  | Binary (_, `included) -> true
  | _ -> false

let diff_of_model model =
  let files_results =
    model_files model |> List.filter non_empty_file
    |> List.map (fun file ->
           let content =
             match file.content with
             | Binary (content, _) -> `Binary content
             | Text { hunks; _ } ->
                 let new_hunks =
                   hunks
                   |> List.filter (fun hunk -> TuiModel.hunk_lines_included hunk <> NoLines)
                   |> List.map (fun (hunk : hunk) : Diff.hunk ->
                          let lines = hunk.lines |> List.filter_map diff_line_of_model_line in
                          {
                            starting_line = hunk.starting_line;
                            context_snippet = hunk.context_snippet;
                            lines;
                          })
                 in
                 `Text new_hunks
           in

           match file.kind with
           | ChangedFile ->
               let path =
                 match file.path with
                 | Path path -> Diff.Path path
                 | ChangedPath changed_path ->
                     Diff.ChangedPath
                       { old_path = changed_path.old_path; new_path = changed_path.new_path }
               in
               let mode_change =
                 file.mode
                 |> Option.map (fun mode ->
                        match mode with
                        | Mode _ -> None
                        | ChangedMode { old_mode; new_mode } -> Some Diff.{ old_mode; new_mode })
                 |> Option.join
               in
               Ok (Diff.ChangedFile { path; mode_change; content })
           | CreatedFile ->
               let* path =
                 match file.path with
                 | Path path -> Ok path
                 | _ -> Error "created file cannot have changed path"
               in
               let mode =
                 file.mode
                 |> Option.map (fun mode ->
                        match mode with Mode mode -> Some mode | ChangedMode _ -> None)
                 |> Option.join
               in
               let* mode =
                 match mode with
                 | Some mode -> Ok mode
                 | None -> Error "created file cannot have changed mode"
               in
               let content =
                 match content with
                 | `Text hunks ->
                     let hunk = List.hd hunks in
                     let lines =
                       hunk.lines
                       |> List.filter_map (fun line ->
                              match line with
                              | `AddedLine added_line -> Some (`AddedLine added_line)
                              | _ -> None)
                     in
                     `Text lines
                 | `Binary bin -> `Binary bin
               in
               Ok (Diff.CreatedFile { path; mode; content })
           | DeletedFile ->
               let mode =
                 file.mode
                 |> Option.map (fun mode ->
                        match mode with Mode mode -> Some mode | ChangedMode _ -> None)
                 |> Option.join
               in
               let* mode =
                 match mode with
                 | Some mode -> Ok mode
                 | None -> Error "deleted file cannot have changed mode"
               in

               let is_deleted_file =
                 match content with
                 | `Binary _ -> true
                 | `Text hunks ->
                     let hunk = List.hd hunks in
                     hunk.lines
                     |> List.for_all (fun line ->
                            match line with `RemovedLine _ -> true | _ -> false)
               in

               if is_deleted_file then
                 let* path =
                   match file.path with
                   | Path path -> Ok path
                   | _ -> Error "deleted file cannot have changed path"
                 in
                 let content =
                   match content with
                   | `Text hunks ->
                       let hunk = List.hd hunks in
                       let lines =
                         hunk.lines
                         |> List.filter_map (fun line ->
                                match line with
                                | `RemovedLine removed_line -> Some (`RemovedLine removed_line)
                                | _ -> None)
                       in
                       `Text lines
                   | `Binary bin -> `Binary bin
                 in
                 Ok (Diff.DeletedFile { path; mode; content })
               else
                 let* path =
                   match file.path with
                   | Path path -> Ok (Diff.Path path)
                   | _ -> Error "deleted file cannot have changed path"
                 in
                 Ok (Diff.ChangedFile { path; mode_change = None; content }))
  in
  let* files = Ok (List.filter_map Result.to_option files_results) in
  Ok Diff.{ files }
