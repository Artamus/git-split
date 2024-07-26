open TuiTypes

type model =
  | File of file Zipper.zipper
  | Hunk of file Zipper.zipper * hunk Zipper.zipper
  | Line of file Zipper.zipper * hunk Zipper.zipper * LineZipper.zipper
[@@deriving show, eq]

let prev = function
  | File file_z -> (
      let prev_file_z = Zipper.prev_wrap file_z in
      let file = Zipper.cursor prev_file_z in
      match file.visibility with
      | Collapsed -> File prev_file_z
      | Expanded -> (
          let hunk_z = Option.get @@ Zipper.from_list_rev file.hunks in
          let hunk = Zipper.cursor hunk_z in
          match hunk.visibility with
          | Collapsed -> Hunk (prev_file_z, hunk_z)
          | Expanded ->
              Line (prev_file_z, hunk_z, Result.get_ok @@ LineZipper.from_list_rev hunk.lines)))
  | Hunk (file_z, hunk_z) -> (
      if Zipper.at_begin hunk_z then
        let file = Zipper.cursor file_z in
        let file_z = Zipper.replace { file with hunks = Zipper.to_list hunk_z } file_z in
        File file_z
      else
        let prev_hunk_z = Zipper.prev hunk_z in
        let hunk = Zipper.cursor prev_hunk_z in
        match hunk.visibility with
        | Collapsed -> Hunk (file_z, prev_hunk_z)
        | Expanded -> Line (file_z, prev_hunk_z, Result.get_ok (LineZipper.from_list_rev hunk.lines))
      )
  | Line (file_z, hunk_z, line_z) ->
      if LineZipper.at_begin line_z then
        let hunk = Zipper.cursor hunk_z in
        let hunk_z = Zipper.replace { hunk with lines = LineZipper.to_list line_z } hunk_z in
        Hunk (file_z, hunk_z)
      else Line (file_z, hunk_z, LineZipper.prev line_z)

let next = function
  | File file_z -> (
      let file = Zipper.cursor file_z in
      match file.visibility with
      | Collapsed -> File (Zipper.next_wrap file_z)
      | Expanded ->
          Zipper.from_list file.hunks
          |> Option.map (fun hunk_z -> Hunk (file_z, hunk_z))
          |> Option.value ~default:(File (Zipper.next file_z)))
  | Hunk (file_z, hunk_z) -> (
      let hunk = Zipper.cursor hunk_z in
      match hunk.visibility with
      | Collapsed ->
          if Zipper.at_end hunk_z then
            let file = Zipper.cursor file_z in
            let file_z = Zipper.replace { file with hunks = Zipper.to_list hunk_z } file_z in
            File (Zipper.next_wrap file_z)
          else Hunk (file_z, Zipper.next hunk_z)
      | Expanded -> Line (file_z, hunk_z, Result.get_ok (LineZipper.from_list hunk.lines)))
  | Line (file_z, hunk_z, line_z) ->
      if LineZipper.at_end line_z then
        if Zipper.at_end hunk_z then
          let hunk = Zipper.cursor hunk_z in
          let hunk_z = Zipper.replace { hunk with lines = LineZipper.to_list line_z } hunk_z in
          let file = Zipper.cursor file_z in
          let file_z = Zipper.replace { file with hunks = Zipper.to_list hunk_z } file_z in
          File (Zipper.next_wrap file_z)
        else
          let hunk = Zipper.cursor hunk_z in
          let hunk_z = Zipper.replace { hunk with lines = LineZipper.to_list line_z } hunk_z in
          Hunk (file_z, Zipper.next hunk_z)
      else Line (file_z, hunk_z, LineZipper.next line_z)

let collapse = function
  | File file_z ->
      let file = Zipper.cursor file_z in
      let file_z = Zipper.replace { file with visibility = Collapsed } file_z in
      File file_z
  | Hunk (file_z, hunk_z) ->
      let hunk = Zipper.cursor hunk_z in
      let hunk_z = Zipper.replace { hunk with visibility = Collapsed } hunk_z in
      Hunk (file_z, hunk_z)
  | z -> z

let expand = function
  | File file_z ->
      let file = Zipper.cursor file_z in
      let file_z = Zipper.replace { file with visibility = Expanded } file_z in
      File file_z
  | Hunk (file_z, hunk_z) ->
      let hunk = Zipper.cursor hunk_z in
      let hunk_z = Zipper.replace { hunk with visibility = Expanded } hunk_z in
      Hunk (file_z, hunk_z)
  | z -> z

type lines_included = AllLines | SomeLines | NoLines

let hunk_lines_included hunk =
  let all_lines_included =
    hunk.lines
    |> List.for_all (fun line ->
           match line with Context _ -> true | Diff (_, _, `included) -> true | _ -> false)
  in
  let any_line_included =
    hunk.lines
    |> List.exists (fun line ->
           match line with Context _ -> false | Diff (_, _, `included) -> true | _ -> false)
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

let toggle_line include_status =
  match include_status with `included -> `notincluded | `notincluded -> `included

let toggle_lines = function
  | NoLines -> `included
  | SomeLines -> `included
  | AllLines -> `notincluded

let toggle_inclusion = function
  | File file_z ->
      let file = Zipper.cursor file_z in
      let include_lines = toggle_lines @@ file_lines_included file in
      let hunks =
        file.hunks
        |> List.map (fun hunk ->
               let lines =
                 hunk.lines
                 |> List.map (fun line ->
                        match line with
                        | Context content -> Context content
                        | Diff (content, kind, _) -> Diff (content, kind, include_lines))
               in
               { hunk with lines })
      in
      let file = { file with hunks } in
      let file_z = Zipper.replace file file_z in
      File file_z
  | Hunk (file_z, hunk_z) ->
      let hunk = Zipper.cursor hunk_z in
      let include_lines = toggle_lines @@ hunk_lines_included hunk in
      let lines =
        hunk.lines
        |> List.map (fun line ->
               match line with
               | Context content -> Context content
               | Diff (content, kind, _) -> Diff (content, kind, include_lines))
      in
      let hunk = { hunk with lines } in
      let hunk_z = Zipper.replace hunk hunk_z in
      let file = Zipper.cursor file_z in
      let file_z = Zipper.replace { file with hunks = Zipper.to_list hunk_z } file_z in
      Hunk (file_z, hunk_z)
  | Line (file_z, hunk_z, line_z) ->
      let line = LineZipper.cursor line_z in
      let line =
        match line with
        | Diff (content, kind, included) -> Diff (content, kind, toggle_line included)
        | l -> l
      in
      let line_z = LineZipper.replace line line_z in
      let hunk = Zipper.cursor hunk_z in
      let hunk_z = Zipper.replace { hunk with lines = LineZipper.to_list line_z } hunk_z in
      let file = Zipper.cursor file_z in
      let file_z = Zipper.replace { file with hunks = Zipper.to_list hunk_z } file_z in
      Line (file_z, hunk_z, line_z)