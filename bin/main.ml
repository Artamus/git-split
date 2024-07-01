let tui_line_of_diff_line = function
  | Git_split.DiffParser.UnchangedLine content -> Git_split.NottyTui.Context content
  | Git_split.DiffParser.RemovedLine content ->
      Git_split.NottyTui.Diff (content, `removed, `included)
  | Git_split.DiffParser.AddedLine content -> Git_split.NottyTui.Diff (content, `added, `included)

let model_of_diff (diff : Git_split.DiffParser.diff) : Git_split.NottyTui.model =
  let files =
    diff.files
    |> List.map (fun (file : Git_split.DiffParser.diff_file) : Git_split.NottyTui.file ->
           match file with
           | Git_split.DiffParser.RenamedFile renamed_file ->
               Git_split.NottyTui.RenamedFile
                 {
                   old_path = renamed_file.old_path;
                   new_path = renamed_file.new_path;
                   included = `included;
                 }
           | Git_split.DiffParser.DiffFile changed_file ->
               let hunks =
                 changed_file.hunks
                 |> List.map (fun (hunk : Git_split.DiffParser.hunk) : Git_split.NottyTui.hunk ->
                        let lines =
                          hunk.lines |> List.map (fun line -> tui_line_of_diff_line line)
                        in
                        { lines; lines_visibility = Git_split.NottyTui.Expanded })
               in
               Git_split.NottyTui.ChangedFile
                 {
                   hunks;
                   hunks_visibility = Git_split.NottyTui.Collapsed;
                   path = changed_file.path;
                 })
  in
  { files; cursor = FileCursor 0 }

let main () =
  let git_diff_cmd = Feather.process "git" [ "diff"; "HEAD~1"; "HEAD" ] in
  let _stdout = git_diff_cmd |> Feather.collect Feather.stdout in
  (* Many separate lines in one hunk. *)
  let _example1 =
    {|diff --git a/lib/tui.ml b/lib/tui.ml
index 0441c31..02b5753 100644
--- a/lib/tui.ml
+++ b/lib/tui.ml
@@ -1,14 +1,16 @@
open Minttea

-let cursor = Spices.(default |> reverse true |> build)
+let cursor_style = Spices.(default |> reverse true |> build)
let header = Spices.(default |> reverse true |> build)

+type visibility = Expanded | Collapsed
+
type line =
  | Context of string
  | Diff of string * [ `added | `removed ] * [ `included | `notincluded ]

-type hunk = { lines : line list; visibility : [ `expanded | `collapsed ] }
-type file = { path : string; hunks : hunk list }
+type hunk = { lines : line list; lines_visibility : visibility }
+type file = { path : string; hunks : hunk list; hunks_visibility : visibility }
type cursor = FileCursor of int | HunkCursor of int * int | LineCursor of int * int * int
type model = { files : file list; cursor : cursor }
type set_lines_inclusion = AllLines | SomeLines | NoLines|}
  in
  let example2 =
    {|diff --git a/bin/main.ml b/bin/main.ml
index 0ce4f70..3633cfb 100644
--- a/bin/main.ml
+++ b/bin/main.ml
@@ -14,7 +14,7 @@ let main () =
      + (libraries git_split feather re))"
  in
  let _diff = Git_split.DiffParser.parse_diff stdout in
- let _final_model = Git_split.Notty_tui.run in
+ let _final_model = Git_split.NottyTui.run in
  ()

 let () = main ()
diff --git a/lib/minttea_tui.ml b/lib/mintteaTui.ml
similarity index 100%
rename from lib/minttea_tui.ml
rename to lib/mintteaTui.ml
diff --git a/lib/notty_tui.ml b/lib/nottyTui.ml
similarity index 100%
rename from lib/notty_tui.ml
rename to lib/nottyTui.ml|}
  in
  let diff = Git_split.DiffParser.parse_diff example2 in
  let initial_model = model_of_diff diff in
  let _final_model = Git_split.NottyTui.run initial_model in
  ()

let () = main ()
