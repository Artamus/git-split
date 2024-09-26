open TuiTypes
open TuiModel

let model : model =
  File
    (Zipper.from_list_exn
       [
         {
           path = Path "src/TestMain";
           content =
             Text
               {
                 visibility = Collapsed;
                 hunks =
                   [
                     {
                       starting_line = 1;
                       context_snippet = None;
                       visibility = Expanded;
                       lines =
                         [
                           Context "code";
                           Diff ("code 2", `removed, `included);
                           Diff ("code 3", `added, `included);
                           Context "code 4";
                         ];
                     };
                     {
                       starting_line = 15;
                       context_snippet = None;
                       visibility = Expanded;
                       lines =
                         [
                           Context "code 5";
                           Context "code 6";
                           Context "code 7";
                           Diff ("code 7.5", `removed, `included);
                           Diff ("code 7.9", `added, `included);
                           Context "code 8";
                         ];
                     };
                   ];
               };
         };
         {
           path = Path "src/YetAnotherFile";
           content =
             Text
               {
                 visibility = Collapsed;
                 hunks =
                   [
                     {
                       starting_line = 1;
                       context_snippet = None;
                       visibility = Expanded;
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
                     };
                   ];
               };
         };
         {
           path = ChangedPath { old_path = "lib/nottui_tui.ml"; new_path = "lib/nottuiTui.ml" };
           content =
             Text
               {
                 visibility = Collapsed;
                 hunks =
                   [
                     {
                       starting_line = 1;
                       context_snippet = None;
                       visibility = Expanded;
                       lines =
                         [
                           Context "code";
                           Diff ("code 2", `removed, `included);
                           Diff ("code 3", `added, `included);
                           Context "code 4";
                         ];
                     };
                   ];
               };
         };
       ])
