external libgit_version: unit -> int * int * int = "ocaml_libgit2_version"

let maj, min, rev = libgit_version () in
  Printf.printf "Version: %d, %d, %d\n" maj min rev

let () = print_endline "start" in
let repo_path = "/home/artamus/repos/git-split" in
let _open_res = C.Functions.libgit2_init () in
let repo_ptr =
  Ctypes.allocate
    (Ctypes.ptr C.Types.git_repository)
    (Ctypes.from_voidp C.Types.git_repository Ctypes.null)
in
let repo_open_res = C.Functions.git_repository_open repo_ptr repo_path in
let () = print_endline ("Repo opened, res: " ^ string_of_int repo_open_res) in
let git_obj_ptr =
  Ctypes.allocate (Ctypes.ptr C.Types.git_object) (Ctypes.from_voidp C.Types.git_object Ctypes.null)
in
let revparse_res = C.Functions.git_revparse_single git_obj_ptr (Ctypes.( !@ ) repo_ptr) "HEAD" in
let () = print_endline ("Revparse, res: " ^ string_of_int revparse_res) in
let git_oid = C.Functions.git_object_id (Ctypes.( !@ ) git_obj_ptr) in
let buf = Ctypes.allocate_n Ctypes.char ~count:40 in
let _oid_fmt_res = C.Functions.git_oid_fmt buf git_oid in
let pask = Ctypes.string_from_ptr buf ~length:40 in
let () = print_endline ("lol " ^ pask) in
let git_commit_ptr =
  Ctypes.allocate (Ctypes.ptr C.Types.git_commit) (Ctypes.from_voidp C.Types.git_commit Ctypes.null)
in
let git_commit_res =
  C.Functions.git_commit_lookup git_commit_ptr (Ctypes.( !@ ) repo_ptr) git_oid
in
let () = print_endline ("Commit, res: " ^ string_of_int git_commit_res) in
let parent_commit_ptr =
  Ctypes.allocate (Ctypes.ptr C.Types.git_commit) (Ctypes.from_voidp C.Types.git_commit Ctypes.null)
in
let parent_commit_res =
  C.Functions.git_commit_parent parent_commit_ptr (Ctypes.( !@ ) git_commit_ptr) Unsigned.UInt.zero
in
let () = print_endline ("Parent commit, res: " ^ string_of_int parent_commit_res) in
let commit_tree_ptr =
  Ctypes.allocate (Ctypes.ptr C.Types.git_tree) (Ctypes.from_voidp C.Types.git_tree Ctypes.null)
in
let commit_tree_res = C.Functions.git_commit_tree commit_tree_ptr (Ctypes.( !@ ) git_commit_ptr) in
let () = print_endline ("Commit tree res: " ^ string_of_int commit_tree_res) in
let parent_tree_ptr =
  Ctypes.allocate (Ctypes.ptr C.Types.git_tree) (Ctypes.from_voidp C.Types.git_tree Ctypes.null)
in
let parent_tree_res =
  C.Functions.git_commit_tree parent_tree_ptr (Ctypes.( !@ ) parent_commit_ptr)
in
let () = print_endline ("Parent tree res: " ^ string_of_int parent_tree_res) in
let diff_ptr =
  Ctypes.allocate (Ctypes.ptr C.Types.git_diff) (Ctypes.from_voidp C.Types.git_diff Ctypes.null)
in
let diff_opts_ptr = Ctypes.from_voidp C.Types.git_diff_options Ctypes.null in
let diff_res =
  C.Functions.git_diff_tree_to_tree diff_ptr (Ctypes.( !@ ) repo_ptr)
    (Ctypes.( !@ ) commit_tree_ptr) (Ctypes.( !@ ) parent_tree_ptr) diff_opts_ptr
in
let () = print_endline ("diff res: " ^ string_of_int diff_res) in
let cmp (_a : C.Types.git_diff_delta Ctypes.structure Ctypes_static.ptr)
    (_b : C.Types.git_diff_hunk Ctypes.structure Ctypes_static.ptr)
    (_c : C.Types.git_diff_line Ctypes.structure Ctypes_static.ptr) (_d : unit Ctypes_static.ptr) =
  0
in
let () = print_endline "kekw" in
let start = Ctypes.to_voidp (Ctypes.allocate Ctypes.string "") in
let () = print_endline "kekwiggle" in
let print_res =
  C.Functions.git_diff_print (Ctypes.( !@ ) diff_ptr) C.Types.GIT_DIFF_FORMAT_PATCH cmp start
in
let () = print_endline ("print res: " ^ string_of_int print_res) in
let _sd = C.Functions.libgit2_shutdown () in
print_endline repo_path
