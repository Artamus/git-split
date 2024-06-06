let () =
  let major_ptr = Ctypes.allocate Ctypes.int 0 in
  let minor_ptr = Ctypes.allocate Ctypes.int 0 in
  let rev_ptr = Ctypes.allocate Ctypes.int 0 in
  let _res = C.Functions.libgit2_version major_ptr minor_ptr rev_ptr in
  let major = Ctypes.( !@ ) major_ptr in
  let minor = Ctypes.( !@ ) minor_ptr in
  let rev = Ctypes.( !@ ) rev_ptr in
  let bla = C.Functions.libgit2_init () in
  let asd = C.Functions.libgit2_shutdown () in
  print_endline
    (string_of_int major ^ " " ^ string_of_int minor ^ " " ^ string_of_int rev ^ " asd " ^ string_of_int bla ^ ":"
   ^ string_of_int asd)

  let () = 
    let _init = C.Functions.libgit2_init () in
    let repo_ptr = Ctypes.allocate (Ctypes.ptr C.Types.git_repository) (Ctypes.from_voidp C.Types.git_repository Ctypes.null)  in
    let repo_path = "/home/artamus/stacked-diff-test" in
    let _repo_res = C.Functions.git_repository_open repo_ptr repo_path in
    let _repo = Ctypes.(!@) repo_ptr in
    let _sd = C.Functions.libgit2_shutdown () in
    print_endline repo_path