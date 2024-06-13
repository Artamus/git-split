open Ctypes

module Types (F : Ctypes.TYPE) = struct
  open F

  type git_repository
  type git_object
  type git_oid
  type git_commit
  type git_tree
  type git_diff
  type git_diff_options
  type git_diff_delta
  type git_diff_hunk
  type git_diff_line
  type git_diff_file

  type git_diff_format =
    | GIT_DIFF_FORMAT_PATCH
    | GIT_DIFF_FORMAT_PATCH_HEADER
    | GIT_DIFF_FORMAT_RAW
    | GIT_DIFF_FORMAT_NAME_ONLY
    | GIT_DIFF_FORMAT_NAME_STATUS
    | GIT_DIFF_FORMAT_PATCH_ID

  type git_delta =
    | GIT_DELTA_UNMODIFIED
    | GIT_DELTA_ADDED
    | GIT_DELTA_DELETED
    | GIT_DELTA_MODIFIED
    | GIT_DELTA_RENAMED
    | GIT_DELTA_COPIED
    | GIT_DELTA_IGNORED
    | GIT_DELTA_UNTRACKED
    | GIT_DELTA_TYPECHANGE
    | GIT_DELTA_UNREADABLE
    | GIT_DELTA_CONFLICTED

  let git_repository : git_repository structure typ = structure "git_repository"
  let git_object : git_object structure typ = structure "git_object"
  let git_oid : git_oid structure typ = structure "git_oid"
  let id = field git_oid "id" (array 20 char)
  let () = seal git_oid
  let git_commit : git_commit structure typ = structure "git_commit"
  let git_tree : git_tree structure typ = structure "git_tree"
  let git_diff : git_diff structure typ = structure "git_diff"
  let git_diff_options : git_diff_options structure typ = structure "git_diff_options"

  let git_diff_format : git_diff_format typ =
    enum "git_diff_format_t"
      [
        (GIT_DIFF_FORMAT_PATCH, constant "GIT_DIFF_FORMAT_PATCH" int64_t);
        (GIT_DIFF_FORMAT_PATCH_HEADER, constant "GIT_DIFF_FORMAT_PATCH_HEADER" int64_t);
        (GIT_DIFF_FORMAT_RAW, constant "GIT_DIFF_FORMAT_RAW" int64_t);
        (GIT_DIFF_FORMAT_NAME_ONLY, constant "GIT_DIFF_FORMAT_NAME_ONLY" int64_t);
        (GIT_DIFF_FORMAT_NAME_STATUS, constant "GIT_DIFF_FORMAT_NAME_STATUS" int64_t);
        (GIT_DIFF_FORMAT_PATCH_ID, constant "GIT_DIFF_FORMAT_PATCH_ID" int64_t);
      ]
      ~unexpected:(fun _ -> GIT_DIFF_FORMAT_PATCH)
      ~typedef:true

  let git_delta : git_delta typ =
    enum "git_delta_t"
      [
        (GIT_DELTA_UNMODIFIED, constant "GIT_DELTA_UNMODIFIED" int64_t);
        (GIT_DELTA_ADDED, constant "GIT_DELTA_ADDED" int64_t);
        (GIT_DELTA_DELETED, constant "GIT_DELTA_DELETED" int64_t);
        (GIT_DELTA_MODIFIED, constant "GIT_DELTA_MODIFIED" int64_t);
        (GIT_DELTA_RENAMED, constant "GIT_DELTA_RENAMED" int64_t);
        (GIT_DELTA_COPIED, constant "GIT_DELTA_COPIED" int64_t);
        (GIT_DELTA_IGNORED, constant "GIT_DELTA_IGNORED" int64_t);
        (GIT_DELTA_UNTRACKED, constant "GIT_DELTA_UNTRACKED" int64_t);
        (GIT_DELTA_TYPECHANGE, constant "GIT_DELTA_TYPECHANGE" int64_t);
        (GIT_DELTA_UNREADABLE, constant "GIT_DELTA_UNREADABLE" int64_t);
        (GIT_DELTA_CONFLICTED, constant "GIT_DELTA_CONFLICTED" int64_t);
      ]
      ~unexpected:(fun _ -> GIT_DELTA_UNREADABLE)
      ~typedef:true

  (* let git_diff_file : git_diff_file structure typ = structure "git_diff_file" *)

  let git_diff_delta : git_diff_delta structure typ = structure "git_diff_delta"

  (* let status = field git_diff_delta "status" git_delta
     let flags = field git_diff_delta "flags" uint32_t
     let similarity = field git_diff_delta "similarity" uint32_t
     let nfiles = field git_diff_delta "nfiles" uint32_t
     let old_file = field git_diff_delta "old_file" () *)
  let git_diff_hunk : git_diff_hunk structure typ = structure "git_diff_hunk"
  let git_diff_line : git_diff_line structure typ = structure "git_diff_line"
end
