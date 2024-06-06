open Ctypes

module Types (F : Ctypes.TYPE) = struct
  open F

  type git_repository

  let git_repository = typedef (structure "git_repository" : git_repository structure typ) "git_repository"
end
