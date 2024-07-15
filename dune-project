(lang dune 3.15)

(name git_split)

(generate_opam_files true)

(source
 (github Artamus/git-split))

(authors "Artamus")

(maintainers "Artamus")

(license LICENSE)

(package
 (name git_split)
 (synopsis "A tool to split a git commit into multiple")
 (description
  "When one commit becomes too large and needs to be split up, the general git workflow that involves resetting the commit and then using interactive staging to generate multiple commits from it can be quite cumbersome. In addition to that, the git interactive staging tool's UX can be quite bad, therefore this tools aims to provide this functionality in a nicer way.")
 (depends ocaml dune re notty feather re))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
