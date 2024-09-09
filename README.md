# git split

TUI-based commit splitting tool for git.

## Why?

For those of us who like keeping their commit history clean, sometimes it may happen that a single commit ends up containing more functionality than we'd like. For these cases the main solution is to do a soft reset on the commit and start using `git add -p` to pick what you want in your commit, leaving the rest for another commit. That tool operates mostly on a hunk-by-hunk basis and when you'd like to go for line-by-line adding, the UX gets pretty bad, which led me to build this.

## How?

This tool works by parsing the diff of the commit you are trying to split and re-assembling a diff after confirming the lines you want to keep. 

Another option would have been to keep an internal "set" of changes and assemble the diffs in memory until the unbound changeset is empty, but I deemed that to be overall a worse UX.

## Demo

![](https://github.com/Artamus/git-split/blob/main/demo.gif)

## NB!

The goal of this project was to learn OCaml, so if you are an OCaml expert reading this and despairing at the code quality, feel free to open a discussion with improvement suggestions.
