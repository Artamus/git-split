# git split

TUI-based commit splitting tool for git.

## Why?

For those of us who like keeping their commit history clean, sometimes it may happen that a single commit ends up containing more functionality than we'd like. For these cases the main solution is to do a soft reset on the commit and start using `git add -p` to pick what you want in your commit, leaving the rest for another commit. That tool operates mostly on a hunk-by-hunk basis and when you'd like to go for line-by-line adding, the UX gets pretty bad, which led me to build this.
