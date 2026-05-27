---
description: Regenerate roxygen2 documentation and report what changed
allowed-tools:
  - Bash(Rscript -e 'devtools::document*')
  - Bash(git status)
  - Bash(git diff:*)
---

Run `Rscript -e 'devtools::document()'` to regenerate `man/`, `NAMESPACE`, and
the `DESCRIPTION` collate field from roxygen2 comments.

Then run `git status` / `git diff` and report which generated files changed. If
nothing changed, confirm the documentation was already in sync (this is what the
`R-check-docs.yml` CI workflow verifies). Remind the user to commit the
regenerated files.
