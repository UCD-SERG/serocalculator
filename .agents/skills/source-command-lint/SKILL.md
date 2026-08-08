---
name: "source-command-lint"
description: "Lint all R source files and report issues"
---

# source-command-lint

Use this skill when the user asks to run the migrated source command `lint`.

## Command Template

Run `Rscript -e 'lintr::lint_package()'` to check all R source files against `.lintr.R`.

Report all lint issues grouped by file, including:

- File path and line number
- The lint rule violated
- A one-line explanation of how to fix it

If there are no issues, confirm the code is clean.
