---
description: Lint all R source files and report issues
allowed-tools:
  - Bash(Rscript -e 'lintr::lint*')
---

Run `Rscript -e 'lintr::lint_package()'` to check all R source files against `.lintr.R`.

Report all lint issues grouped by file, including:

- File path and line number
- The lint rule violated
- A one-line explanation of how to fix it

If there are no issues, confirm the code is clean.
