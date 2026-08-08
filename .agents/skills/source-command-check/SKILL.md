---
name: "source-command-check"
description: "Run the R package check suite and report ERRORs/WARNINGs/NOTEs"
---

# source-command-check

Use this skill when the user asks to run the migrated source command `check`.

## Command Template

Run `Rscript -e 'devtools::check()'` to run the full R package check suite.

Report any ERRORs, WARNINGs, or NOTEs grouped by severity. For each issue explain
what it means and suggest a fix. If the check passes cleanly, say so.
