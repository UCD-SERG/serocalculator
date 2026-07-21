---
name: "source-command-test"
description: "Run the testthat suite and report failures"
---

# source-command-test

Use this skill when the user asks to run the migrated source command `test`.

## Command Template

Run `Rscript -e 'devtools::test()'` to run the testthat suite.

Report any failures or warnings grouped by test file, with the expectation that
failed and the likely cause. If snapshot tests changed, note which snapshots
differ and whether the change looks intentional (don't blindly accept snapshot
updates). If everything passes, say so.
