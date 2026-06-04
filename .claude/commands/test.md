---
description: Run the testthat suite and report failures
allowed-tools:
  - Bash(Rscript -e 'devtools::test*')
---

Run `Rscript -e 'devtools::test()'` to run the testthat suite.

Report any failures or warnings grouped by test file, with the expectation that
failed and the likely cause. If snapshot tests changed, note which snapshots
differ and whether the change looks intentional (don't blindly accept snapshot
updates). If everything passes, say so.
