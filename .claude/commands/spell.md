---
description: Run the package spell check and report misspellings
allowed-tools:
  - Bash(Rscript -e 'spelling::spell_check*')
---

Run `Rscript -e 'spelling::spell_check_package()'` to check spelling across
documentation, vignettes, and the package description.

Report each flagged word with the file(s) it appears in. For each, judge whether
it is a genuine typo (fix it at the source) or a legitimate technical term (add
it to `inst/WORDLIST` rather than disabling the check). If nothing is flagged,
confirm spelling is clean.
