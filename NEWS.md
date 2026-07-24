# serocalculator (development version)

## New features

* `autoplot.sim_results()` gains `x_var`, `group_var`, and `color_var`
  arguments, letting users choose which columns map to the x-axis, group,
  and color aesthetics instead of the previous hardcoded `sample_size` /
  `lambda.sim` mapping.
* `graph.curve.params()` now uses the 5-parameter `ab_5p()` antibody response
  model and supports `units`-aware curve parameters. (#393)
* Added `ab_5p()`, a 5-parameter antibody response model that supports
  {units}-aware inputs (e.g. `t = units::as_units(50, "days")`), building on
  the existing `bt()` active-phase helper. CI now installs the system
  `udunits2` library on macOS and Windows so the new `units` dependency can
  compile there. (#393)
* Added `sim_pop_data_2()`, a `sim_pop_data()` alternative built on `ab_5p()`
  that simulates each simulated individual's age (`sim_age()`) and time
  since their last seroconversion (`sim_time_since_last_sc()`) directly,
  rather than simulating an infection history. Both new helper functions,
  and `sim_pop_data_2()` itself, accept `units`-aware inputs.
* `sim_pop_data_multi()` gained a `sim_function` parameter (default
  `sim_pop_data`) so callers can select `sim_pop_data_2()` instead. (#393)

## Documentation

* The documentation site now deploys multiple versions side by side, following
  `rpt`'s pattern: pushes to `main` deploy development docs to `/dev/`,
  published releases deploy stable docs to `/latest-tag/` (plus an archived
  `/vX.Y.Z/` copy), and the site root redirects to whichever was deployed most
  recently. A "Versions" navbar dropdown links between them. (#504)
* Updated the documentation site configuration to promote "Get started",
  "Reference", and "News" in the top navigation, and added a grouped
  `reference.qmd` index plus grouped sidebar reference sections that
  exclude internal-only topics.
* Updated the documentation site's dark-mode styling to match `rpt` by adding
  the same inline-code color override used there, improving contrast on the
  home page and article text.
* Added introductory lecture slides to the `methodology` vignette
  ("Estimating Incidence Rates from Cross-Sectional Serosurveys").
* Completed the measurement-noise model in the `methodology` vignette
  (multiplicative relative error), added a "Combined biological and
  measurement noise" section, and added a "Noise and never-infected
  subjects" section explaining that additive biological noise spreads a
  never-infected subject's measured response over a positive range while
  multiplicative measurement noise leaves a true zero at zero. (#561)
* Corrected the documentation of the `eps` measurement-noise parameter
  (in `example_noise_params_pk`/`example_noise_params_sees` and the
  vignettes): `eps` is the bound on the relative measurement error
  (`Unif(-eps, eps)`), not a coefficient of variation. A measured CV
  corresponds to `eps = sqrt(3) * CV`. (#563)
* Added the never-infected density under combined biological and
  measurement noise to the `methodology` vignette: the piecewise
  closed form for `y_obs = eps_b * (1 + xi)`, matching Teunis and van
  Eijkeren (2020) Equation 19 and verified to integrate to the
  never-infected probability. (#567)
* Made the never-infected term explicit in the "per-person likelihood"
  slide of the `methodology` vignette: the observed-data likelihood
  integral is now shown split into its continuous (ever-infected) and
  discrete (`T = NA`, never-infected) parts, with `p(Y=y | T=NA)`
  defined as a point mass at zero (before noise). (#567)
* Explained, in the "Biological noise" section of the `methodology`
  vignette, why the biological-noise width `nu` is estimated as the
  95th percentile of negative controls: Teunis and van Eijkeren (2020)
  show that a uniform noise model only needs to match the true noise
  distribution's width, not its exact shape, and note that this width
  is difficult to verify against a mixed (ongoing-seroresponse)
  population -- motivating estimation from a clean negative-control
  panel instead. Also noted that the specific choice of the 95th
  percentile (rather than, e.g., the 99th or the sample maximum) is an
  adopted convention, not a result derived or optimized in the paper.
  (#567)
* Added the conditional variance `Var(y_obs | y_true)` for the combined
  biological- and measurement-noise model to the `methodology` vignette,
  derived from the independent-product-variance identity and checked
  against both single-source special cases already in the vignette.
  (#571)
* Added `Var(y_obs | T=t)`, marginalizing over between-person heterogeneity
  in `y_true`, to the `methodology` vignette: derived via the law of total
  variance from the `Var(y_obs | y_true)` formula above, with the
  between-person heterogeneity term `Var(y_true | T=t)` introduced
  symbolically (it has no closed form in this framework, since
  `serodynamics` represents curve-parameter heterogeneity as an empirical
  posterior sample rather than a stated parametric distribution). Clarified
  that the longitudinal model's residual variance is constant on the log
  scale conditional on individual random effects, while random waning rates
  can induce time-varying marginal population variance that `serocalculator`
  carries forward by averaging over kinetic-parameter draws. (#571)
* Moved `f_dev0()`'s `@examples` block to a separate example file
  (`inst/examples/exm-f_dev.R`), following the convention already used by
  other functions in this package. (#393)

## Internal

* Documentation website now renders HTML (primary), docx (download link on every page), and revealjs (slides for `methodology.qmd`) formats. Fixed the HTML/revealjs output-filename collision by specifying `output-file: methodology-slides.html` for revealjs in `methodology.qmd`'s frontmatter; docx goes in `_metadata.yml` globally since `.docx` has no collision risk. (#503)
* Added Codex repository guidance and R-package workflow skills. (#574)
* `news.yaml` now calls the central
  [`d-morrison/gha`](https://github.com/d-morrison/gha) `check-news.yml@v1`
  reusable workflow instead of invoking `UCD-SERG/changelog-check-action@v2`
  directly. (#537)
* `claude.yml` and `claude-code-review.yml` now call the central
  [`d-morrison/gha`](https://github.com/d-morrison/gha) `claude.yml@v2` and
  `claude-code-review.yml@v2` reusable workflows instead of carrying their own
  copy of the agent/review machinery. (#549)
* The `methodology` vignette's LaTeX macros now come from the shared
  [`d-morrison/macros`](https://github.com/d-morrison/macros) git submodule
  (included via `{{< include ../macros/macros.qmd >}}`) instead of a local
  `vignettes/articles/_macros.qmd`. The deck adopts the shared macro
  vocabulary (e.g. `\dens` for the density function in place of the local
  `\pdf`). (#534)
* `claude-code-review.yml` now sets `allowed_bots: github-actions[bot]` so the review still runs (and posts feedback) when `claude.yml` re-dispatches it on an `@claude review` comment; previously the bot-initiated dispatch aborted with "Workflow initiated by non-human actor".
* `claude.yml` now grants the `@claude` agent the file tools (`Read`/`Glob`/`Grep`/`Edit`/`MultiEdit`/`Write`) in `--allowedTools`; previously the agent could run checks/git/gh but not edit files, so it fell back to posting diffs for manual application.
* Added the `iterate` Claude Code skill (`.claude/skills/iterate/`) for driving a PR to a clean review verdict.
* Ported the `@claude` agent and PR-review GitHub Actions workflows (plus Claude/Copilot config: `CLAUDE.md`, `.claude/` settings and slash commands, and path-scoped `.github/instructions/`) from the UCD-SERG `qwt` template, adapted to this package. (#523)
* Claude PR review workflow now skips (rather than hard-failing) when triggered by a bot (e.g. `claude[bot]` pushing a commit). (#519)
* Added the `lint-changed-lines` CI workflow (calling the reusable
  [`d-morrison/gha`](https://github.com/d-morrison/gha)
  `lint-changed-lines.yml@v2` workflow), which flags lint issues only on the
  lines a PR actually adds or modifies (rather than whole changed files, as
  `lint-changed-files` does). This lets lint rules be adopted or tightened
  incrementally as code is touched, instead of forcing a repo-wide reformat.
  Intended to replace `lint-changed-files` as the lint gate once branch
  protection is updated to require it. (#558)

## Bug fixes

* `sim_pop_data()` and `sim_pop_data_multi()` now produce identical results
  across operating systems. Simulated inter-infection times are now rounded to
  whole days, so the number of random draws consumed no longer depends on
  platform-specific floating-point results of `log()` (which previously
  shifted the random-number stream out of sync and made simulated values, and
  their snapshots, differ between macOS, Windows, and Linux). Simulated
  values change slightly as a result of this fix. (#447)
* Corrected default axis labels in `strat_ests_barplot()` (`xlab`) and
  `strat_ests_scatterplot()` (`ylab`) to say "seroincidence" rather than
  "seroconversion"/"incidence".
* `load_noise_params()` and `load_sr_params()` now fail gracefully with informative messages when internet resources are unavailable, complying with CRAN policy (#505)
* Added Version Crosswalk article to pkgdown website to help users migrate code from v1.3.0 to v1.4.0
  - Provides clear tables comparing old and new function names
  - Includes code examples showing how to update existing code
  - Accessible as a prominent tab in the website navigation

## Compatibility

* Replaced deprecated `dplyr::is.grouped_df()` usage with `dplyr::is_grouped_df()` in `df_to_array()` for compatibility with newer dplyr releases.

## New features

* Added `cluster_var` and `stratum_var` parameters to `est_seroincidence()` and 
  `est_seroincidence_by()` to support cluster-robust standard error estimation. 
  When `cluster_var` is specified, `summary.seroincidence()` automatically computes 
  cluster-robust (sandwich) variance estimates to account for within-cluster 
  correlation in clustered sampling designs such as household or school-based surveys.
* `cluster_var` parameter now accepts multiple variables (e.g., `c("school", "classroom")`)
  for multi-level clustered sampling designs. Cluster-robust standard errors will account
  for all specified clustering levels.

## Bug fixes

* Fixed column naming issue in `summary.seroincidence()` where cluster-robust standard
  errors caused `[]` notation in column names (`SE[,1]` instead of `SE`).
* Added `se_type` column to `summary.seroincidence()` output to clearly indicate whether
  "standard" or "cluster-robust" standard errors are being used.
* Fixed `est_seroincidence_by()` to properly pass cluster and stratum variables through
  to stratified analyses. Previously, these variables were dropped during data stratification,
  causing errors when trying to use clustering with `est_seroincidence_by()`.

## Code organization

* Refactored clustering-related code following package organization policies:
  - Moved `.compute_cluster_robust_var()` to `R/compute_cluster_robust_var.R`
  - Each function now in its own file for better maintainability and git history
* Updated copilot-instructions.md with code organization policies
## Dependencies

* Replaced `ggpubr` with `patchwork` for arranging multi-panel plots,
  removing the indirect `ggrepel` transitive dependency.

# serocalculator 1.4.0

## New features

* Added support for cluster-robust standard errors in `est_seroincidence()` through
  new `cluster_var` and `stratum_var` parameters. When `cluster_var` is specified,
  `summary.seroincidence()` automatically computes cluster-robust (sandwich) variance
  estimates to account for within-cluster correlation in clustered sampling designs
  such as household or school-based surveys.
* Added `compare_seroincidence()` function for statistical comparison of seroincidence rates
  - Performs two-sample z-tests to compare seroincidence estimates
  - Returns `htest` format when comparing two single estimates
  - Returns formatted table with all pairwise comparisons for stratified estimates
  - Added examples to tutorial vignette and comprehensive unit tests
* Implemented multi-version pkgdown documentation with version dropdown menu
  - Users can now switch between main, latest-tag, and versioned releases
  - Default landing page shows latest-tag (most recent release)
  - Based on insightsengineering/r-pkgdown-multiversion setup
* Added `chain_color` option to `graph.curve.params()` to control MCMC line color (#455)
* Made `graph.curve.params()` the default sub-method for `autoplot.curve_params()` (#450)
* Added `log_x` and `log_y` options to `graph.curve.params()` sub-method for 
`autoplot.curve_params()` (#453)
* Extended `sim_pop_data_multi()` to loop over multiple sample sizes (#444)
* Added new functions `analyze_sims()` and `autoplot.sim_results()` (#444)
* Rename `estimate_scr()` to `est_seroincidence_by()` (#439)
* Rename `estimate_scr()` to `est_seroincidence()` (#432)
* Rename argument `curve_params` to `sr_params` for estimation functions (#424)
* added documentation for `count_strata()` (#431)
* Rename  `as_curve_params()` to `as_sr_params()` (#421)
* Rename `load_curve_params()` to `load_sr_params()` (#421)
* added default for `xvar` in `"scatter"` option for `autoplot.seroincidence.by()` (#417)
* Extended `autoplot.summary.seroincidence.by()` to include types for either scatter or bar plots of stratified results (#397)
* added option to add lines using `group_var` input to `autoplot.summary.seroincidence.by()` (#410)
* `autoplot.pop_data(type = "age-scatter")` now shows legend at bottom (#407)
* `autoplot.pop_data(type = "age-scatter")` now facets by antigen isotype (#406)
* Rename `est.incidence.by()` to `estimate_scr_by()` (#389)
* Rename `est.incidence()` to `estimate_scr()` (#389)
* Improved warning messages for `get_biomarker_names_var()`
* Added `get_*()` extractor functions to API (#380)
* Added optional CI error bars to `autoplot.summary.seroincidence.by()` (#372)
* Improved y-limit calculation in `graph.curve.params()` (#368)
* Added option for `graph.curve.params()` to show all curves (#368)
* Added color-coding for `graph.curve.params()` (#383)
* Added `quantiles` parameter to `graph.curve.params()` and corresponding test in `test-graph.curve.params.R` (#434)
* Removed `warn.missing.strata()` from API (#366)

* Added more details about contributing PRs in `Contributing.md` (#280)

* Added warnings for missing biomarker data (#168):
  - completely missing antigen-isotype in a stratum
  - uneven antigen-isotype counts in a stratum (likely from incomplete data)

* Split dev and release websites into:
   - release: https://ucd-serg.github.io/serocalculator/
   - dev: https://ucd-serg.github.io/serocalculator/dev/

* Fixed citations in `methodology.qmd` article (#360)

* Added outline to pkgdown website (#353)
* Added verbose option for `summary.seroincidence()` and 
`summary.seroincidence.by()` (#348)
* Extended `simulate_xsectionalData.Rmd` article to explore
`renew_params = TRUE` vs `renew_params = FALSE` (#348)

* Renamed variables for consistency (#281, #373):
  - `sim.cs()` -> `sim_pop_data()` 
  - `sim.cs.multi()` -> `sim_pop_data_multi()`

## Bug fixes

* Fixed CRAN errors (#464)
* Fixed stratification issue in enteric fever vignette (#418)
* Fixed issue in `graph.curve.params()` where MCMC samples 
with the same iteration number from different MCMC chains
would get merged by `ggplot2::aes(group = iter)` (#382)

## Internal changes

* switched `expect_snapshot_data()` to an internal function due to CRAN errors (#464)
* generalized `ab1()`
* added codecov/test-results-action to test-coverage.yaml workflow
* added test for censored data in f_dev() (#399)
* added test for `autoplot.curve_params()`
* added test for `graph.curve.params()` (#368)
* reverted Readme source file from qmd to Rmd.
* switched pkgdown GHA from `any::pkgdown` to `r-lib/pkgdown` (i.e., dev version) (#359)
* added test for `summary.seroincidence.by()` (#352)
* Started checking for use of base pipe instead of magrittr pipe
by linter (#347)
* Removed `ldpar()` from API (#345)
* Added test for `sim.cs()` (#344)
* Added test for internal function `ab()` (#342)

* Reverted name change `ldpar()`-> `row_longitudinal_parameter()` (#343)

# serocalculator 1.3.0

## New features

* Removed function 'get_additional_data()' (#332)

* Updated documentation examples to include csv files (#328)

* Added csv files for use in documentation examples (#329)

* Added `serocalculator_example()` function to help locate example data files (#329)

* Fixed a bug in computing the antibody response curve when $r=1$ (#323)

* Added example datasets with documentation for examples and testing (#314)

* Improved error messaging for `autoplot.pop_data()` (#234).

* Clarified package installation instructions in scrub typhus vignette (#234).

* Add `as_noise_params` (#228) 

* Updated `simulate_xsectionalData.Rmd` (linting, removing deprecated functions)
(#289)

* Added default value for `antigen_isos` argument in `log_likelihood()` (#286)

* Updated enteric fever example article with upgraded code and visualizations (#290)

* Added `Methodology` vignette (#284, #302, #303)

* Added template for reporting Issues 
(from `usethis::use_tidy_issue_template()`) (#270)

* Added template for pull requests 
(from <https://github.com/bcgov/ssdtools>) (#265)

## Internal changes
* Updated documentation to align with previous CRAN feedback (#328)

* Updated tests to use internal testing datasets instead of external links (#328)

* Updated `test-coverage.yml` GHA action to current `r-lib` standard (#330)

* Change default pipe setting (#312)

* Add test for missing strata in `est.incidence.by` (#227)
* Added `snapshot_value` test for `est.incidence()` (#315)

* Sped up `lint-changed-files` GitHub Action (#317)

* Added online preview builds for PRs that change the `pkgdown` website (#309)

* Added `test-autoplot.pop_data` test (#234)

* initialized [`lintr`](https://lintr.r-lib.org/) with `lintr::use_lint()` (#278)

* created unit test for `df_to_array()` (#276)

* fixed `dplyr::select()` deprecation warning in `df_to_array()` (#276)

* Added `devtag` to package (using `devtag::use_devtag()`) (#292)

* Added `@dev` tag to `?df_to_array()` (#292)

* Generalized `get_()` and `set_()` methods to be general-purpose
(no S3 class-specific methods needed yet) (#274).

* Updated GitHub Action files and reformatted `DESCRIPTION` (#268)
* Added `.gitattributes` file (<https://git-scm.com/docs/gitattributes>)
copied from <https://github.com/tidyverse/ggplot2>

* Added QR code to `README.qmd`
* Added additional automated checks through 
[GitHub actions](https://docs.github.com/en/actions), 
including:
  - check that `README.qmd` still compiles 
  (advice from [preventive-r-package-care](https://indrajeetpatil.github.io/preventive-r-package-care/#/preventive-care-for-r-packages)) (#259)
  - check `NEWS.md` for updated change log (#257)
  - lint changed files (#256)

# serocalculator 1.2.0

* Added `test-summary.pop_data` test

* Modified `test-est.incidence` test

* Added stratification to `summary.pop_data`

* Added `verbose` option for `check_pop_data()`, changing default behavior
to avoid printing an OK message.

# serocalculator 1.1.0

* Renamed `llik()` to `log_likelihood()`

* Renamed `fdev()` to `f_dev()`

* Renamed `df.to.array()` to `df_to_array()`

* Renamed `getAdditionalData()` to `get_additional_data()`

* Removed `clean_pop_data()` function

* Remove `clean_pop_data()` dependency functions documentation examples

* Added `age`, `value`, `id` and `standardize` arguments to `load_pop_data()`

* Added the following methods to `pop_data` class:

  - `set_age()`
  - `set_value()`
  - `set_id_var()`
  - `get_age()`
  - `get_values()`
  - `ids()`
  - `get_age_var()`
  - `get_values_var()`
  - `ids_varname()`
  
* Added additional warnings to `load_pop_data()`

* Added `scales::pseudo_log_trans()` to `autoplot.pop_data()` to avoid log 0

* Added `test-est.incidence-status.R` test to check output when `standardize` option is FALSE on `load_pop_data()`

* Replaced column name comparison on `check_pop_data()` to use attribute name on `pop_data` class

# serocalculator 1.0.1

* added `n_points` argument to `plot_curve_params_one_ab()`
* Added `type = "age-scatter"` option for `autoplot.pop_data()`

## serocalculator 1.0.0

* Moved underlying methods to `serocalculator` vignette

## serocalculator 0.5.0

* Spell-checking of function documentation and tutorial articles.

* Added functions and methods:

  - `load_pop_data()`
  - `check_pop_data()`
  - `summary.pop_data()`
  - `autoplot.pop_data()`
  - `load_curve_params()`

* Renamed `graph.decay.curves.by()` to `autoplot.curve_params()`

## serocalculator 0.4.0

* `plot()` methods have been renamed to `autoplot()`, matching general convention for `ggplot2`-based graphics.

* added visualization of curve parameters

* `sim.cs()` now has `format` argument to specify long or wide format for output.

### serocalculator 0.3.2

Fixed bug in passing `antigen_isos` from `est.incidence.by()` to `est.incidence()`.

### serocalculator 0.3.1

Rolled back required R version from 4.2 to 4.1

## serocalculator 0.3.0

* Fixed stability and documentation-clarity issues after initial tester feedback.

## serocalculator 0.2.0 (never formally incremented in DESCRIPTION)

* Added new vignettes.

## serocalculator 0.1.0

Forking from the seroincidence package and adding Teunis et al 2020 approach.
