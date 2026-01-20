# serocalculator (development version)

## New features

* Added `compute_icc()` function to calculate the Intraclass Correlation Coefficient
  (ICC) for seroincidence estimates from clustered sampling designs. The ICC measures
  the proportion of variance due to between-cluster variation. Now works with both
  `seroincidence` and `seroincidence.by` objects, calculating ICC for each stratum
  when used with stratified analyses.
* Added `cluster_var` and `stratum_var` parameters to `est_seroincidence_by()` to
  support cluster-robust standard error estimation in stratified analyses.
* `compute_icc()` output now includes antigen isotypes information to clarify which
  antibodies were used in the analysis.
* `compute_icc()` output now includes minimum and maximum cluster sizes in addition
  to the average, providing more complete information about cluster size distribution.
  The print method now clarifies that cluster size refers to "observations per cluster".
* `cluster_var` parameter now accepts multiple variables (e.g., `c("school", "classroom")`)
  for multi-level clustered sampling designs. Cluster-robust standard errors will account
  for all specified clustering levels. Note: ICC calculation only supports single-level
  clustering and will produce an error if multiple cluster variables are provided.

## Bug fixes

* Fixed column naming issue in `summary.seroincidence()` where cluster-robust standard
  errors caused `[]` notation in column names (`SE[,1]` instead of `SE`).
* Added `se_type` column to `summary.seroincidence()` output to clearly indicate whether
  "standard" or "cluster-robust" standard errors are being used.
* Fixed `est_seroincidence_by()` to properly pass cluster and stratum variables through
  to stratified analyses. Previously, these variables were dropped during data stratification,
  causing errors when trying to use clustering with `est_seroincidence_by()`.

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
