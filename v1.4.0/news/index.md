# Changelog

## serocalculator 1.4.0

CRAN release: 2025-12-11

### New features

- Added `chain_color` option to
  [`graph.curve.params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/graph.curve.params.md)
  to control MCMC line color
  ([\#455](https://github.com/UCD-SERG/serocalculator/issues/455))

- Made
  [`graph.curve.params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/graph.curve.params.md)
  the default sub-method for
  [`autoplot.curve_params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/autoplot.curve_params.md)
  ([\#450](https://github.com/UCD-SERG/serocalculator/issues/450))

- Added `log_x` and `log_y` options to
  [`graph.curve.params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/graph.curve.params.md)
  sub-method for
  [`autoplot.curve_params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/autoplot.curve_params.md)
  ([\#453](https://github.com/UCD-SERG/serocalculator/issues/453))

- Extended
  [`sim_pop_data_multi()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/sim_pop_data_multi.md)
  to loop over multiple sample sizes
  ([\#444](https://github.com/UCD-SERG/serocalculator/issues/444))

- Added new functions
  [`analyze_sims()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/analyze_sims.md)
  and
  [`autoplot.sim_results()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/autoplot.sim_results.md)
  ([\#444](https://github.com/UCD-SERG/serocalculator/issues/444))

- Rename `estimate_scr()` to
  [`est_seroincidence_by()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/est_seroincidence_by.md)
  ([\#439](https://github.com/UCD-SERG/serocalculator/issues/439))

- Rename `estimate_scr()` to
  [`est_seroincidence()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/est_seroincidence.md)
  ([\#432](https://github.com/UCD-SERG/serocalculator/issues/432))

- Rename argument `curve_params` to `sr_params` for estimation functions
  ([\#424](https://github.com/UCD-SERG/serocalculator/issues/424))

- added documentation for
  [`count_strata()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/count_strata.md)
  ([\#431](https://github.com/UCD-SERG/serocalculator/issues/431))

- Rename
  [`as_curve_params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/as_curve_params.md)
  to
  [`as_sr_params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/as_sr_params.md)
  ([\#421](https://github.com/UCD-SERG/serocalculator/issues/421))

- Rename
  [`load_curve_params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/load_curve_params.md)
  to
  [`load_sr_params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/load_sr_params.md)
  ([\#421](https://github.com/UCD-SERG/serocalculator/issues/421))

- added default for `xvar` in `"scatter"` option for
  [`autoplot.seroincidence.by()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/autoplot.seroincidence.by.md)
  ([\#417](https://github.com/UCD-SERG/serocalculator/issues/417))

- Extended
  [`autoplot.summary.seroincidence.by()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/autoplot.summary.seroincidence.by.md)
  to include types for either scatter or bar plots of stratified results
  ([\#397](https://github.com/UCD-SERG/serocalculator/issues/397))

- added option to add lines using `group_var` input to
  [`autoplot.summary.seroincidence.by()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/autoplot.summary.seroincidence.by.md)
  ([\#410](https://github.com/UCD-SERG/serocalculator/issues/410))

- `autoplot.pop_data(type = "age-scatter")` now shows legend at bottom
  ([\#407](https://github.com/UCD-SERG/serocalculator/issues/407))

- `autoplot.pop_data(type = "age-scatter")` now facets by antigen
  isotype
  ([\#406](https://github.com/UCD-SERG/serocalculator/issues/406))

- Rename
  [`est.incidence.by()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/est.incidence.by.md)
  to `estimate_scr_by()`
  ([\#389](https://github.com/UCD-SERG/serocalculator/issues/389))

- Rename
  [`est.incidence()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/est.incidence.md)
  to `estimate_scr()`
  ([\#389](https://github.com/UCD-SERG/serocalculator/issues/389))

- Improved warning messages for
  [`get_biomarker_names_var()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/get_biomarker_names_var.md)

- Added `get_*()` extractor functions to API
  ([\#380](https://github.com/UCD-SERG/serocalculator/issues/380))

- Added optional CI error bars to
  [`autoplot.summary.seroincidence.by()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/autoplot.summary.seroincidence.by.md)
  ([\#372](https://github.com/UCD-SERG/serocalculator/issues/372))

- Improved y-limit calculation in
  [`graph.curve.params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/graph.curve.params.md)
  ([\#368](https://github.com/UCD-SERG/serocalculator/issues/368))

- Added option for
  [`graph.curve.params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/graph.curve.params.md)
  to show all curves
  ([\#368](https://github.com/UCD-SERG/serocalculator/issues/368))

- Added color-coding for
  [`graph.curve.params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/graph.curve.params.md)
  ([\#383](https://github.com/UCD-SERG/serocalculator/issues/383))

- Added `quantiles` parameter to
  [`graph.curve.params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/graph.curve.params.md)
  and corresponding test in `test-graph.curve.params.R`
  ([\#434](https://github.com/UCD-SERG/serocalculator/issues/434))

- Removed `warn.missing.strata()` from API
  ([\#366](https://github.com/UCD-SERG/serocalculator/issues/366))

- Added more details about contributing PRs in `Contributing.md`
  ([\#280](https://github.com/UCD-SERG/serocalculator/issues/280))

- Added warnings for missing biomarker data
  ([\#168](https://github.com/UCD-SERG/serocalculator/issues/168)):

  - completely missing antigen-isotype in a stratum
  - uneven antigen-isotype counts in a stratum (likely from incomplete
    data)

- Split dev and release websites into:

  - release: <https://ucd-serg.github.io/serocalculator/>
  - dev: <https://ucd-serg.github.io/serocalculator/dev/>

- Fixed citations in `methodology.qmd` article
  ([\#360](https://github.com/UCD-SERG/serocalculator/issues/360))

- Added outline to pkgdown website
  ([\#353](https://github.com/UCD-SERG/serocalculator/issues/353))

- Added verbose option for
  [`summary.seroincidence()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/summary.seroincidence.md)
  and
  [`summary.seroincidence.by()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/summary.seroincidence.by.md)
  ([\#348](https://github.com/UCD-SERG/serocalculator/issues/348))

- Extended `simulate_xsectionalData.Rmd` article to explore
  `renew_params = TRUE` vs `renew_params = FALSE`
  ([\#348](https://github.com/UCD-SERG/serocalculator/issues/348))

- Renamed variables for consistency
  ([\#281](https://github.com/UCD-SERG/serocalculator/issues/281),
  [\#373](https://github.com/UCD-SERG/serocalculator/issues/373)):

  - [`sim.cs()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/sim.cs.md)
    -\>
    [`sim_pop_data()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/sim_pop_data.md)
  - [`sim.cs.multi()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/sim.cs.multi.md)
    -\>
    [`sim_pop_data_multi()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/sim_pop_data_multi.md)

### Bug fixes

- Fixed CRAN errors
  ([\#464](https://github.com/UCD-SERG/serocalculator/issues/464))
- Fixed stratification issue in enteric fever vignette
  ([\#418](https://github.com/UCD-SERG/serocalculator/issues/418))
- Fixed issue in
  [`graph.curve.params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/graph.curve.params.md)
  where MCMC samples with the same iteration number from different MCMC
  chains would get merged by `ggplot2::aes(group = iter)`
  ([\#382](https://github.com/UCD-SERG/serocalculator/issues/382))

### Internal changes

- switched
  [`expect_snapshot_data()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/expect_snapshot_data.md)
  to an internal function due to CRAN errors
  ([\#464](https://github.com/UCD-SERG/serocalculator/issues/464))

- generalized `ab1()`

- added codecov/test-results-action to test-coverage.yaml workflow

- added test for censored data in f_dev()
  ([\#399](https://github.com/UCD-SERG/serocalculator/issues/399))

- added test for
  [`autoplot.curve_params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/autoplot.curve_params.md)

- added test for
  [`graph.curve.params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/graph.curve.params.md)
  ([\#368](https://github.com/UCD-SERG/serocalculator/issues/368))

- reverted Readme source file from qmd to Rmd.

- switched pkgdown GHA from `any::pkgdown` to `r-lib/pkgdown` (i.e., dev
  version)
  ([\#359](https://github.com/UCD-SERG/serocalculator/issues/359))

- added test for
  [`summary.seroincidence.by()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/summary.seroincidence.by.md)
  ([\#352](https://github.com/UCD-SERG/serocalculator/issues/352))

- Started checking for use of base pipe instead of magrittr pipe by
  linter
  ([\#347](https://github.com/UCD-SERG/serocalculator/issues/347))

- Removed
  [`ldpar()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/ldpar.md)
  from API
  ([\#345](https://github.com/UCD-SERG/serocalculator/issues/345))

- Added test for
  [`sim.cs()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/sim.cs.md)
  ([\#344](https://github.com/UCD-SERG/serocalculator/issues/344))

- Added test for internal function
  [`ab()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/ab.md)
  ([\#342](https://github.com/UCD-SERG/serocalculator/issues/342))

- Reverted name change
  [`ldpar()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/ldpar.md)-\>
  `row_longitudinal_parameter()`
  ([\#343](https://github.com/UCD-SERG/serocalculator/issues/343))

## serocalculator 1.3.0

CRAN release: 2025-01-25

### New features

- Removed function ‘get_additional_data()’
  ([\#332](https://github.com/UCD-SERG/serocalculator/issues/332))

- Updated documentation examples to include csv files
  ([\#328](https://github.com/UCD-SERG/serocalculator/issues/328))

- Added csv files for use in documentation examples
  ([\#329](https://github.com/UCD-SERG/serocalculator/issues/329))

- Added
  [`serocalculator_example()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/serocalculator_example.md)
  function to help locate example data files
  ([\#329](https://github.com/UCD-SERG/serocalculator/issues/329))

- Fixed a bug in computing the antibody response curve when $r = 1$
  ([\#323](https://github.com/UCD-SERG/serocalculator/issues/323))

- Added example datasets with documentation for examples and testing
  ([\#314](https://github.com/UCD-SERG/serocalculator/issues/314))

- Improved error messaging for
  [`autoplot.pop_data()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/autoplot.pop_data.md)
  ([\#234](https://github.com/UCD-SERG/serocalculator/issues/234)).

- Clarified package installation instructions in scrub typhus vignette
  ([\#234](https://github.com/UCD-SERG/serocalculator/issues/234)).

- Add `as_noise_params`
  ([\#228](https://github.com/UCD-SERG/serocalculator/issues/228))

- Updated `simulate_xsectionalData.Rmd` (linting, removing deprecated
  functions)
  ([\#289](https://github.com/UCD-SERG/serocalculator/issues/289))

- Added default value for `antigen_isos` argument in
  [`log_likelihood()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/log_likelihood.md)
  ([\#286](https://github.com/UCD-SERG/serocalculator/issues/286))

- Updated enteric fever example article with upgraded code and
  visualizations
  ([\#290](https://github.com/UCD-SERG/serocalculator/issues/290))

- Added `Methodology` vignette
  ([\#284](https://github.com/UCD-SERG/serocalculator/issues/284),
  [\#302](https://github.com/UCD-SERG/serocalculator/issues/302),
  [\#303](https://github.com/UCD-SERG/serocalculator/issues/303))

- Added template for reporting Issues (from
  [`usethis::use_tidy_issue_template()`](https://usethis.r-lib.org/reference/tidyverse.html))
  ([\#270](https://github.com/UCD-SERG/serocalculator/issues/270))

- Added template for pull requests (from
  <https://github.com/bcgov/ssdtools>)
  ([\#265](https://github.com/UCD-SERG/serocalculator/issues/265))

### Internal changes

- Updated documentation to align with previous CRAN feedback
  ([\#328](https://github.com/UCD-SERG/serocalculator/issues/328))

- Updated tests to use internal testing datasets instead of external
  links ([\#328](https://github.com/UCD-SERG/serocalculator/issues/328))

- Updated `test-coverage.yml` GHA action to current `r-lib` standard
  ([\#330](https://github.com/UCD-SERG/serocalculator/issues/330))

- Change default pipe setting
  ([\#312](https://github.com/UCD-SERG/serocalculator/issues/312))

- Add test for missing strata in `est.incidence.by`
  ([\#227](https://github.com/UCD-SERG/serocalculator/issues/227))

- Added `snapshot_value` test for
  [`est.incidence()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/est.incidence.md)
  ([\#315](https://github.com/UCD-SERG/serocalculator/issues/315))

- Sped up `lint-changed-files` GitHub Action
  ([\#317](https://github.com/UCD-SERG/serocalculator/issues/317))

- Added online preview builds for PRs that change the `pkgdown` website
  ([\#309](https://github.com/UCD-SERG/serocalculator/issues/309))

- Added `test-autoplot.pop_data` test
  ([\#234](https://github.com/UCD-SERG/serocalculator/issues/234))

- initialized [`lintr`](https://lintr.r-lib.org/) with
  `lintr::use_lint()`
  ([\#278](https://github.com/UCD-SERG/serocalculator/issues/278))

- created unit test for
  [`df_to_array()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/df_to_array.md)
  ([\#276](https://github.com/UCD-SERG/serocalculator/issues/276))

- fixed
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  deprecation warning in
  [`df_to_array()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/df_to_array.md)
  ([\#276](https://github.com/UCD-SERG/serocalculator/issues/276))

- Added `devtag` to package (using `devtag::use_devtag()`)
  ([\#292](https://github.com/UCD-SERG/serocalculator/issues/292))

- Added `@dev` tag to `?df_to_array()`
  ([\#292](https://github.com/UCD-SERG/serocalculator/issues/292))

- Generalized `get_()` and `set_()` methods to be general-purpose (no S3
  class-specific methods needed yet)
  ([\#274](https://github.com/UCD-SERG/serocalculator/issues/274)).

- Updated GitHub Action files and reformatted `DESCRIPTION`
  ([\#268](https://github.com/UCD-SERG/serocalculator/issues/268))

- Added `.gitattributes` file (<https://git-scm.com/docs/gitattributes>)
  copied from <https://github.com/tidyverse/ggplot2>

- Added QR code to `README.qmd`

- Added additional automated checks through [GitHub
  actions](https://docs.github.com/en/actions), including:

  - check that `README.qmd` still compiles (advice from
    [preventive-r-package-care](https://indrajeetpatil.github.io/preventive-r-package-care/#/preventive-care-for-r-packages))
    ([\#259](https://github.com/UCD-SERG/serocalculator/issues/259))
  - check `NEWS.md` for updated change log
    ([\#257](https://github.com/UCD-SERG/serocalculator/issues/257))
  - lint changed files
    ([\#256](https://github.com/UCD-SERG/serocalculator/issues/256))

## serocalculator 1.2.0

- Added `test-summary.pop_data` test

- Modified `test-est.incidence` test

- Added stratification to `summary.pop_data`

- Added `verbose` option for
  [`check_pop_data()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/check_pop_data.md),
  changing default behavior to avoid printing an OK message.

## serocalculator 1.1.0

- Renamed
  [`llik()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/llik.md)
  to
  [`log_likelihood()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/log_likelihood.md)

- Renamed
  [`fdev()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/fdev.md)
  to
  [`f_dev()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/f_dev.md)

- Renamed
  [`df.to.array()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/df.to.array.md)
  to
  [`df_to_array()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/df_to_array.md)

- Renamed `getAdditionalData()` to `get_additional_data()`

- Removed `clean_pop_data()` function

- Remove `clean_pop_data()` dependency functions documentation examples

- Added `age`, `value`, `id` and `standardize` arguments to
  [`load_pop_data()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/load_pop_data.md)

- Added the following methods to `pop_data` class:

  - `set_age()`
  - `set_value()`
  - [`set_id_var()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/set_id_var.md)
  - `get_age()`
  - [`get_values()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/get_values.md)
  - [`ids()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/ids.md)
  - `get_age_var()`
  - [`get_values_var()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/get_values_var.md)
  - [`ids_varname()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/ids_varname.md)

- Added additional warnings to
  [`load_pop_data()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/load_pop_data.md)

- Added
  [`scales::pseudo_log_trans()`](https://scales.r-lib.org/reference/transform_log.html)
  to
  [`autoplot.pop_data()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/autoplot.pop_data.md)
  to avoid log 0

- Added `test-est.incidence-status.R` test to check output when
  `standardize` option is FALSE on
  [`load_pop_data()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/load_pop_data.md)

- Replaced column name comparison on
  [`check_pop_data()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/check_pop_data.md)
  to use attribute name on `pop_data` class

## serocalculator 1.0.1

- added `n_points` argument to
  [`plot_curve_params_one_ab()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/plot_curve_params_one_ab.md)
- Added `type = "age-scatter"` option for
  [`autoplot.pop_data()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/autoplot.pop_data.md)

### serocalculator 1.0.0

- Moved underlying methods to `serocalculator` vignette

### serocalculator 0.5.0

- Spell-checking of function documentation and tutorial articles.

- Added functions and methods:

  - [`load_pop_data()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/load_pop_data.md)
  - [`check_pop_data()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/check_pop_data.md)
  - [`summary.pop_data()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/summary.pop_data.md)
  - [`autoplot.pop_data()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/autoplot.pop_data.md)
  - [`load_curve_params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/load_curve_params.md)

- Renamed `graph.decay.curves.by()` to
  [`autoplot.curve_params()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/autoplot.curve_params.md)

### serocalculator 0.4.0

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods have
  been renamed to
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
  matching general convention for `ggplot2`-based graphics.

- added visualization of curve parameters

- [`sim.cs()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/sim.cs.md)
  now has `format` argument to specify long or wide format for output.

#### serocalculator 0.3.2

Fixed bug in passing `antigen_isos` from
[`est.incidence.by()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/est.incidence.by.md)
to
[`est.incidence()`](https:/ucd-serg.github.io/serocalculator/v1.4.0/reference/est.incidence.md).

#### serocalculator 0.3.1

Rolled back required R version from 4.2 to 4.1

### serocalculator 0.3.0

- Fixed stability and documentation-clarity issues after initial tester
  feedback.

### serocalculator 0.2.0 (never formally incremented in DESCRIPTION)

- Added new vignettes.

### serocalculator 0.1.0

Forking from the seroincidence package and adding Teunis et al 2020
approach.
