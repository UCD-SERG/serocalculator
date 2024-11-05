# serocalculator 1.3.0

## New features

* Added example datasets with documentation for examples and testing (#314)

* Add test for missing strata in `est.incidence.by` (#227)

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
  (advice from [preventive-r-package-care](https://indrajeetpatil.github.io/preventive-r-package-care)) (#259)
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
  - `set_id()`
  - `get_age()`
  - `get_value()`
  - `get_id()`
  - `get_age_var()`
  - `get_value_var()`
  - `get_id_var()`
  
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
