* Added `type = "age-scatter"` option for `autoplot.pop_data()`

## serocalculator 1.0.0

* Moved underlying methods to `serocalculator` vignette

## serocalculator 0.5.0

* Spell-checking of function documentation and tutorial articles.

* Added functions and methods:

  - `load_pop_data()`
  - `check_pop_data()`
  - `clean_pop_data()`
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
