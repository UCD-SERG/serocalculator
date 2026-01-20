# Version Crosswalk: v1.3.0 to v1.4.0

## Overview

This guide helps users migrate code from **serocalculator v1.3.0** to
**v1.4.0**. The main changes are function renamings for improved clarity
and consistency.

**If you have existing code using v1.3.0, use this guide to update your
function calls.**

## Quick Reference Table

### Main Estimation Functions

| v1.3.0 Function                                                                                 | v1.4.0 Function                                                                                         | What it does                                |
|-------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------|---------------------------------------------|
| [`est.incidence()`](https://ucd-serg.github.io/serocalculator/reference/est.incidence.md)       | [`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence.md)       | Estimate seroincidence for entire dataset   |
| [`est.incidence.by()`](https://ucd-serg.github.io/serocalculator/reference/est.incidence.by.md) | [`est_seroincidence_by()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence_by.md) | Estimate seroincidence stratified by groups |

**Note:** The intermediate names `estimate_scr()` and
`estimate_scr_by()` were used briefly in early v1.4.0 development but
replaced with the final names shown above.

### Data Preparation Functions

| v1.3.0 Function                                                                                   | v1.4.0 Function                                                                                     | What it does                               |
|---------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------|--------------------------------------------|
| [`load_curve_params()`](https://ucd-serg.github.io/serocalculator/reference/load_curve_params.md) | [`load_sr_params()`](https://ucd-serg.github.io/serocalculator/reference/load_sr_params.md)         | Load seroresponse curve parameters         |
| [`as_curve_params()`](https://ucd-serg.github.io/serocalculator/reference/as_curve_params.md)     | [`as_sr_params()`](https://ucd-serg.github.io/serocalculator/reference/as_sr_params.md)             | Convert data to seroresponse parameters    |
| [`sim.cs()`](https://ucd-serg.github.io/serocalculator/reference/sim.cs.md)                       | [`sim_pop_data()`](https://ucd-serg.github.io/serocalculator/reference/sim_pop_data.md)             | Simulate cross-sectional population data   |
| [`sim.cs.multi()`](https://ucd-serg.github.io/serocalculator/reference/sim.cs.multi.md)           | [`sim_pop_data_multi()`](https://ucd-serg.github.io/serocalculator/reference/sim_pop_data_multi.md) | Simulate multiple cross-sectional datasets |

### Function Arguments

| v1.3.0 Argument | v1.4.0 Argument | Used in functions                                                                                                                                                                                                                 |
|-----------------|-----------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `curve_params`  | `sr_params`     | [`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence.md), [`est_seroincidence_by()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence_by.md), and related functions |

## Code Examples

### Example 1: Basic Seroincidence Estimation

**Old code (v1.3.0):**

``` r
# Load curve parameters
curve_params <- load_curve_params("parameters.csv")

# Estimate seroincidence
results <- est.incidence(
  pop_data = my_data,
  curve_params = curve_params
)
```

**New code (v1.4.0):**

``` r
# Load seroresponse parameters
sr_params <- load_sr_params("parameters.csv")

# Estimate seroincidence
results <- est_seroincidence(
  pop_data = my_data,
  sr_params = sr_params
)
```

### Example 2: Stratified Analysis

**Old code (v1.3.0):**

``` r
# Stratified estimation
results_by_group <- est.incidence.by(
  pop_data = my_data,
  curve_params = curve_params,
  strata = c("age_group", "location")
)
```

**New code (v1.4.0):**

``` r
# Stratified estimation
results_by_group <- est_seroincidence_by(
  pop_data = my_data,
  sr_params = sr_params,
  strata = c("age_group", "location")
)
```

### Example 3: Simulating Data

**Old code (v1.3.0):**

``` r
# Simulate cross-sectional data
simulated_data <- sim.cs(
  curve_params = curve_params,
  n = 100
)

# Simulate multiple datasets
multiple_sims <- sim.cs.multi(
  curve_params = curve_params,
  n = 100,
  n_reps = 10
)
```

**New code (v1.4.0):**

``` r
# Simulate cross-sectional data
simulated_data <- sim_pop_data(
  sr_params = sr_params,
  n = 100
)

# Simulate multiple datasets
multiple_sims <- sim_pop_data_multi(
  sr_params = sr_params,
  n = 100,
  n_reps = 10
)
```

## New Features in v1.4.0

In addition to the renamed functions, v1.4.0 includes several new
features:

- **[`compare_seroincidence()`](https://ucd-serg.github.io/serocalculator/reference/compare_seroincidence.md)**:
  New function for statistical comparison of seroincidence rates between
  groups
- **Enhanced plotting**: Additional options for
  [`autoplot.curve_params()`](https://ucd-serg.github.io/serocalculator/reference/autoplot.curve_params.md)
  including `log_x`, `log_y`, and `chain_color`
- **Extended simulation analysis**: New
  [`analyze_sims()`](https://ucd-serg.github.io/serocalculator/reference/analyze_sims.md)
  and
  [`autoplot.sim_results()`](https://ucd-serg.github.io/serocalculator/reference/autoplot.sim_results.md)
  functions
- **Improved documentation**: Multi-version pkgdown documentation with
  version dropdown

## Other Notable Changes

- The package now uses snake_case naming consistently throughout
- Argument `curve_params` is now `sr_params` (short for “seroresponse
  parameters”) in all functions
- Many internal improvements to error messages and warnings

## Need More Help?

- See the [NEWS
  file](https://ucd-serg.github.io/serocalculator/news/index.html) for
  complete details
- Check the [main
  tutorial](https://ucd-serg.github.io/serocalculator/articles/serocalculator.html)
  for comprehensive examples using v1.4.0 syntax
- Visit the [function
  reference](https://ucd-serg.github.io/serocalculator/reference/index.html)
  for detailed documentation

## Summary

The transition from v1.3.0 to v1.4.0 primarily involves:

1.  Replace dots in function names with underscores (e.g.,
    [`est.incidence()`](https://ucd-serg.github.io/serocalculator/reference/est.incidence.md)
    →
    [`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence.md))
2.  Update `curve_params` arguments to `sr_params`
3.  Use the new
    [`load_sr_params()`](https://ucd-serg.github.io/serocalculator/reference/load_sr_params.md)
    and
    [`as_sr_params()`](https://ucd-serg.github.io/serocalculator/reference/as_sr_params.md)
    instead of the `_curve_params` versions

**Quick search-and-replace tips:**

- `est.incidence.by(` → `est_seroincidence_by(`
- `est.incidence(` → `est_seroincidence(`
- `load_curve_params(` → `load_sr_params(`
- `as_curve_params(` → `as_sr_params(`
- `sim.cs.multi(` → `sim_pop_data_multi(`
- `sim.cs(` → `sim_pop_data(`
- `curve_params =` → `sr_params =`
