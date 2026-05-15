# Version Crosswalk: v1.3.0 to v1.4.0

## Overview

This guide helps users migrate code from **serocalculator v1.3.0** to
**v1.4.0**. The main changes are function renamings for improved clarity
and consistency.

**If you have existing code using v1.3.0, use this guide to update your
function calls.**

## Quick Reference Table

### Main Estimation Functions

| v1.3.0 Function | v1.4.0 Function | What it does |
|----|----|----|
| [`est.incidence()`](https://ucd-serg.github.io/serocalculator/reference/est.incidence.md) | [`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence.md) | Estimate seroincidence for entire dataset |
| [`est.incidence.by()`](https://ucd-serg.github.io/serocalculator/reference/est.incidence.by.md) | [`est_seroincidence_by()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence_by.md) | Estimate seroincidence stratified by groups |

> **Note**
>
> The intermediate names `estimate_scr()` and `estimate_scr_by()` were
> used briefly in early v1.4.0 development but replaced with the final
> names shown above.

### Data Preparation Functions

| v1.3.0 Function | v1.4.0 Function | What it does |
|----|----|----|
| [`load_curve_params()`](https://ucd-serg.github.io/serocalculator/reference/load_curve_params.md) | [`load_sr_params()`](https://ucd-serg.github.io/serocalculator/reference/load_sr_params.md) | Load seroresponse curve parameters |
| [`as_curve_params()`](https://ucd-serg.github.io/serocalculator/reference/as_curve_params.md) | [`as_sr_params()`](https://ucd-serg.github.io/serocalculator/reference/as_sr_params.md) | Convert data to seroresponse parameters |
| [`sim.cs()`](https://ucd-serg.github.io/serocalculator/reference/sim.cs.md) | [`sim_pop_data()`](https://ucd-serg.github.io/serocalculator/reference/sim_pop_data.md) | Simulate cross-sectional population data |
| [`sim.cs.multi()`](https://ucd-serg.github.io/serocalculator/reference/sim.cs.multi.md) | [`sim_pop_data_multi()`](https://ucd-serg.github.io/serocalculator/reference/sim_pop_data_multi.md) | Simulate multiple cross-sectional datasets |

### Function Arguments

| v1.3.0 Argument | v1.4.0 Argument | Used in functions |
|----|----|----|
| `curve_params` | `sr_params` | [`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence.md), [`est_seroincidence_by()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence_by.md) |

## Code Examples

### Example 1: Basic Seroincidence Estimation

**Old code (v1.3.0):**

``` r

# Load curve parameters (v1.3.0 - DEPRECATED)
curve_params <- load_curve_params("parameters.rds")

# Estimate seroincidence
results <- est.incidence(
  pop_data = my_data,
  curve_params = curve_params
)
```

**New code (v1.4.0):**

``` r

library(serocalculator)

# Load seroresponse parameters from RDS file
sr_params <- load_sr_params(
  serocalculator_example("example_curve_params.rds")
)

# Load example population data from CSV
pop_data <- readr::read_csv(
  serocalculator_example("example_pop_data.csv"),
  show_col_types = FALSE
) |>
  as_pop_data()

# Load noise parameters
noise_params_full <- load_noise_params(
  serocalculator_example("example_noise_params.rds")
)

# Use only the antigen-isotypes present in pop_data
antigen_isos <- unique(get_biomarker_names(pop_data))
noise_params <- noise_params_full |>
  dplyr::filter(antigen_iso %in% antigen_isos) |>
  dplyr::select(-Country, -X)

# Estimate seroincidence
results <- est_seroincidence(
  pop_data = pop_data,
  sr_params = sr_params,
  noise_params = noise_params,
  antigen_isos = antigen_isos
)

summary(results)
#> # A tibble: 1 × 11
#>   est.start incidence.rate     SE CI.lwr CI.upr se_type  coverage log.lik
#>       <dbl>          <dbl>  <dbl>  <dbl>  <dbl> <chr>       <dbl>   <dbl>
#> 1       0.1          0.166 0.0178  0.135  0.205 standard     0.95   -524.
#> # ℹ 3 more variables: iterations <int>, antigen.isos <chr>,
#> #   nlm.convergence.code <ord>
```

### Example 2: Stratified Analysis

**Old code (v1.3.0):**

``` r

# Stratified estimation (v1.3.0 - DEPRECATED)
results_by_group <- est.incidence.by(
  pop_data = my_data,
  curve_params = curve_params,
  strata = c("age_group", "location")
)
```

**New code (v1.4.0):**

``` r

# Stratified estimation by Country
results_by_group <- est_seroincidence_by(
  pop_data = pop_data,
  sr_params = sr_params,
  noise_params = noise_params,
  strata = "Country",
  antigen_isos = antigen_isos
)
#> Warning: `curve_params` is missing all strata variables and will be used unstratified.
#> ℹ To avoid this warning, specify the desired set of stratifying variables in
#>   the `curve_strata_varnames` and `noise_strata_varnames` arguments to
#>   `est_seroincidence_by()`.
#> Warning: `noise_params` is missing all strata variables and will be used unstratified.
#> ℹ To avoid this warning, specify the desired set of stratifying variables in
#>   the `curve_strata_varnames` and `noise_strata_varnames` arguments to
#>   `est_seroincidence_by()`.

summary(results_by_group)
#> Seroincidence estimated given the following setup:
#> a) Antigen isotypes   : HlyE_IgA, HlyE_IgG 
#> b) Strata       : Country 
#> 
#>  Seroincidence estimates:
#> # A tibble: 1 × 14
#>   Stratum   Country      n est.start incidence.rate     SE CI.lwr CI.upr se_type
#>   <chr>     <chr>    <int>     <dbl>          <dbl>  <dbl>  <dbl>  <dbl> <chr>  
#> 1 Stratum 1 Pakistan   100       0.1          0.166 0.0178  0.135  0.205 standa…
#> # ℹ 5 more variables: coverage <dbl>, log.lik <dbl>, iterations <int>,
#> #   antigen.isos <chr>, nlm.convergence.code <ord>
```

### Example 3: Simulating Data

**Old code (v1.3.0):**

``` r

# Simulate cross-sectional data (v1.3.0 - DEPRECATED)
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

# Biologic noise distribution for simulation
noise_limits <- matrix(
  c(0, 0.5),
  nrow = length(antigen_isos),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(antigen_isos, c("min", "max"))
)

# Simulate cross-sectional data
simulated_data <- sim_pop_data(
  curve_params = sr_params,
  noise_limits = noise_limits,
  antigen_isos = antigen_isos,
  n_samples = 100,
  add_noise = TRUE,
  format = "long"
)
#> Warning: Some dimension variables are not factors.
#> ℹ These dimensions will be ordered by first appearance.
#> ℹ Check results using `dimnames()`.

head(simulated_data)
#> # A tibble: 6 × 4
#>     age id    antigen_iso value
#>   <dbl> <chr> <chr>       <dbl>
#> 1 10.3  1     HlyE_IgA    0.720
#> 2 10.3  1     HlyE_IgG    0.777
#> 3 18.7  2     HlyE_IgA    1.37 
#> 4 18.7  2     HlyE_IgG    1.78 
#> 5  7.22 3     HlyE_IgA    1.45 
#> 6  7.22 3     HlyE_IgG    2.51

# Simulate multiple datasets
multiple_sims <- sim_pop_data_multi(
  curve_params = sr_params,
  noise_limits = noise_limits,
  antigen_isos = antigen_isos,
  sample_sizes = 100,
  nclus = 3,
  lambdas = 0.1,
  num_cores = 1,
  add_noise = TRUE,
  format = "long"
)
#> Warning: Some dimension variables are not factors.
#> ℹ These dimensions will be ordered by first appearance.
#> ℹ Check results using `dimnames()`.
#> Some dimension variables are not factors.
#> ℹ These dimensions will be ordered by first appearance.
#> ℹ Check results using `dimnames()`.
#> Some dimension variables are not factors.
#> ℹ These dimensions will be ordered by first appearance.
#> ℹ Check results using `dimnames()`.

nrow(multiple_sims)
#> [1] 600
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

- Main estimation functions
  ([`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence.md),
  [`est_seroincidence_by()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence_by.md))
  now use `sr_params` argument instead of `curve_params`
- Function names now use snake_case instead of dots (e.g.,
  [`est.incidence()`](https://ucd-serg.github.io/serocalculator/reference/est.incidence.md)
  →
  [`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence.md))
- Many internal improvements to error messages and warnings

> **Warning**
>
> **Important**: Not all functions changed their parameter names.
> Functions like
> [`sim_pop_data()`](https://ucd-serg.github.io/serocalculator/reference/sim_pop_data.md)
> and
> [`graph_loglik()`](https://ucd-serg.github.io/serocalculator/reference/graph_loglik.md)
> still use `curve_params` as the parameter name. Only the main
> estimation functions use `sr_params`.

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
2.  Update `curve_params` arguments to `sr_params` **for estimation
    functions only**
3.  Use the new
    [`load_sr_params()`](https://ucd-serg.github.io/serocalculator/reference/load_sr_params.md)
    and
    [`as_sr_params()`](https://ucd-serg.github.io/serocalculator/reference/as_sr_params.md)
    instead of the `_curve_params` versions
4.  Use `.rds` files with
    [`load_sr_params()`](https://ucd-serg.github.io/serocalculator/reference/load_sr_params.md)
    (not `.csv` files)

**Quick search-and-replace tips:**

> **Function name changes (safe for all code)**
>
> - `est.incidence.by(` → `est_seroincidence_by(`
> - `est.incidence(` → `est_seroincidence(`
> - `load_curve_params(` → `load_sr_params(`
> - `as_curve_params(` → `as_sr_params(`
> - `sim.cs.multi(` → `sim_pop_data_multi(`
> - `sim.cs(` → `sim_pop_data(`

> **Argument name changes (only for estimation functions)**
>
> Within calls to
> [`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence.md)
> and
> [`est_seroincidence_by()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence_by.md):
>
> - `curve_params =` → `sr_params =`
>
> **Do not** change `curve_params` to `sr_params` in other functions
> like
> [`sim_pop_data()`](https://ucd-serg.github.io/serocalculator/reference/sim_pop_data.md).
