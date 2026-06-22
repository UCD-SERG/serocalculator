# Print Method for Seroincidence Summary Object

Custom [`print()`](https://rdrr.io/r/base/print.html) function for
"summary.seroincidence.by" objects (constructed by
[`summary.seroincidence.by()`](https://ucd-serg.github.io/serocalculator/reference/summary.seroincidence.by.md))

## Usage

``` r
# S3 method for class 'summary.seroincidence.by'
print(x, ...)
```

## Arguments

- x:

  A "summary.seroincidence.by" object (constructed by
  [`summary.seroincidence.by()`](https://ucd-serg.github.io/serocalculator/reference/summary.seroincidence.by.md))

- ...:

  Additional arguments affecting the summary produced.

## Value

an [invisible](https://rdrr.io/r/base/invisible.html) copy of input
parameter `x`

## Examples

``` r
library(dplyr)

xs_data <-
  sees_pop_data_pk_100

curve <-
  typhoid_curves_nostrat_100 |>
  filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

noise <-
  example_noise_params_pk

# estimate seroincidence
est2 <- est_seroincidence_by(
  strata = c("catchment"),
  pop_data = xs_data,
  sr_params = curve,
  noise_params = noise,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
  # num_cores = 8 # Allow for parallel processing to decrease run time
)
#> Warning: `curve_params` is missing all strata variables and will be used unstratified.
#> ℹ To avoid this warning, specify the desired set of stratifying variables in
#>   the `curve_strata_varnames` and `noise_strata_varnames` arguments to
#>   `est_seroincidence_by()`.
#> Warning: `noise_params` is missing all strata variables and will be used unstratified.
#> ℹ To avoid this warning, specify the desired set of stratifying variables in
#>   the `curve_strata_varnames` and `noise_strata_varnames` arguments to
#>   `est_seroincidence_by()`.

# calculate summary statistics for the seroincidence object
est2_summary <- summary(est2)
print(est2_summary)
#> Seroincidence estimated given the following setup:
#> a) Antigen isotypes   : HlyE_IgG, HlyE_IgA 
#> b) Strata       : catchment 
#> 
#>  Seroincidence estimates:
#> # A tibble: 2 × 14
#>   Stratum  catchment     n est.start incidence.rate     SE CI.lwr CI.upr se_type
#>   <chr>    <chr>     <int>     <dbl>          <dbl>  <dbl>  <dbl>  <dbl> <chr>  
#> 1 Stratum… aku          53       0.1          0.140 0.0216  0.104  0.189 standa…
#> 2 Stratum… kgh          47       0.1          0.200 0.0301  0.149  0.268 standa…
#> # ℹ 5 more variables: coverage <dbl>, log.lik <dbl>, iterations <int>,
#> #   antigen.isos <chr>, nlm.convergence.code <ord>
```
