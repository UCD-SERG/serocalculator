# Summary Method for `"seroincidence.by"` Objects

Calculate seroincidence from output of the seroincidence calculator
[`est_seroincidence_by()`](https:/ucd-serg.github.io/serocalculator/preview/pr466/reference/est_seroincidence_by.md).

## Usage

``` r
# S3 method for class 'seroincidence.by'
summary(
  object,
  confidence_level = 0.95,
  show_deviance = TRUE,
  show_convergence = TRUE,
  verbose = FALSE,
  ...
)
```

## Arguments

- object:

  A dataframe containing output of
  [`est_seroincidence_by()`](https:/ucd-serg.github.io/serocalculator/preview/pr466/reference/est_seroincidence_by.md).

- confidence_level:

  desired confidence interval coverage probability

- show_deviance:

  Logical flag (`FALSE`/`TRUE`) for reporting deviance
  (-2\*log(likelihood) at estimated seroincidence. Default = `TRUE`.

- show_convergence:

  Logical flag (`FALSE`/`TRUE`) for reporting convergence (see help for
  [`optim()`](https://rdrr.io/r/stats/optim.html) for details). Default
  = `FALSE`.

- verbose:

  a [logical](https://rdrr.io/r/base/logical.html) scalar indicating
  whether to print verbose messages to the console

- ...:

  Additional arguments affecting the summary produced.

## Value

A `summary.seroincidence.by` object, which is a
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html),
with the following columns:

- `incidence.rate` maximum likelihood estimate of `lambda`
  (seroincidence)

- `CI.lwr` lower confidence bound for lambda

- `CI.upr` upper confidence bound for lambda

- `Deviance` (included if `show_deviance = TRUE`) Negative log
  likelihood (NLL) at estimated (maximum likelihood) `lambda`)

- `nlm.convergence.code` (included if `show_convergence = TRUE`)
  Convergence information returned by
  [`stats::nlm()`](https://rdrr.io/r/stats/nlm.html)

The object also has the following metadata (accessible through
[`base::attr()`](https://rdrr.io/r/base/attr.html)):

- `antigen_isos` Character vector with names of input antigen isotypes
  used in
  [`est_seroincidence_by()`](https:/ucd-serg.github.io/serocalculator/preview/pr466/reference/est_seroincidence_by.md)

- `Strata` Character with names of strata used in
  [`est_seroincidence_by()`](https:/ucd-serg.github.io/serocalculator/preview/pr466/reference/est_seroincidence_by.md)

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
summary(est2)
#> Seroincidence estimated given the following setup:
#> a) Antigen isotypes   : HlyE_IgG, HlyE_IgA 
#> b) Strata       : catchment 
#> 
#>  Seroincidence estimates:
#> # A tibble: 2 × 13
#>   Stratum catchment     n est.start incidence.rate     SE CI.lwr CI.upr coverage
#>   <chr>   <chr>     <int>     <dbl>          <dbl>  <dbl>  <dbl>  <dbl>    <dbl>
#> 1 Stratu… aku          53       0.1          0.140 0.0216  0.104  0.189     0.95
#> 2 Stratu… kgh          47       0.1          0.200 0.0301  0.149  0.268     0.95
#> # ℹ 4 more variables: log.lik <dbl>, iterations <int>, antigen.isos <chr>,
#> #   nlm.convergence.code <ord>
```
