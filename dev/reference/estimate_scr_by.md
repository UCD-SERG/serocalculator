# Estimate Seroincidence

Function to estimate seroincidences based on cross-sectional serology
data and longitudinal response model.

## Usage

``` r
estimate_scr_by(
  pop_data,
  curve_params,
  noise_params,
  strata,
  curve_strata_varnames = strata,
  noise_strata_varnames = strata,
  antigen_isos = unique(pull(pop_data, "antigen_iso")),
  lambda_start = 0.1,
  build_graph = FALSE,
  num_cores = 1L,
  verbose = FALSE,
  print_graph = FALSE,
  ...
)
```

## Arguments

- pop_data:

  a [data.frame](https://rdrr.io/r/base/data.frame.html) with
  cross-sectional serology data per antibody and age, and additional
  columns corresponding to each element of the `strata` input

- curve_params:

  a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) containing
  MCMC samples of parameters from the Bayesian posterior distribution of
  a longitudinal decay curve model. The parameter columns must be named:

  - `antigen_iso`: a
    [`character()`](https://rdrr.io/r/base/character.html) vector
    indicating antigen-isotype combinations

  - `iter`: an [`integer()`](https://rdrr.io/r/base/integer.html) vector
    indicating MCMC sampling iterations

  - `y0`: baseline antibody level at \$t=0\$ (\$y(t=0)\$)

  - `y1`: antibody peak level (ELISA units)

  - `t1`: duration of infection

  - `alpha`: antibody decay rate (1/days for the current longitudinal
    parameter sets)

  - `r`: shape factor of antibody decay

- noise_params:

  a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) (or
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html))
  containing the following variables, specifying noise parameters for
  each antigen isotype:

  - `antigen_iso`: antigen isotype whose noise parameters are being
    specified on each row

  - `nu`: biological noise

  - `eps`: measurement noise

  - `y.low`: lower limit of detection for the current antigen isotype

  - `y.high`: upper limit of detection for the current antigen isotype

- strata:

  a [character](https://rdrr.io/r/base/character.html) vector of
  stratum-defining variables. Values must be variable names in
  `pop_data`.

- curve_strata_varnames:

  A subset of `strata`. Values must be variable names in `curve_params`.
  Default = "".

- noise_strata_varnames:

  A subset of `strata`. Values must be variable names in `noise_params`.
  Default = "".

- antigen_isos:

  Character vector with one or more antibody names. Must match
  `pop_data`

- lambda_start:

  starting guess for incidence rate, in years/event.

- build_graph:

  whether to graph the log-likelihood function across a range of
  incidence rates (lambda values)

- num_cores:

  Number of processor cores to use for calculations when computing by
  strata. If set to more than 1 and package parallel is available, then
  the computations are executed in parallel. Default = 1L.

- verbose:

  logical: if TRUE, print verbose log information to console

- print_graph:

  whether to display the log-likelihood curve graph in the course of
  running
  [`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/dev/reference/est_seroincidence.md)

- ...:

  Arguments passed on to
  [`est_seroincidence`](https://ucd-serg.github.io/serocalculator/dev/reference/est_seroincidence.md),
  [`stats::nlm`](https://rdrr.io/r/stats/nlm.html)

  `stepmin`

  :   A positive scalar providing the minimum allowable relative step
      length.

  `stepmax`

  :   a positive scalar which gives the maximum allowable scaled step
      length. `stepmax` is used to prevent steps which would cause the
      optimization function to overflow, to prevent the algorithm from
      leaving the area of interest in parameter space, or to detect
      divergence in the algorithm. `stepmax` would be chosen small
      enough to prevent the first two of these occurrences, but should
      be larger than any anticipated reasonable step.

  `typsize`

  :   an estimate of the size of each parameter at the minimum.

  `fscale`

  :   an estimate of the size of `f` at the minimum.

  `ndigit`

  :   the number of significant digits in the function `f`.

  `gradtol`

  :   a positive scalar giving the tolerance at which the scaled
      gradient is considered close enough to zero to terminate the
      algorithm. The scaled gradient is a measure of the relative change
      in `f` in each direction `p[i]` divided by the relative change in
      `p[i]`.

  `iterlim`

  :   a positive integer specifying the maximum number of iterations to
      be performed before the program is terminated.

  `check.analyticals`

  :   a logical scalar specifying whether the analytic gradients and
      Hessians, if they are supplied, should be checked against
      numerical derivatives at the initial parameter values. This can
      help detect incorrectly formulated gradients or Hessians.

## Value

- if `strata` has meaningful inputs: An object of class
  `"seroincidence.by"`; i.e., a list of `"seroincidence"` objects from
  [`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/dev/reference/est_seroincidence.md),
  one for each stratum, with some meta-data attributes.

- if `strata` is missing, `NULL`, `NA`, or `""`: An object of class
  `"seroincidence"`.

## Details

If `strata` is left empty, a warning will be produced, recommending that
you use
[`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/dev/reference/est_seroincidence.md)
for unstratified analyses, and then the data will be passed to
[`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/dev/reference/est_seroincidence.md).
If for some reason you want to use `estimate_scr_by()` with no strata
instead of calling
[`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/dev/reference/est_seroincidence.md),
you may use `NA`, `NULL`, or `""` as the `strata` argument to avoid that
warning.

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

est2 <- estimate_scr_by(
  strata = "catchment",
  pop_data = xs_data,
  curve_params = curve,
  noise_params = noise,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
  # num_cores = 8 # Allow for parallel processing to decrease run time
  iterlim = 5 # limit iterations for the purpose of this example
)
#> Warning: `curve_params` is missing all strata variables and will be used unstratified.
#> ℹ To avoid this warning, specify the desired set of stratifying variables in
#>   the `curve_strata_varnames` and `noise_strata_varnames` arguments to
#>   `estimate_scr_by()`.
#> Warning: `noise_params` is missing all strata variables and will be used unstratified.
#> ℹ To avoid this warning, specify the desired set of stratifying variables in
#>   the `curve_strata_varnames` and `noise_strata_varnames` arguments to
#>   `estimate_scr_by()`.
print(est2)
#> `seroincidence.by` object estimated given the following setup:
#> a) Antigen isotypes   : HlyE_IgG, HlyE_IgA 
#> b) Strata       : catchment 
#> 
#> This object is a list of `seroincidence` objects, with added meta-data attributes:
#> `antigen_isos` - Character vector of antigen isotypes used in analysis.
#> `Strata`       - Input parameter strata of function `estimate_scr_by()`
#> 
#> Call the `summary()` function to obtain output results.
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
