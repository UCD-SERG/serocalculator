# Find the maximum likelihood estimate of the incidence rate parameter

This function models seroincidence using maximum likelihood estimation;
that is, it finds the value of the seroincidence parameter which
maximizes the likelihood (i.e., joint probability) of the data.

## Usage

``` r
estimate_scr(
  pop_data,
  sr_params,
  noise_params,
  antigen_isos = get_biomarker_names(pop_data),
  lambda_start = 0.1,
  stepmin = 1e-08,
  stepmax = 3,
  verbose = FALSE,
  build_graph = FALSE,
  print_graph = build_graph & verbose,
  ...
)
```

## Arguments

- pop_data:

  a [data.frame](https://rdrr.io/r/base/data.frame.html) with
  cross-sectional serology data per antibody and age, and additional
  columns

- sr_params:

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

- antigen_isos:

  Character vector with one or more antibody names. Must match
  `pop_data`

- lambda_start:

  starting guess for incidence rate, in years/event.

- stepmin:

  A positive scalar providing the minimum allowable relative step
  length.

- stepmax:

  a positive scalar which gives the maximum allowable scaled step
  length. `stepmax` is used to prevent steps which would cause the
  optimization function to overflow, to prevent the algorithm from
  leaving the area of interest in parameter space, or to detect
  divergence in the algorithm. `stepmax` would be chosen small enough to
  prevent the first two of these occurrences, but should be larger than
  any anticipated reasonable step.

- verbose:

  logical: if TRUE, print verbose log information to console

- build_graph:

  whether to graph the log-likelihood function across a range of
  incidence rates (lambda values)

- print_graph:

  whether to display the log-likelihood curve graph in the course of
  running `estimate_scr()`

- ...:

  Arguments passed on to
  [`stats::nlm`](https://rdrr.io/r/stats/nlm.html)

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

a `"seroincidence"` object, which is a
[`stats::nlm()`](https://rdrr.io/r/stats/nlm.html) fit object with extra
metadata attributes `lambda_start`, `antigen_isos`, and `ll_graph`

## Examples

``` r
library(dplyr)

xs_data <-
  sees_pop_data_pk_100

sr_curve <-
  typhoid_curves_nostrat_100 |>
  filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

noise <-
  example_noise_params_pk

est1 <- estimate_scr(
  pop_data = xs_data,
  sr_params = sr_curve,
  noise_params = noise,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
)

summary(est1)
#> # A tibble: 1 × 10
#>   est.start incidence.rate     SE CI.lwr CI.upr coverage log.lik iterations
#>       <dbl>          <dbl>  <dbl>  <dbl>  <dbl>    <dbl>   <dbl>      <int>
#> 1       0.1          0.166 0.0178  0.135  0.205     0.95   -524.          5
#> # ℹ 2 more variables: antigen.isos <chr>, nlm.convergence.code <ord>
```
