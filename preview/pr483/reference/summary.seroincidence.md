# Summarizing fitted seroincidence models

This function is a [`summary()`](https://rdrr.io/r/base/summary.html)
method for `seroincidence` objects.

## Usage

``` r
# S3 method for class 'seroincidence'
summary(object, coverage = 0.95, verbose = TRUE, ...)
```

## Arguments

- object:

  a [`list()`](https://rdrr.io/r/base/list.html) outputted by
  [`stats::nlm()`](https://rdrr.io/r/stats/nlm.html) or
  [`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence.md)

- coverage:

  desired confidence interval coverage probability

- verbose:

  whether to produce verbose messaging

- ...:

  unused

## Value

a
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
containing the following:

- `est.start`: the starting guess for incidence rate

- `ageCat`: the age category we are analyzing

- `incidence.rate`: the estimated incidence rate, per person year

- `CI.lwr`: lower limit of confidence interval for incidence rate

- `CI.upr`: upper limit of confidence interval for incidence rate

- `coverage`: coverage probability

- `log.lik`: log-likelihood of the data used in the call to
  [`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence.md),
  evaluated at the maximum-likelihood estimate of lambda (i.e., at
  `incidence.rate`)

- `iterations`: the number of iterations used

- `antigen_isos`: a list of antigen isotypes used in the analysis

- `nlm.convergence.code`: information about convergence of the
  likelihood maximization procedure performed by
  [`nlm()`](https://rdrr.io/r/stats/nlm.html) (see "Value" section of
  [`stats::nlm()`](https://rdrr.io/r/stats/nlm.html), component `code`);
  codes 3-5 indicate issues:

  - 1: relative gradient is close to zero, current iterate is probably
    solution.

  - 2: successive iterates within tolerance, current iterate is probably
    solution.

  - 3: Last global step failed to locate a point lower than x. Either x
    is an approximate local minimum of the function, the function is too
    non-linear for this algorithm, or `stepmin` in
    [`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence.md)
    (a.k.a., `steptol` in
    [`stats::nlm()`](https://rdrr.io/r/stats/nlm.html)) is too large.

  - 4: iteration limit exceeded; increase `iterlim`.

  - 5: maximum step size `stepmax` exceeded five consecutive times.
    Either the function is unbounded below, becomes asymptotic to a
    finite value from above in some direction, or `stepmax` is too
    small.

The returned object also has the following attributes:

- `noise_params`: a
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  with exact numeric noise parameters (`antigen_iso`, `eps` (measurement
  noise), `nu` (biological noise))

- `n_sr_params`: number of longitudinal seroresponse parameter
  observations

- `n_pop_data`: number of population data observations

- `sr_params_stratified`: logical indicating whether seroresponse
  parameters were stratified (`FALSE` for unstratified)

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

est1 <- est_seroincidence(
  pop_data = xs_data,
  sr_params = curve,
  noise_params = noise,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA")
)

summary(est1)
#> # A tibble: 1 × 10
#>   est.start incidence.rate     SE CI.lwr CI.upr coverage log.lik iterations
#>       <dbl>          <dbl>  <dbl>  <dbl>  <dbl>    <dbl>   <dbl>      <int>
#> 1       0.1          0.166 0.0178  0.135  0.205     0.95   -524.          5
#> # ℹ 2 more variables: antigen.isos <chr>, nlm.convergence.code <ord>
```
