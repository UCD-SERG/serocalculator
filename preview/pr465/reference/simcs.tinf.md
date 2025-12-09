# collect cross-sectional data

collect cross-sectional data

## Usage

``` r
simcs.tinf(
  lambda,
  n_samples,
  age_range,
  age_fixed = NA,
  antigen_isos,
  n_mcmc_samples = 0,
  renew_params = FALSE,
  ...
)
```

## Arguments

- lambda:

  seroconversion rate (in events/person-day)

- n_samples:

  number of samples n_samples (= nr of simulated records)

- age_range:

  age range to use for simulating data, in days

- age_fixed:

  age_fixed for parameter sample (age_fixed = NA for age at infection)

- antigen_isos:

  [character](https://rdrr.io/r/base/character.html)
  [vector](https://rdrr.io/r/base/vector.html) with one or more antibody
  names. Values must match `curve_params`.

- n_mcmc_samples:

  - when `n_mcmc_samples` is in 1:4000, a fixed posterior sample is used

  - when n_mcmc_samples = 0 a random sample is chosen

- renew_params:

  - `renew_params = TRUE` generates a new parameter set for each
    infection

  - `renew_params = FALSE` keeps the one selected at birth, but updates
    baseline y0

- ...:

  Arguments passed on to
  [`simresp.tinf`](https:/ucd-serg.github.io/serocalculator/preview/pr465/reference/simresp.tinf.md)

  `predpar`

  :   an [`array()`](https://rdrr.io/r/base/array.html) with dimensions
      named:

      - `antigen_iso`

      - `parameter`

      - `obs`

## Value

an [`array()`](https://rdrr.io/r/base/array.html) with dimensions
`n_samples`, `length(antigen_isos) + 1`, where rows are observations and
columns are age and biomarkers y(t)
