# simulate antibody kinetics of y over a time interval

simulate antibody kinetics of y over a time interval

## Usage

``` r
simresp.tinf(
  lambda,
  t_end,
  age_fixed,
  antigen_isos,
  n_mcmc_samples = 0,
  renew_params,
  predpar,
  ...
)
```

## Arguments

- lambda:

  seroconversion rate (1/days),

- t_end:

  end of time interval (beginning is time 0) in days(?)

- age_fixed:

  parameter estimates for fixed age (age_fixed in years) or not. when
  age_fixed = NA then age at infection is used.

- antigen_isos:

  antigen isotypes

- n_mcmc_samples:

  a posterior sample may be selected (1:4000), or not when
  n_mcmc_samples = 0 a posterior sample is chosen at random.

- renew_params:

  At infection, a new parameter sample may be generated (when
  `renew_params = TRUE`). Otherwise (when `renew_params = FALSE`), a
  sample is generated at birth and kept, but baseline y0 are carried
  over from prior infections.

- predpar:

  an [`array()`](https://rdrr.io/r/base/array.html) with dimensions
  named:

  - `antigen_iso`

  - `parameter`

  - `obs`

- ...:

  Arguments passed on to
  [`ldpar`](https:/ucd-serg.github.io/serocalculator/preview/pr470/reference/ldpar.md),
  [`ab`](https:/ucd-serg.github.io/serocalculator/preview/pr470/reference/ab.md),
  [`mk_baseline`](https:/ucd-serg.github.io/serocalculator/preview/pr470/reference/mk_baseline.md)

  `age`

  :   age at infection

  `nmc`

  :   mcmc sample to use

  `npar`

  :   number of parameters

  `t`

  :   [numeric](https://rdrr.io/r/base/numeric.html)
      [vector](https://rdrr.io/r/base/vector.html) of elapsed times
      since start of infection

  `par`

  :   [numeric](https://rdrr.io/r/base/numeric.html)
      [matrix](https://rdrr.io/r/base/matrix.html) of model parameters:

      - rows are parameters

      - columns are biomarkers

  `kab`

  :   [integer](https://rdrr.io/r/base/integer.html) indicating which
      row to read from `blims`

  `n`

  :   number of observations

  `blims`

  :   range of possible baseline antibody levels

## Value

a [list](https://rdrr.io/r/base/list.html) with:

- t = times (in days, birth at day 0),

- b = bacteria level, for each antibody signal (not used; probably
  meaningless),

- y = antibody level, for each antibody signal

- smp = whether an infection involves a big jump or a small jump

- t.inf = times when infections have occurred.
