# Calculate negative log-likelihood

Same as
[`log_likelihood()`](https:/ucd-serg.github.io/serocalculator/preview/pr466/reference/log_likelihood.md),
except negated and requiring lambda on log scale (used in combination
with [`nlm()`](https://rdrr.io/r/stats/nlm.html), to ensure that the
optimization search doesn't stray into negative values of `lambda`).

## Usage

``` r
.nll(log.lambda, ...)
```

## Arguments

- log.lambda:

  natural logarithm of incidence rate

- ...:

  Arguments passed on to
  [`log_likelihood`](https:/ucd-serg.github.io/serocalculator/preview/pr466/reference/log_likelihood.md)

  `pop_data`

  :   a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) with
      cross-sectional serology data by antibody and age, and additional
      columns

  `antigen_isos`

  :   Character vector listing one or more antigen isotypes. Values must
      match `pop_data`.

  `curve_params`

  :   a [`data.frame()`](https://rdrr.io/r/base/data.frame.html)
      containing MCMC samples of parameters from the Bayesian posterior
      distribution of a longitudinal decay curve model. The parameter
      columns must be named:

      - `antigen_iso`: a
        [`character()`](https://rdrr.io/r/base/character.html) vector
        indicating antigen-isotype combinations

      - `iter`: an [`integer()`](https://rdrr.io/r/base/integer.html)
        vector indicating MCMC sampling iterations

      - `y0`: baseline antibody level at \$t=0\$ (\$y(t=0)\$)

      - `y1`: antibody peak level (ELISA units)

      - `t1`: duration of infection

      - `alpha`: antibody decay rate (1/days for the current
        longitudinal parameter sets)

      - `r`: shape factor of antibody decay

  `noise_params`

  :   a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) (or
      [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html))
      containing the following variables, specifying noise parameters
      for each antigen isotype:

      - `antigen_iso`: antigen isotype whose noise parameters are being
        specified on each row

      - `nu`: biological noise

      - `eps`: measurement noise

      - `y.low`: lower limit of detection for the current antigen
        isotype

      - `y.high`: upper limit of detection for the current antigen
        isotype

  `verbose`

  :   logical: if TRUE, print verbose log information to console

## Value

the negative log-likelihood of the data with the current parameter
values
