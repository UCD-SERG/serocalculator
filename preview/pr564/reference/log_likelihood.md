# Calculate log-likelihood

Calculates the log-likelihood of a set of cross-sectional antibody
response data, for a given incidence rate (`lambda`) value.

## Usage

``` r
log_likelihood(
  lambda,
  pop_data,
  curve_params,
  noise_params,
  antigen_isos = get_biomarker_levels(pop_data),
  verbose = FALSE,
  ...
)
```

## Arguments

- lambda:

  a [numeric](https://rdrr.io/r/base/numeric.html) vector of incidence
  parameters, in events per person-year

- pop_data:

  a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) with
  cross-sectional serology data by antibody and age, and additional
  columns

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

- antigen_isos:

  Character vector listing one or more antigen isotypes. Values must
  match `pop_data`.

- verbose:

  logical: if TRUE, print verbose log information to console

- ...:

  additional arguments passed to other functions (not currently used).

## Value

the log-likelihood of the data with the current parameter values

## Examples

``` r
library(dplyr)
library(tibble)

# Load cross-sectional data
xs_data <-
  sees_pop_data_pk_100

# Load curve parameters and subset for the purposes of this example
curve <-
  typhoid_curves_nostrat_100 %>%
  filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

# Load noise params
cond <- tibble(
  antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
  nu = c(0.5, 0.5), # Biologic noise (nu)
  eps = c(0, 0), # M noise (eps)
  y.low = c(1, 1), # low cutoff (llod)
  y.high = c(5e6, 5e6)
) # high cutoff (y.high)

# Calculate log-likelihood
ll_AG <- log_likelihood(
  pop_data = xs_data,
  curve_params = curve,
  noise_params = cond,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
  lambda = 0.1
) %>% print()
#> [1] -610.1194
```
