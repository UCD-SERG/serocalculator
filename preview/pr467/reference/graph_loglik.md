# Graph log-likelihood of data

Graph log-likelihood of data

## Usage

``` r
graph_loglik(
  pop_data,
  curve_params,
  noise_params,
  antigen_isos = pop_data %>% get_biomarker_levels(),
  x = 10^seq(-3, 0, by = 0.1),
  highlight_points = NULL,
  highlight_point_names = "highlight_points",
  log_x = FALSE,
  previous_plot = NULL,
  curve_label = paste(antigen_isos, collapse = " + "),
  ...
)
```

## Arguments

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

- x:

  sequence of lambda values to graph

- highlight_points:

  a possible highlighted value

- highlight_point_names:

  labels for highlighted points

- log_x:

  should the x-axis be on a logarithmic scale (`TRUE`) or linear scale
  (`FALSE`, default)?

- previous_plot:

  if not NULL, the current data is added to the existing graph

- curve_label:

  if not NULL, add a label for the curve

- ...:

  Arguments passed on to
  [`log_likelihood`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/log_likelihood.md)

  `verbose`

  :   logical: if TRUE, print verbose log information to console

## Value

a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)

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

# Load noise parameters
cond <- tibble(
  antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
  nu = c(0.5, 0.5),                          # Biologic noise (nu)
  eps = c(0, 0),                             # M noise (eps)
  y.low = c(1, 1),                           # Low cutoff (llod)
  y.high = c(5e6, 5e6))                      # High cutoff (y.high)

# Graph the log likelihood
lik_HlyE_IgA <- # nolint: object_name_linter
  graph_loglik(
    pop_data = xs_data,
    curve_params = curve,
    noise_params = cond,
    antigen_isos = "HlyE_IgA",
    log_x = TRUE
)

lik_HlyE_IgA # nolint: object_name_linter

```
