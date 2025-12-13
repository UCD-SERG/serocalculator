# Plot `seroincidence.by` log-likelihoods

Plots log-likelihood curves by stratum, for `seroincidence.by` objects

## Usage

``` r
# S3 method for class 'seroincidence.by'
autoplot(object, ncol = min(3, length(object)), ...)
```

## Arguments

- object:

  a '"seroincidence.by"' object (from
  [`est_seroincidence_by()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/est_seroincidence_by.md))

- ncol:

  number of columns to use for panel of plots

- ...:

  Arguments passed on to
  [`autoplot.seroincidence`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/autoplot.seroincidence.md)

  `log_x`

  :   should the x-axis be on a logarithmic scale (`TRUE`) or linear
      scale (`FALSE`, default)?

## Value

a `"ggarrange"` object: a single or
[`list()`](https://rdrr.io/r/base/list.html) of
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)s

## Examples

``` r
# \donttest{
library(dplyr)
library(ggplot2)

xs_data <-
  sees_pop_data_pk_100

curve <-
  typhoid_curves_nostrat_100 %>%
  filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

noise <-
  example_noise_params_pk

est2 <- est_seroincidence_by(
  strata = c("catchment"),
  pop_data = xs_data,
  sr_params = curve,
  curve_strata_varnames= NULL,
  noise_strata_varnames = NULL,
  noise_params = noise,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
  #num_cores = 8, #Allow for parallel processing to decrease run time
  build_graph = TRUE
)

# Plot the log-likelihood curve
autoplot(est2)

# }
```
