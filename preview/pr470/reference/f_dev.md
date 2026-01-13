# Calculate negative log-likelihood (deviance) for one antigen:isotype pair and several values of incidence

Calculates negative log-likelihood (deviance) for one antigen:isotype
pair and several values of incidence (`lambda`).

## Usage

``` r
f_dev(lambda, csdata, lnpars, cond)
```

## Arguments

- lambda:

  a [numeric](https://rdrr.io/r/base/numeric.html) vector of incidence
  parameters, in events per person-year

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of negative
log-likelihoods, corresponding to the elements of input `lambda`

## Details

Vectorized version of
[`f_dev0()`](https:/ucd-serg.github.io/serocalculator/preview/pr470/reference/f_dev0.md);
interface with C lib `serocalc.so`

## Examples

``` r
# \donttest{
library(dplyr)
library(tibble)

# load in longitudinal parameters
curve_params <-
  typhoid_curves_nostrat_100 %>%
  filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

# load in pop data
xs_data <-
  sees_pop_data_pk_100

#Load noise params
noise_params <- tibble(
  antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
  nu = c(0.5, 0.5),                          # Biologic noise (nu)
  eps = c(0, 0),                             # M noise (eps)
  y.low = c(1, 1),                           # low cutoff (llod)
  y.high = c(5e6, 5e6))                      # high cutoff (y.high)

cur_antibody = "HlyE_IgA"

cur_data =
  xs_data %>%
  dplyr::filter(
   .data$catchment == "aku",
   .data$antigen_iso == cur_antibody) %>%
  dplyr::slice_head(n = 100)

cur_curve_params =
  curve_params %>%
  dplyr::filter(.data$antigen_iso == cur_antibody) %>%
  dplyr::slice_head(n = 100)

cur_noise_params =
  noise_params %>%
  dplyr::filter(.data$antigen_iso == cur_antibody)

if(!is.element('d', names(cur_curve_params)))
{
  cur_curve_params =
    cur_curve_params %>%
    dplyr::mutate(
      alpha = .data$alpha * 365.25,
      d = .data$r - 1)
}

lambdas = seq(.1, .2, by = .01)
f_dev(
    lambda = lambdas,
    csdata = cur_data,
    lnpars = cur_curve_params,
    cond = cur_noise_params
  )
#>  [1] 155.7506 153.2650 151.1030 149.2133 147.5558 146.0978 144.8130 143.6793
#>  [9] 142.6785 141.7951 141.0158
# }
```
