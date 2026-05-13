# Split data by stratum

Split biomarker data, decay curve parameters, and noise parameters to
prepare for stratified incidence estimation.

## Usage

``` r
stratify_data(
  data,
  curve_params,
  noise_params,
  strata_varnames = "",
  curve_strata_varnames = NULL,
  noise_strata_varnames = NULL,
  antigen_isos = get_biomarker_levels(data),
  cluster_var = NULL,
  stratum_var = NULL
)
```

## Arguments

- strata_varnames:

  [`character()`](https://rdrr.io/r/base/character.html) vector of names
  of variables in `data` to stratify by

## Value

a `"biomarker_data_and_params.list"` object (a
[list](https://rdrr.io/r/base/list.html) with extra attributes
`"strata"`, `"antigen_isos"`, etc)

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

xs_data <-
  sees_pop_data_pk_100

curve <-
  typhoid_curves_nostrat_100 |>
  filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

noise <-
  example_noise_params_pk

stratified_data =
  stratify_data(
   data = xs_data,
   curve_params = curve,
   noise_params = noise,
   strata_varnames = "catchment",
   curve_strata_varnames = NULL,
   noise_strata_varnames = NULL
   )
} # }
```
