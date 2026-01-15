# Print Method for `seroincidence.by` Object

Custom [`print()`](https://rdrr.io/r/base/print.html) function for
`seroincidence.by` objects (from
[`est_seroincidence_by()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence_by.md))

## Usage

``` r
# S3 method for class 'seroincidence.by'
print(x, ...)
```

## Arguments

- x:

  A list containing output of function
  [`est_seroincidence_by()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence_by.md).

- ...:

  Additional arguments affecting the summary produced.

## Value

an [invisible](https://rdrr.io/r/base/invisible.html) copy of input
parameter `x`

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

# estimate seroincidence
est2 <- est_seroincidence_by(
  strata = c("catchment"),
  pop_data = xs_data,
  sr_params = curve,
  noise_params = noise,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
  # num_cores = 8 # Allow for parallel processing to decrease run time
)
#> Warning: `curve_params` is missing all strata variables and will be used unstratified.
#> ℹ To avoid this warning, specify the desired set of stratifying variables in
#>   the `curve_strata_varnames` and `noise_strata_varnames` arguments to
#>   `est_seroincidence_by()`.
#> Warning: `noise_params` is missing all strata variables and will be used unstratified.
#> ℹ To avoid this warning, specify the desired set of stratifying variables in
#>   the `curve_strata_varnames` and `noise_strata_varnames` arguments to
#>   `est_seroincidence_by()`.

# calculate summary statistics for the seroincidence object
print(est2)
#> `seroincidence.by` object estimated given the following setup:
#> a) Antigen isotypes   : HlyE_IgG, HlyE_IgA 
#> b) Strata       : catchment 
#> 
#> This object is a list of `seroincidence` objects, with added meta-data attributes:
#> `antigen_isos` - Character vector of antigen isotypes used in analysis.
#> `Strata`       - Input parameter strata of function `est_seroincidence_by()`
#> 
#> Call the `summary()` function to obtain output results.
```
