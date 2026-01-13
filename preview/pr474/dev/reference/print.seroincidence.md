# Print Method for `seroincidence` Class

[`print()`](https://rdrr.io/r/base/print.html) function for
`seroincidence` objects from
[`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/dev/reference/est_seroincidence.md)

## Usage

``` r
# S3 method for class 'seroincidence'
print(x, ...)
```

## Arguments

- x:

  A list containing output of function
  [`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/dev/reference/est_seroincidence.md).

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
  typhoid_curves_nostrat_100 %>%
  filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

noise <-
  example_noise_params_pk

est1 <- est_seroincidence(
  pop_data = xs_data,
  sr_params = curve,
  noise_params = noise,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
)
print(est1)
#> `seroincidence` object estimated given the following setup:
#> a) `antigen_isos`:  HlyE_IgG, HlyE_IgA 
#> b) `lambda_start`:  0.1 
#> Call the `summary()` function to obtain output results.
#> Call the `autoplot()` function to graph the log-likelihood curve.
```
