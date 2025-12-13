# Load noise parameters

Load noise parameters

## Usage

``` r
as_noise_params(data, antigen_isos = NULL)
```

## Arguments

- data:

  a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) or
  [tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)

- antigen_isos:

  [`character()`](https://rdrr.io/r/base/character.html) vector of
  antigen isotypes to be used in analyses

## Value

a `noise_params` object (a
[tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
with extra attribute `antigen_isos`)

## Examples

``` r
library(magrittr)
noise_data <-
  serocalculator_example("example_noise_params.csv") %>%
  read.csv() %>%
  as_noise_params()

print(noise_data)
#> # A tibble: 4 × 8
#>       X antigen_iso Country  y.low   eps    nu  y.high Lab  
#>   <int> <chr>       <chr>    <dbl> <dbl> <dbl>   <dbl> <chr>
#> 1     1 HlyE_IgA    Pakistan 0.508 0.279  2.60 5000000 AKU  
#> 2     2 HlyE_IgG    Pakistan 1.59  0.146  2.36 5000000 AKU  
#> 3     3 LPS_IgA     Pakistan 5.13  0.246  2.14 5000000 AKU  
#> 4     4 LPS_IgG     Pakistan 4.84  0.273  3.24 5000000 AKU  
```
