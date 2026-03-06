# Load longitudinal seroresponse parameters

Load longitudinal seroresponse parameters

## Usage

``` r
as_sr_params(data, antigen_isos = NULL)
```

## Arguments

- data:

  a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) or
  [tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)

- antigen_isos:

  a [`character()`](https://rdrr.io/r/base/character.html) vector of
  antigen isotypes to be used in analyses

## Value

a `curve_data` object (a
[tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
with extra attribute `antigen_isos`)

## Examples

``` r
library(magrittr)
curve_data <-
  serocalculator_example("example_curve_params.csv") %>%
  read.csv() %>%
  as_curve_params()
#> Warning: `as_curve_params()` was deprecated in serocalculator 1.3.1.
#> ℹ Please use `as_sr_params()` instead.

print(curve_data)
#> # A tibble: 500 × 8
#>        X antigen_iso  iter     y0     y1    t1     alpha     r
#>    <int> <chr>       <int>  <dbl>  <dbl> <dbl>     <dbl> <dbl>
#>  1     1 HlyE_IgA        1  2.48    63.5  9.52 0.000581   1.75
#>  2     2 HlyE_IgG        1  3.04   164.   6.55 0.00457    1.17
#>  3     3 LPS_IgA         1  0.748  103.   4.98 0.00308    1.58
#>  4     4 LPS_IgG         1  0.941  320.   6.14 0.00166    1.41
#>  5     5 Vi_IgG          1  8.46  4348.   3.07 0.0000340  1.06
#>  6     6 HlyE_IgA        2  3.86   288.   1.27 0.000459   2.66
#>  7     7 HlyE_IgG        2  1.82   154.  10.8  0.000921   1.30
#>  8     8 LPS_IgA         2  1.76   852.   2.49 0.000126   2.91
#>  9     9 LPS_IgG         2  0.434   20.6  4.29 0.00122    1.37
#> 10    10 Vi_IgG          2 18.8    345.   3.48 0.000142   1.02
#> # ℹ 490 more rows
```
