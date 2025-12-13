# Load a cross-sectional antibody survey data set

Load a cross-sectional antibody survey data set

## Usage

``` r
load_pop_data(file_path, ...)
```

## Arguments

- file_path:

  path to an RDS file containing a cross-sectional antibody survey data
  set, stored as a
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) or
  [tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)

- ...:

  Arguments passed on to
  [`as_pop_data`](https:/ucd-serg.github.io/serocalculator/preview/pr466/reference/as_pop_data.md)

  `data`

  :   a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) or
      [tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)

  `antigen_isos`

  :   [`character()`](https://rdrr.io/r/base/character.html) vector of
      antigen isotypes to be used in analyses

  `age`

  :   a [`character()`](https://rdrr.io/r/base/character.html)
      identifying the age column

  `id`

  :   a [`character()`](https://rdrr.io/r/base/character.html)
      identifying the id column

  `value`

  :   a [`character()`](https://rdrr.io/r/base/character.html)
      identifying the value column

  `standardize`

  :   a [`logical()`](https://rdrr.io/r/base/logical.html) to determine
      standardization of columns

## Value

a `pop_data` object (a
[tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
with extra attributes)

## Examples

``` r
xs_data <- load_pop_data(serocalculator_example("example_pop_data.rds"))

print(xs_data)
#> # A tibble: 200 × 8
#>    id    Country  cluster               catchment   age ageCat antigen_iso value
#>    <chr> <chr>    <chr>                 <chr>     <dbl> <chr>  <chr>       <dbl>
#>  1 P1    Pakistan Lyari Other           kgh        13.2 5-15   HlyE_IgA    0.568
#>  2 P3    Pakistan Gillani Railway Stat… aku        18   16+    HlyE_IgA    5.69 
#>  3 P5    Pakistan Gillani Railway Stat… aku         7.3 5-15   HlyE_IgA    1.23 
#>  4 P7    Pakistan Civic Centre          aku         2.6 <5     HlyE_IgA    1.08 
#>  5 P9    Pakistan Gillani Railway Stat… aku         3.9 <5     HlyE_IgA    1.43 
#>  6 P11   Pakistan Civic Centre          aku        13   5-15   HlyE_IgA    3.06 
#>  7 P13   Pakistan Machar Colony         kgh        11   5-15   HlyE_IgA    0.779
#>  8 P15   Pakistan Machar Colony         kgh        12   5-15   HlyE_IgA    1.90 
#>  9 P17   Pakistan Machar Colony         kgh        16   16+    HlyE_IgA    1.41 
#> 10 P19   Pakistan Gillani Railway Stat… aku        14.4 5-15   HlyE_IgA    3.10 
#> # ℹ 190 more rows
```
