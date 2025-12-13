# Summarize cross-sectional antibody survey data

[`summary()`](https://rdrr.io/r/base/summary.html) method for `pop_data`
objects

## Usage

``` r
# S3 method for class 'pop_data'
summary(object, strata = NULL, ...)

# S3 method for class 'summary.pop_data'
print(x, ...)
```

## Arguments

- object:

  a `pop_data` object (from
  [`as_pop_data()`](https:/ucd-serg.github.io/serocalculator/preview/pr466/reference/as_pop_data.md))

- strata:

  a [`character()`](https://rdrr.io/r/base/character.html) specifying
  grouping column(s)

- ...:

  unused

- x:

  an object of class `"summary.pop_data"`; usually, the result of a call
  to `summary.pop_data()`

## Value

a `summary.pop_data` object, which is a list containing two summary
tables:

- `age_summary` summarizing `age`

- `ab_summary` summarizing `value`, stratified by `antigen_iso`

## Examples

``` r
library(dplyr)

xs_data <-
  sees_pop_data_pk_100
summary(xs_data, strata = "catchment")
#> 
#> n = 200 
#> 
#> Distribution of age: 
#> 
#> # A tibble: 2 × 8
#>   catchment     n   min first_quartile median  mean third_quartile   max
#>   <chr>     <int> <dbl>          <dbl>  <dbl> <dbl>          <dbl> <dbl>
#> 1 kgh          94   2.3           6.85     11  11.6           15.9    24
#> 2 aku         106   2.3           6.6      12  11.8           16      23
#> 
#> Distributions of antigen-isotype measurements:
#> 
#> # A tibble: 4 × 8
#>   antigen_iso catchment    Min `1st Qu.` Median `3rd Qu.`   Max `# NAs`
#>   <fct>       <chr>      <dbl>     <dbl>  <dbl>     <dbl> <dbl>   <int>
#> 1 HlyE_IgA    kgh       0          1.55    2.90      3.99  45.6       0
#> 2 HlyE_IgA    aku       0.0308     1.20    2.00      5.74  69.9       0
#> 3 HlyE_IgG    kgh       0.362      1.99    2.79      8.06  64.5       0
#> 4 HlyE_IgG    aku       0.217      0.983   2.12      5.78  33.6       0
#> 
```
