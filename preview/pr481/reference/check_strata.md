# Check a `pop_data` object for requested strata variables

Check a `pop_data` object for requested strata variables

## Usage

``` r
check_strata(
  pop_data,
  strata,
  biomarker_names_var = get_biomarker_names_var(pop_data)
)
```

## Arguments

- pop_data:

  a `pop_data` object

- strata:

  a [character](https://rdrr.io/r/base/character.html) vector

- biomarker_names_var:

  name of column in `pop_data` indicating biomarker type

## Value

[NULL](https://rdrr.io/r/base/NULL.html), invisibly

## Examples

``` r
sees_pop_data_pk_100 |>
  check_strata(strata = c("ag", "catch", "Count")) |>
  try()
#> Error in check_strata(sees_pop_data_pk_100, strata = c("ag", "catch",  : 
#>   could not find function "check_strata"
```
