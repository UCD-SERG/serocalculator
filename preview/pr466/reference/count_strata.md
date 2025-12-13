# Count observations by stratum

Count observations by stratum

## Usage

``` r
count_strata(
  data,
  strata_varnames,
  biomarker_names_var = get_biomarker_names_var(data)
)
```

## Arguments

- data:

  a `"pop_data"` object (e.g., from
  [`as_pop_data()`](https:/ucd-serg.github.io/serocalculator/preview/pr466/reference/as_pop_data.md))

- strata_varnames:

  a [vector](https://rdrr.io/r/base/vector.html) of
  [character](https://rdrr.io/r/base/character.html) strings matching
  colnames to stratify on from `data`

- biomarker_names_var:

  a [character](https://rdrr.io/r/base/character.html) string indicating
  the column of `data` indicating which biomarker is being measured

## Value

a
[tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
counting observations by stratum

## Examples

``` r
sees_pop_data_pk_100 |> count_strata(strata_varnames = "catchment")
#> # A tibble: 2 × 3
#>   Stratum   catchment     n
#>   <chr>     <chr>     <int>
#> 1 Stratum 1 aku          53
#> 2 Stratum 2 kgh          47
```
