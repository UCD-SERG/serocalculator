# Warn about missing stratifying variables in a dataset

Warn about missing stratifying variables in a dataset

## Usage

``` r
warn.missing.strata(data, strata, dataname)
```

## Arguments

- data:

  the dataset that should contain the strata

- strata:

  a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) showing the
  strata levels that are expected to be in the dataset

- dataname:

  the name of the dataset, for use in warning messages if some strata
  are missing.

## Value

a [`character()`](https://rdrr.io/r/base/character.html) vector of the
subset of stratifying variables that are present in `pop_data`

## Examples

``` r
if (FALSE) { # \dontrun{
expected_strata <- data.frame(Species = "banana", type = "orchid")

warn.missing.strata(iris, expected_strata, dataname = "iris")
} # }
```
