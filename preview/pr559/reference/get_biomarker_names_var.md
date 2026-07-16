# Get biomarker variable name

Get biomarker variable name

## Usage

``` r
get_biomarker_names_var(object, ...)
```

## Arguments

- object:

  a `pop_data` object

- ...:

  unused

## Value

a [character](https://rdrr.io/r/base/character.html) string identifying
the biomarker names column in `object`

## Examples

``` r
sees_pop_data_100 |> get_biomarker_names_var()
#> [1] "antigen_iso"
```
