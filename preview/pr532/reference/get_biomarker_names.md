# Extract biomarker names column

Extract biomarker names column

## Usage

``` r
get_biomarker_names(object, ...)
```

## Arguments

- object:

  a long-format dataset (`pop_data`, `curve_params`, etc)

- ...:

  unused

## Value

a [character](https://rdrr.io/r/base/character.html) or
[factor](https://rdrr.io/r/base/factor.html)
[vector](https://rdrr.io/r/base/vector.html) of biomarker names

## Examples

``` r
sees_pop_data_100 |> get_biomarker_names() |> head()
#> [1] HlyE_IgA HlyE_IgA HlyE_IgA HlyE_IgA HlyE_IgA HlyE_IgA
#> Levels: HlyE_IgA HlyE_IgG
```
