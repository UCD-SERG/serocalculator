# Extract biomarker levels

Extract biomarker levels

## Usage

``` r
get_biomarker_levels(object, ...)
```

## Arguments

- object:

  a `pop_data` object

- ...:

  unused

## Value

the biomarker levels in `object`

## Examples

``` r
sees_pop_data_100 |> get_biomarker_levels()
#> [1] "HlyE_IgA" "HlyE_IgG"
```
