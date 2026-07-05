# Extract antibody measurement values

Extract antibody measurement values

## Usage

``` r
get_values_var(object, ...)
```

## Arguments

- object:

  a `pop_data` object

- ...:

  unused

## Value

the name of the column in `object` specified as containing antibody
abundance measurements

## Examples

``` r
sees_pop_data_100 |> get_values_var()
#> [1] "value"
```
