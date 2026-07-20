# Get person ID variable name

Get person ID variable name

## Usage

``` r
ids_varname(object, ...)
```

## Arguments

- object:

  a [data.frame](https://rdrr.io/r/base/data.frame.html)

- ...:

  unused

## Value

a [character](https://rdrr.io/r/base/character.html) string containing
the person ID column, as recorded in the metadata of `object`

## Examples

``` r
ids_varname(sees_pop_data_pk_100)
#> [1] "id"
```
