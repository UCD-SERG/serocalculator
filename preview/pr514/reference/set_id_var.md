# Specify person ID column

Sets the `id_var` metadata attribute of `object`

## Usage

``` r
set_id_var(object, id = "index_id", standardize = TRUE, ...)
```

## Arguments

- object:

  a [data.frame](https://rdrr.io/r/base/data.frame.html)

- id:

  [character](https://rdrr.io/r/base/character.html) string at least
  partially matching a column in `object`

- standardize:

  whether to rename the column specified by `id` to "id"

- ...:

  unused

## Value

a modified version of `object`

## Examples

``` r
serocalculator_example("example_pop_data.rds") |>
  readr::read_rds() |>
    set_id_var(id = "index_id") |>
    attr("id_var")
#> [1] "id"
```
