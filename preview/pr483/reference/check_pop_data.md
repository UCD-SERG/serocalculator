# Check the formatting of a cross-sectional antibody survey dataset.

Check the formatting of a cross-sectional antibody survey dataset.

## Usage

``` r
check_pop_data(pop_data, verbose = FALSE)
```

## Arguments

- pop_data:

  dataset to check

- verbose:

  whether to print an "OK" message when all checks pass

## Value

NULL (invisibly)

## Examples

``` r
library(magrittr)

xs_data <-
  serocalculator_example("example_pop_data.csv") |>
  read.csv() |>
  as_pop_data()

check_pop_data(xs_data, verbose = TRUE)
#> data format is as expected.
```
