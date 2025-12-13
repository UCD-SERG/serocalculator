# Plot distribution of antibodies

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
method for `pop_data` objects

## Usage

``` r
# S3 method for class 'pop_data'
autoplot(object, log = FALSE, type = "density", strata = NULL, ...)
```

## Arguments

- object:

  A `pop_data` object (from
  [`load_pop_data()`](https:/ucd-serg.github.io/serocalculator/preview/pr466/reference/load_pop_data.md))

- log:

  whether to show antibody responses on logarithmic scale

- type:

  an option to choose type of chart: the current options are `"density"`
  or `"age-scatter"`

- strata:

  the name of a variable in `pop_data` to stratify by (or `NULL` for no
  stratification)

- ...:

  unused

## Value

a [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object

## Examples

``` r
# \donttest{
library(dplyr)
library(ggplot2)
library(magrittr)

xs_data <-
  serocalculator_example("example_pop_data.csv") |>
  read.csv() |>
  as_pop_data()

xs_data |> autoplot(strata = "catchment", type = "density")

xs_data |> autoplot(strata = "catchment", type = "age-scatter")

# }
```
