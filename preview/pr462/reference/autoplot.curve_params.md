# Graph antibody decay curves by antigen isotype

Graph antibody decay curves by antigen isotype

## Usage

``` r
# S3 method for class 'curve_params'
autoplot(
  object,
  method = c("graph.curve.params", "graph_seroresponse_model_1"),
  ...
)
```

## Arguments

- object:

  a `curve_params` object (constructed using
  [`as_sr_params()`](https:/ucd-serg.github.io/serocalculator/preview/pr462/reference/as_sr_params.md)),
  which is a [`data.frame()`](https://rdrr.io/r/base/data.frame.html)
  containing MCMC samples of antibody decay curve parameters

- method:

  a [character](https://rdrr.io/r/base/character.html) string indicating
  whether to use

  - [`graph.curve.params()`](https:/ucd-serg.github.io/serocalculator/preview/pr462/reference/graph.curve.params.md)
    (default) or

  - [`graph_seroresponse_model_1()`](https:/ucd-serg.github.io/serocalculator/preview/pr462/reference/graph_seroresponse_model_1.md)
    (previous default) as the graphing method.

- ...:

  additional arguments passed to the sub-function indicated by the
  `method` argument.

## Value

a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object

## Details

Currently, the backend for this method is
[`graph.curve.params()`](https:/ucd-serg.github.io/serocalculator/preview/pr462/reference/graph.curve.params.md).
Previously, the backend for this method was
[`graph_seroresponse_model_1()`](https:/ucd-serg.github.io/serocalculator/preview/pr462/reference/graph_seroresponse_model_1.md).
That function is still available if preferred.

## Examples

``` r
# \donttest{
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
library(magrittr)

curve <-
  serocalculator_example("example_curve_params.csv") |>
  read.csv() |>
  as_sr_params() |>
  filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) |>
  autoplot()

curve

# }
```
