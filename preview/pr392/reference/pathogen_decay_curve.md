# Pathogen decay curve

Pathogen decay curve

## Usage

``` r
pathogen_decay_curve(
  t,
  y0 = 0.74916052,
  b0 = 1,
  mu_b = 0.18432798,
  mu_y = 0.36853621,
  gamma = 0.0013040664
)
```

## Arguments

- t:

  time since infection

- y0:

  initial antibody concentration

- b0:

  initial bacteria concentration

- mu_b:

  pathogen reproduction rate

- mu_y:

  antibody reproduction rate

- gamma:

  bacteria destruction rate per antibody

## Value

a [`numeric()`](https://rdrr.io/r/base/numeric.html)

## Examples

``` r
library(ggplot2)
ggplot() + geom_function(
  fun = pathogen_decay_curve,
  args = list(
    y0 = 0.74916052,
    b0 = 1,
    mu_b = 0.18432798,
    mu_y = 0.36853621,
    gamma = 0.0013040664
  )
) + xlim(0, 100)
```
