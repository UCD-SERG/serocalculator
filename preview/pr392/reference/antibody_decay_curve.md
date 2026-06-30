# Antibody decay curve with natural parameters

Antibody decay curve with natural parameters

## Usage

``` r
antibody_decay_curve(
  t,
  y0 = 0.74916052,
  b0 = 1,
  mu_b = 0.18432798,
  mu_y = 0.36853621,
  gamma = 0.0013040664,
  alpha = 2.192627e-05,
  rho = 2
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

- alpha:

  antibody decay rate

- rho:

  antibody decay rate power function parameter

## Value

a [`numeric()`](https://rdrr.io/r/base/numeric.html) vector

## Examples

``` r
library(ggplot2)
ggplot() + geom_function(fun = antibody_decay_curve) + xlim(1, 100)
```
