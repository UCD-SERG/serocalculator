# Calculate time to end of active infection

Calculate time to end of active infection

## Usage

``` r
t1f(
  y0 = 0.74916052,
  b0 = 1,
  mu_b = 0.18432798,
  mu_y = 0.36853621,
  gamma = 0.0013040664
)
```

## Arguments

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

a [`numeric()`](https://rdrr.io/r/base/numeric.html) vector

## Examples

``` r
t1f(
  y0 = 0.74916052,
  b0 = 1,
  mu_b = 0.18432798,
  mu_y = 0.36853621,
  gamma = 0.0013040664
)
#> [1] 28.47143
```
