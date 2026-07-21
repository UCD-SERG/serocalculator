# Calculate mean antibody response using 5-parameter model

Calculate mean antibody response using 5-parameter model

## Usage

``` r
ab_5p(t, y0, y1, t1, alpha, shape)
```

## Arguments

- t:

  time since last exposure

- y0:

  antibody response at exposure

- y1:

  antibody response at t1

- t1:

  time from last exposure to peak response

- alpha:

  decay power function coefficient

- shape:

  decay power function exponent
  [numeric](https://rdrr.io/r/base/numeric.html) scalar; uses r \> 1
  scale for shape

## Value

a [numeric](https://rdrr.io/r/base/numeric.html)
[vector](https://rdrr.io/r/base/vector.html)

## Examples

``` r
params <- typhoid_curves_nostrat_100[1, ]
ab_5p(
  t = units::as_units(50, "days"),
  y0 = params$y0,
  y1 = params$y1,
  t1 = params$t1 |> units::as_units("days"),
  alpha = params$alpha |> units::as_units("1/days"),
  shape = params$r)
#> [1] 40.96569
```
