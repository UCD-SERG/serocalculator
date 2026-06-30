# Calculate peak antibody concentration

Calculate peak antibody concentration

## Usage

``` r
y1f(
  y0 = 0.74916052,
  mu_y = 0.36853621,
  t1 = t1f(y0 = y0, mu_y = mu_y, ...),
  ...
)
```

## Arguments

- y0:

  initial antibody concentration

- mu_y:

  antibody reproduction rate

- ...:

  Arguments passed on to
  [`t1f`](https://ucd-serg.github.io/serocalculator/reference/t1f.md)

  `b0`

  :   initial bacteria concentration

  `mu_b`

  :   pathogen reproduction rate

  `gamma`

  :   bacteria destruction rate per antibody

## Value

a [`numeric()`](https://rdrr.io/r/base/numeric.html) vector

## Examples

``` r
y1f(y0 = 1, mu_y = 1, t1 = 10)
#> [1] 22026.47
```
