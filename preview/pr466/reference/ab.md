# kinetics of the antibody (ab) response (power function decay)

kinetics of the antibody (ab) response (power function decay)

## Usage

``` r
ab(t, par, ...)
```

## Arguments

- t:

  [numeric](https://rdrr.io/r/base/numeric.html)
  [vector](https://rdrr.io/r/base/vector.html) of elapsed times since
  start of infection

- par:

  [numeric](https://rdrr.io/r/base/numeric.html)
  [matrix](https://rdrr.io/r/base/matrix.html) of model parameters:

  - rows are parameters

  - columns are biomarkers

- ...:

  Arguments passed on to
  [`baseline`](https:/ucd-serg.github.io/serocalculator/preview/pr466/reference/baseline.md)

  `yvec`

  :   a [numeric](https://rdrr.io/r/base/numeric.html)
      [vector](https://rdrr.io/r/base/vector.html) of predicted
      biomarker values, for one biomarker

  `kab`

  :   [integer](https://rdrr.io/r/base/integer.html) indicating which
      row to read from `blims`

  `blims`

  :   range of possible baseline antibody levels

## Value

a [`matrix()`](https://rdrr.io/r/base/matrix.html) of predicted
biomarker values

## Examples

``` r
par1 <- matrix(
    c(
      1.11418923843475, 1, 0.12415057798022207, 0.24829344792968783,
      0.01998946878312856, 0.0012360802436587237, 1.297194045996013,
      1.3976510415108334, 1, 0.2159993563893431, 0.4318070551383313,
      0.0015146395107173347, 0.0003580062906750277, 1.5695811573082081
    ),
    nrow = 7L,
    ncol = 2L,
    dimnames = list(
      params = c("y0", "b0", "mu0", "mu1", "c1", "alpha", "shape_r"),
      antigen_iso = c("HlyE_IgA", "HlyE_IgG")
    )
    )
t <- 0:1444
blims <- matrix(
   rep(c(0, 0.5), each = 2L),
   nrow = 2L,
   ncol = 2L,
   dimnames = list(c("HlyE_IgA", "HlyE_IgG"), c("min", "max"))
   )
preds <- serocalculator:::ab(t = t, par = par1, blims = blims)
```
