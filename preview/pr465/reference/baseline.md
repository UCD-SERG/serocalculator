# Substitute baseline values

whenever y is below a cutoff (`blims[kab,2]`), substitute a random
sample from a baseline distribution

## Usage

``` r
baseline(kab, yvec, blims, ...)
```

## Arguments

- kab:

  [integer](https://rdrr.io/r/base/integer.html) indicating which row to
  read from `blims`

- yvec:

  a [numeric](https://rdrr.io/r/base/numeric.html)
  [vector](https://rdrr.io/r/base/vector.html) of predicted biomarker
  values, for one biomarker

- blims:

  range of possible baseline antibody levels

- ...:

  unused

## Value

an altered version of `yvec`
