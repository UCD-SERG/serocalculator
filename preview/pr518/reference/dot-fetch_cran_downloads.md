# Fetch and aggregate CRAN download counts

Uses
[`packageRank::cranDownloads()`](https://rdrr.io/pkg/packageRank/man/cranDownloads.html)
to fetch daily CRAN download data with cumulative counts, then
aggregates by `unit`.

## Usage

``` r
.fetch_cran_downloads(unit, ...)
```

## Arguments

- unit:

  Character string passed to
  [`cut.Date()`](https://rdrr.io/r/base/cut.POSIXt.html) for time
  aggregation (e.g. `"month"`, `"week"`).

- ...:

  Additional arguments passed to
  [`packageRank::cranDownloads()`](https://rdrr.io/pkg/packageRank/man/cranDownloads.html).

## Value

A tibble with columns `date`, `provider`, `new`, and `cumulative`.
