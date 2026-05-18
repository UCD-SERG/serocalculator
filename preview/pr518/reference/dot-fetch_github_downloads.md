# Fetch and aggregate GitHub release download counts

Queries the GitHub API for all releases of the `serocalculator` package,
computes daily new and cumulative downloads, then aggregates by `unit`.

## Usage

``` r
.fetch_github_downloads(unit)
```

## Arguments

- unit:

  Character string passed to
  [`cut.Date()`](https://rdrr.io/r/base/cut.POSIXt.html) for time
  aggregation (e.g. `"month"`, `"week"`).

## Value

A tibble with columns `date`, `provider`, `new`, and `cumulative`.
