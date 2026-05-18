# Combine, filter, and pivot download data for plotting

Combine, filter, and pivot download data for plotting

## Usage

``` r
.prepare_download_data(cran_data, github_data, start, metrics)
```

## Arguments

- cran_data:

  Tibble of CRAN download data from
  [`.fetch_cran_downloads()`](https://ucd-serg.github.io/serocalculator/reference/dot-fetch_cran_downloads.md).

- github_data:

  Tibble of GitHub download data from
  [`.fetch_github_downloads()`](https://ucd-serg.github.io/serocalculator/reference/dot-fetch_github_downloads.md),
  or `NULL`.

- start:

  Start date for filtering, or `NULL` for no filtering.

- metrics:

  Character vector of metric columns to include (subset of
  `c("new", "cumulative")`).

## Value

A long-format tibble with columns `date`, `provider`, `metric`, and
`downloads`.
