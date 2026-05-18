# Plot package download counts over time by source

Fetches download data from CRAN (via `cranlogs`) and optionally GitHub
Releases (via `gh`), then plots new and cumulative downloads.

## Usage

``` r
graph_downloads(
  github = FALSE,
  new = TRUE,
  cumulative = TRUE,
  start = NULL,
  unit = c("month", "day", "week", "quarter", "year"),
  title
)
```

## Arguments

- github:

  Logical; include GitHub release downloads? Defaults to `FALSE`.

- new:

  Logical; include new (daily) downloads? Defaults to `TRUE`.

- cumulative:

  Logical; include cumulative downloads? Defaults to `TRUE`.

- start:

  Start date for the plot (a [Date](https://rdrr.io/r/base/Dates.html)
  or string coercible to one). Defaults to `NULL` (all available data).

- unit:

  Character string specifying the time unit to aggregate by. One of
  `"day"`, `"week"`, `"month"`, `"quarter"`, or `"year"`. Defaults to
  `"month"`.

- title:

  Character string for the plot title. Defaults to a description
  including the time unit. Set to `NULL` to omit.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object with faceted panels.

## Details

GitHub release downloads are cumulative counts per release asset from
the GitHub API. New GitHub downloads are derived as the contribution of
each release. CRAN downloads are fetched via
[`packageRank::cranDownloads()`](https://rdrr.io/pkg/packageRank/man/cranDownloads.html).

Requires the `packageRank` package (and `gh` if `github = TRUE`), listed
under `Suggests`.
