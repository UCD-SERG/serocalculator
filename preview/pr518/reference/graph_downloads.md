# Plot package download counts over time by source

Fetches download data from CRAN (via `packageRank`) and optionally
GitHub Releases (via `gh`), then plots new and cumulative downloads.

## Usage

``` r
graph_downloads(...)
```

## Arguments

- ...:

  Arguments passed on to
  [`.get_download_data`](https://ucd-serg.github.io/serocalculator/reference/dot-get_download_data.md)

  `github`

  :   Logical; include GitHub release downloads? Defaults to `FALSE`.

  `new`

  :   Logical; include new (daily) downloads? Defaults to `TRUE`.

  `cumulative`

  :   Logical; include cumulative downloads? Defaults to `TRUE`.

  `start`

  :   Start date for the plot (a
      [Date](https://rdrr.io/r/base/Dates.html) or string coercible to
      one). Defaults to `NULL` (all available data). Compared against
      the period start after aggregation, so e.g. `start = "2025-06-15"`
      with `unit = "month"` drops June 2025 (its label is `2025-06-01`);
      pass period boundaries for predictable filtering.

  `unit`

  :   Character string specifying the time unit to aggregate by. One of
      `"day"`, `"week"`, `"month"`, `"quarter"`, or `"year"`. Defaults
      to `"month"`.

  `title`

  :   Character string for the plot title. Defaults to a description
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
