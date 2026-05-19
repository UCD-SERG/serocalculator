# Fetch and prepare download data for plotting

Fetches CRAN (and optionally GitHub) download data, then combines,
filters, and pivots into long format.

## Usage

``` r
.get_download_data(
  github = FALSE,
  new = TRUE,
  cumulative = TRUE,
  start = NULL,
  unit = c("month", "day", "week", "quarter", "year"),
  title,
  ...
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
  Compared against the period start after aggregation, so e.g.
  `start = "2025-06-15"` with `unit = "month"` drops June 2025 (its
  label is `2025-06-01`); pass period boundaries for predictable
  filtering.

- unit:

  Character string specifying the time unit to aggregate by. One of
  `"day"`, `"week"`, `"month"`, `"quarter"`, or `"year"`. Defaults to
  `"month"`.

- title:

  Character string for the plot title. Defaults to a description
  including the time unit. Set to `NULL` to omit.

- ...:

  Arguments passed on to
  [`packageRank::cranDownloads`](https://rdrr.io/pkg/packageRank/man/cranDownloads.html)

  `package`

  :   A character vector, the packages to query, or `NULL` for a sum of
      downloads for all packages. Alternatively, it can also be `"R"`,
      to query downloads of R itself. `"R"` cannot be mixed with
      package.

  `when`

  :   `last-day`, `last-week` or `last-month`. If this is given, then
      `from` and `to` are ignored.

  `from`

  :   Start date as `yyyy-mm-dd`, `yyyy-mm` or `yyyy`.

  `to`

  :   End date as `yyyy-mm-dd`, `yyyy-mm` or `yyyy`.

  `check.package`

  :   Logical. Validate and "spell check" package.

  `dev.mode`

  :   Logical. Use validatePackage0() to scrape CRAN.

  `fix.cranlogs`

  :   Logical. Use RStudio logs to fix 8 dates with duplicated data in
      'cranlogs' results.

  `pro.mode`

  :   Logical. Faster but fewer checks/features. Closer to
      cranlogs::cran_downloads() but with cranDownloads()'s plot method.

## Value

A `download_data` tibble (subclass of `tbl_df`) with columns `date`,
`provider`, `metric`, and `downloads`, plus attributes `title`,
`github`, and `multi_metric`.
