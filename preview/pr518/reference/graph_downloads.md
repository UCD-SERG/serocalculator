# Plot package download counts over time by source

Plots new and cumulative download counts for this package. Delegates to
[`gdl::graph_downloads()`](https://rdrr.io/pkg/gdl/man/graph_downloads.html).

## Usage

``` r
graph_downloads(
  package = "serocalculator",
  github_repo = "UCD-SERG/serocalculator",
  new = TRUE,
  cumulative = TRUE,
  unit = c("month", "day", "week", "quarter", "year"),
  start = NULL,
  title,
  github = FALSE
)
```

## Arguments

- package:

  Character string; the CRAN package name. Defaults to
  `"serocalculator"`.

- github_repo:

  Character string; the GitHub repository in `"owner/repo"` format, used
  when `github = TRUE`. Defaults to `"UCD-SERG/serocalculator"`.

- new:

  Logical; include new (period) downloads? Defaults to `TRUE`.

- cumulative:

  Logical; include cumulative downloads? Defaults to `TRUE`.

- unit:

  Character string specifying the time unit to aggregate by. One of
  `"day"`, `"week"`, `"month"`, `"quarter"`, or `"year"`. Defaults to
  `"month"`.

- start:

  Start date for the plot (a [Date](https://rdrr.io/r/base/Dates.html)
  or string coercible to one). Defaults to `NULL` (all available data).

- title:

  Character string for the plot title. Defaults to a description
  including the package name and time unit. Set to `NULL` to omit.

- github:

  Logical; include GitHub release downloads? Defaults to `FALSE`.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object with faceted panels.
