# Aggregate download data by a calendar unit

Sums new downloads and takes the last cumulative value within each time
period.

## Usage

``` r
.aggregate_by_unit(data, unit = c("month", "day", "week", "quarter", "year"))
```

## Arguments

- data:

  A tibble with columns `date`, `provider`, `new`, and `cumulative`.

- unit:

  Character string passed to
  [`cut.Date()`](https://rdrr.io/r/base/cut.POSIXt.html) for time
  aggregation. One of `"month"`, `"day"`, `"week"`, `"quarter"`, or
  `"year"`.

## Value

A tibble with columns `date`, `provider`, `new`, and `cumulative`,
aggregated by `unit`.
