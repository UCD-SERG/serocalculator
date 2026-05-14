# Snapshot testing for [data.frame](https://rdrr.io/r/base/data.frame.html)s

copied from <https://github.com/bcgov/ssdtools> with permission
(<https://github.com/bcgov/ssdtools/issues/379>)

## Usage

``` r
expect_snapshot_data(x, name, digits = 6)
```

## Arguments

- x:

  a [data.frame](https://rdrr.io/r/base/data.frame.html) to snapshot

- name:

  [character](https://rdrr.io/r/base/character.html) snapshot name

- digits:

  [integer](https://rdrr.io/r/base/integer.html) passed to
  [`signif()`](https://rdrr.io/r/base/Round.html) for numeric variables

## Value

[NULL](https://rdrr.io/r/base/NULL.html) (from
[`testthat::expect_snapshot_file()`](https://testthat.r-lib.org/reference/expect_snapshot_file.html))

## Examples

``` r
if (FALSE) { # \dontrun{
expect_snapshot_data(iris, name = "iris")
} # }
```
