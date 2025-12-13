# Extract the `Strata` attribute from an object, if present

Extract the `Strata` attribute from an object, if present

## Usage

``` r
# Default S3 method
strata(x)
```

## Arguments

- x:

  any R object

## Value

- a
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  with strata in rows, or

- `NULL` if `x` does not have a `"strata"` attribute
