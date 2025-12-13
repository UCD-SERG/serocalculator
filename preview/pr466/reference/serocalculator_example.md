# Get path to an example file

The
[serocalculator](https:/ucd-serg.github.io/serocalculator/preview/pr466/reference/serocalculator-package.md)
package comes bundled with a number of sample files in its
`inst/extdata` directory. This `serocalculator_example()` function make
those sample files easy to access.

## Usage

``` r
serocalculator_example(file = NULL)
```

## Arguments

- file:

  Name of file. If `NULL`, the example files will be listed.

## Value

a [character](https://rdrr.io/r/base/character.html) string providing
the path to the file specified by `file`, or a vector or available files
if `file = NULL`.

## Details

Adapted from
[`readr::readr_example()`](https://readr.tidyverse.org/reference/readr_example.html)
following the guidance in
<https://r-pkgs.org/data.html#sec-data-example-path-helper>.

## Examples

``` r
serocalculator_example()
#> [1] "example_curve_params.csv" "example_curve_params.rds"
#> [3] "example_noise_params.csv" "example_noise_params.rds"
#> [5] "example_pop_data.csv"     "example_pop_data.rds"    
serocalculator_example("example_pop_data.csv")
#> /home/runner/work/_temp/Library/serocalculator/extdata/example_pop_data.csv
```
