# Compare seroincidence rates between two groups

Perform a two-sample z-test to compare seroincidence rates between two
groups. Since we use maximum likelihood estimation (MLE) for each
seroincidence estimate and estimates from different strata or data sets
are uncorrelated, we can use a simple two-sample z-test using the
Gaussian distribution. The standard error for the difference is computed
by adding the estimated variances and taking the square root.

## Usage

``` r
compare_seroincidence(x, y = NULL, coverage = 0.95, verbose = FALSE, ...)

# S3 method for class 'seroincidence'
compare_seroincidence(x, y = NULL, coverage = 0.95, verbose = FALSE, ...)

# S3 method for class 'seroincidence.by'
compare_seroincidence(x, y = NULL, coverage = 0.95, verbose = FALSE, ...)
```

## Arguments

- x:

  A `"seroincidence"` object from
  [`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence.md)
  or a `"seroincidence.by"` object from
  [`est_seroincidence_by()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence_by.md)

- y:

  A `"seroincidence"` object from
  [`est_seroincidence()`](https://ucd-serg.github.io/serocalculator/reference/est_seroincidence.md)
  (optional if `x` is a `"seroincidence.by"` object)

- coverage:

  Desired confidence interval coverage probability (default = 0.95)

- verbose:

  Logical indicating whether to print verbose messages (default = FALSE)

- ...:

  Additional arguments (currently unused)

## Value

- When comparing two `"seroincidence"` objects: An object of class
  `"htest"` containing the test statistic, p-value, confidence interval,
  and estimates.

- When applied to a `"seroincidence.by"` object: A
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  with columns for each pair of strata, the difference in incidence
  rates, standard error, z-statistic, p-value, and confidence interval
  bounds.

## Details

When comparing two single `"seroincidence"` objects, this function
performs a two-sample z-test and returns results in the standard `htest`
format.

When applied to a `"seroincidence.by"` object (stratified estimates),
the function compares all pairs of strata and returns a nicely formatted
table with point estimates for the difference in seroincidence,
p-values, and confidence intervals.

The test statistic is computed as: \$\$z = \frac{\lambda_1 -
\lambda_2}{\sqrt{SE_1^2 + SE_2^2}}\$\$

where \\\lambda_1\\ and \\\lambda_2\\ are the estimated incidence rates,
and \\SE_1\\ and \\SE_2\\ are their standard errors.

## Methods (by class)

- `compare_seroincidence(seroincidence)`: Compare two single
  seroincidence estimates

- `compare_seroincidence(seroincidence.by)`: Compare all pairs of
  stratified seroincidence estimates

## Examples

``` r
if (FALSE) { # \dontrun{
# See inst/examples/exm-compare_seroincidence.R for complete examples
} # }
```
