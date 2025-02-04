# `test_missing_strata()` errors on absent strata

    Code
      warn_missing_strata(iris, expected_strata, dataname = "iris")
    Condition <absent strata levels>
      Error in `warn_missing_strata()`:
      ! Missing strata levels in `iris`
      i The following strata variables are present, but the following specific combinations of those strata are missing:
      Species
      1 banana

# `test_missing_strata()` warns on missing strata vars

    Code
      warn_missing_strata(iris, expected_strata, dataname = "iris")
    Condition <missing strata vars>
      Warning:
      `iris` is missing `type` and will only be stratified by `Species`
      i To avoid this warning, specify the desired set of stratifying variables in the `curve_strata_varnames` and `noise_strata_varnames` arguments to `est.incidence.by()`.
    Output
      [1] "Species"

