# `test_missing_strata()` errors on absent strata

    Code
      warn_missing_strata(sees_pop_data_pk_100, strata = expected_strata, dataname = "sees_pop_data_pk_100")
    Condition <absent strata levels>
      Error in `warn_missing_strata()`:
      ! Missing strata levels in `sees_pop_data_pk_100`
      i The following strata variables are present, but the following specific combinations of those strata are missing:
      Country
      1 US

# `test_missing_strata()` warns on missing strata vars

    Code
      warn_missing_strata(sees_pop_data_pk_100, strata = expected_strata, dataname = "sees_pop_data_pk_100")
    Condition <missing strata vars>
      Warning:
      `sees_pop_data_pk_100` is missing `place` and will only be stratified by `Country`
      i To avoid this warning, specify the desired set of stratifying variables in the `curve_strata_varnames` and `noise_strata_varnames` arguments to `est_seroincidence_by()`.
    Output
      [1] "Country"

