test_that(
  desc = "print() method works consistently",
  code = {

    withr::local_options(width = 80)
    typhoid_results <- estimate_scr_by(
      strata = "catchment",
      pop_data = sees_pop_data_pk_100,
      curve_param = typhoid_curves_nostrat_100,
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      noise_param = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      # Allow for parallel processing to decrease run time
      num_cores = 1
    )

    expect_snapshot(x = print(typhoid_results))

})
