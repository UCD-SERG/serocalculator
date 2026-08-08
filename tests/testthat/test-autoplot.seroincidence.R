test_that("`autoplot()` errors when the fit carries no log-likelihood graph", {
  est_no_graph <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    build_graph = FALSE
  )

  # The message names `build_graph`, so a reader learns the fix from the
  # error alone; assert on that rather than only on the first clause.
  expect_error(autoplot(est_no_graph), "build_graph")
})

test_that("`autoplot()` returns a ggplot when the graph was built", {
  est_with_graph <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    build_graph = TRUE
  )

  expect_s3_class(autoplot(est_with_graph), "ggplot")
  expect_s3_class(autoplot(est_with_graph, log_x = TRUE), "ggplot")
})
