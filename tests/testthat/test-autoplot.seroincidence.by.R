test_that("Build graphs not TRUE", {

  est2 <- est_seroincidence_by(
     strata = c("catchment"),
     pop_data = sees_pop_data_pk_100,
     curve_params = typhoid_curves_nostrat_100,
     curve_strata_varnames= NULL,
     noise_strata_varnames = NULL,
     noise_params = example_noise_params_pk,
     antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
     build_graph = FALSE
   ) |>
   autoplot(est2) |>

   expect_error()
})

test_that("Build graphs works as expected", {

  est2 <- est_seroincidence_by(
    strata = c("catchment"),
    pop_data = sees_pop_data_pk_100,
    curve_params = typhoid_curves_nostrat_100,
    curve_strata_varnames= NULL,
    noise_strata_varnames = NULL,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    build_graph = TRUE
  ) |>
    autoplot() |>
    vdiffr::expect_doppelganger(title = "seroinc-plot")
})

