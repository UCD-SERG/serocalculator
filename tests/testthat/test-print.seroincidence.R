test_that("results are consistent", {

  library(dplyr)
  xs_data <-
    sees_pop_data_pk_100
  curve <-
    typhoid_curves_nostrat_100 %>%
    filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))
  noise <-
    example_noise_params_pk
  est1 <- estimate_scr(
    pop_data = xs_data,
    curve_params = curve,
    noise_params = noise,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
  )
  expect_snapshot(print(est1))
})
