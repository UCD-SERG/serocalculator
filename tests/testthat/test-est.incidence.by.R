
test_that("est.incidence.by() issues warning when strata is missing", {

  # get pop data
  xs_data <- load_pop_data(
    file_path = "https://osf.io/download//n6cp3/",
    age = "Age",
    value = "result",
    id = "index_id",
    standardize = TRUE
  ) %>%
    filter(Country == "Pakistan") %>%
    slice_head(n = 100)

  # get noise data
  noise <- load_noise_params("https://osf.io/download//hqy4v/") %>%
    filter(Country == "Pakistan")

  # get curve data
  curve <- load_curve_params("https://osf.io/download/rtw5k/")

  expect_warning(
    est.incidence.by(
      pop_data = xs_data,
      curve_params = curve,
      noise_params = noise,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    ),
    regexp = "The `strata` argument to `est.incidence.by\\(\\)` is missing."
  )

})
