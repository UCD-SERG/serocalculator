
test_that("est.incidence.by() issues warning when strata is missing", {
  library(dplyr)
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


test_that(
  "est.incidence.by() produces expected results for typhoid data",
  {
    library(dplyr)
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
    # slice if test is too slow (.by = antigen_iso))

    # set start
    start <- .05

    typhoid_results <- est.incidence.by(
      strata = c("catchment"),
      pop_data = xs_data,
      curve_param = curve,
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      noise_param = noise,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      # Allow for parallel processing to decrease run time
      num_cores = 1
    )

    expect_snapshot(x = typhoid_results)

    expect_snapshot_value(typhoid_results, style = "deparse")
  }
)

test_that("`est.incidence.by()` produces expected results", {
  library(dplyr)
  curves <- load_curve_params("https://osf.io/download/rtw5k/")
  noise <- load_noise_params("https://osf.io/download//hqy4v/")

  xs_data_true <- load_pop_data(
    file_path = "https://osf.io/download//n6cp3/",
    age = "Age",
    value = "result",
    id = "index_id",
    standardize = TRUE
  ) %>%
    filter(Country == "Pakistan") %>%
    slice_head(n = 100)


  est_true <- est.incidence.by(
    strata = c("catchment"),
    pop_data = xs_data_true,
    curve_params = curves,
    noise_params = noise %>% filter(Country == "Pakistan"),
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL,
    num_cores = 1 # Allow for parallel processing to decrease run time
  )

  xs_data_false <- load_pop_data(
    file_path = "https://osf.io/download//n6cp3/",
    age = "Age",
    value = "result",
    id = "index_id",
    standardize = FALSE
  ) %>%
    filter(Country == "Pakistan") %>%
    slice_head(n = 100)


  est_false <- est.incidence.by(
    strata = c("catchment"),
    pop_data = xs_data_false,
    curve_params = curves,
    noise_params = noise %>% filter(Country == "Pakistan"),
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    num_cores = 1 # Allow for parallel processing to decrease run time
  )

  expect_equal(est_true, est_false)
})
