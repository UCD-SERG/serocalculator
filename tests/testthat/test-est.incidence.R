test_that(
  "est.incidence() produces expected results for typhoid data",
  {
    # get pop data
    xs_data <- load_pop_data(
      file_path = "https://osf.io/download//n6cp3/",
      age = "Age",
      value = "result",
      id = "index_id",
      standardize = TRUE
    ) %>%
      filter(Country == "Pakistan")

    # get noise data
    noise <- load_noise_params("https://osf.io/download//hqy4v/") %>%
      filter(Country == "Pakistan")

    # get curve data
    curve <- load_curve_params("https://osf.io/download/rtw5k/")

    # set start
    start <- .05

    typhoid_results <- est.incidence(
      pop_data = xs_data,
      curve_param = curve,
      noise_param = noise,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    ) %>%
      summary.seroincidence(
        coverage = .95,
        start = start
      ) %>%
      mutate(
        ageCat = NULL,
        antigen.iso = paste(collapse = "+", "HlyE_IgG")
      ) %>%
      structure(noise.parameters = noise)

    expect_snapshot(x = typhoid_results)
  }
)

test_that("`est.incidence()` produces expected results", {
  curves <- load_curve_params("https://osf.io/download/rtw5k/")
  noise <- load_noise_params("https://osf.io/download//hqy4v/")
  xs_data_true <- load_pop_data(
    file_path = "https://osf.io/download//n6cp3/",
    age = "Age",
    value = "result",
    id = "index_id",
    standardize = TRUE
  )

  est_true <- est.incidence(
    pop_data = xs_data_true %>% filter(Country == "Pakistan"),
    curve_params = curves,
    noise_params = noise %>% filter(Country == "Pakistan"),
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  xs_data_false <- load_pop_data(
    file_path = "https://osf.io/download//n6cp3/",
    age = "Age",
    value = "result",
    id = "index_id",
    standardize = FALSE
  )

  est_false <- est.incidence(
    pop_data = xs_data_false %>% filter(Country == "Pakistan"),
    curve_params = curves,
    noise_params = noise %>% filter(Country == "Pakistan"),
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  expect_equal(est_true, est_false)
})
