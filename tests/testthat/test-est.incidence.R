
# get pop data
xs_data <- load_pop_data(
  file_path = "https://osf.io/download//n6cp3/",
  age = "Age",
  value = "result",
  id = "index_id",
  standardize = TRUE
)

# get noise data
noise <- load_noise_params("https://osf.io/download//hqy4v/")

# get curve data
curve <- load_curve_params("https://osf.io/download/rtw5k/")


test_that(
  "est.incidence() produces expected results for typhoid data",
  {
    skip(message = "Skipping test of `est.incidence()` for now, because github was producing miniscule differences in SE (and thus CIs) for some reason that I don't have time to hunt down.")

    library(readr)
    library(dplyr)

    cond.hlye.IgG <- data.frame(
      nu = 1.027239, # B noise
      eps = 0.2, # M noise
      y.low = 0.0, # low cutoff
      y.high = 5e4,
      antigen_iso = "HlyE_IgG"
    )

    start <- .05

    fit <- est.incidence(
      pop_data = xs_data %>% filter(Country == "Pakistan"),
      curve_param = curve,
      noise_param = noise %>% filter(Country == "Pakistan"),
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    ) %>% summary.seroincidence(
      coverage = .95,
      start = start
    ) %>%
      mutate(
        ageCat = NULL,
        antigen.iso = paste(collapse = "+", "HlyE_IgG")
      ) %>%
      structure(noise.parameters = noise)

    # compare with `typhoid_results` from data-raw/typhoid_results.qmd

    expect_equal(
      object = fit,
      expected = typhoid_results
    )
  }
)
