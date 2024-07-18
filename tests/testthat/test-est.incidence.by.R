test_that(
  desc = "`est.incidence.by()` produces consistent results",

  code =     {
    withr::local_options(width = 80)
    library(dplyr)

    xs_data <-
      load_pop_data("https://osf.io/download//n6cp3/") %>%
      filter(Country == "Pakistan") %>%
      slice_head(n = 100, by = c("catchment", "antigen_iso"))

    curve <- load_curve_params("https://osf.io/download/rtw5k/") %>%
      filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) %>%
      slice_head(n = 100, by = antigen_iso) # Reduce dataset for the purposes of this example

    noise <- load_noise_params("https://osf.io/download//hqy4v/")

    strat_ests = est.incidence.by(
      strata = "catchment",
      pop_data = xs_data,
      curve_params = curve,
      curve_strata_varnames = NULL,
      noise_params = noise %>% filter(Country == "Pakistan"),
      noise_strata_varnames = NULL,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      # verbose = TRUE,
      num_cores = 1
    )

    expect_snapshot(x = strat_ests)

    expect_snapshot_value(
      x = strat_ests,
      tolerance = 10^-3,
      style = "deparse"
    )

    strat_ests_summary = summary(strat_ests)

    expect_snapshot(x = strat_ests_summary)

    expect_snapshot_value(
      x = strat_ests_summary,
      tolerance = 10^-3,
      style = "serialize"
    )

  }
)

test_that("est.incidence.by() warns about missing data", {

  library(dplyr)
  library(readr)

  xs_data <-
    read_rds("https://osf.io/download//n6cp3/")  %>%
    as_pop_data() %>%
    filter(Country == "Nepal") %>%
    slice_head(n = 100, by = "antigen_iso")

  curve <-
    load_curve_params("https://osf.io/download/rtw5k/") %>%
    filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) %>%
    slice_head(n = 100, by = antigen_iso) # Reduce dataset for the purposes of this example

  noise <-
    load_noise_params("https://osf.io/download//hqy4v/") %>%
    filter(Country == "Nepal")

  est.incidence.by(
    pop_data = xs_data,
    curve_params = curve,
    noise_params = noise,
    strata = "catchment",
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL
  ) |>
    expect_warning(regexp = "The number of observations in `data` varies")
})
