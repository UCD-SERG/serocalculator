test_that(
  desc = "`est.incidence.by()` produces consistent results",

  code =     {

    library(dplyr)

    xs_data <-
      load_pop_data("https://osf.io/download//n6cp3/") %>%
      filter(Country == "Pakistan") %>%
      slice_head(n = 100, by = c("catchment", "antigen_iso"))

    curve <- load_curve_params("https://osf.io/download/rtw5k/") %>%
      filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) %>%
      slice_head(n = 100, by = antigen_iso) # Reduce dataset for the purposes of this example

    noise <- load_noise_params("https://osf.io/download//hqy4v/")

    testobj = est.incidence.by(
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

    expect_snapshot(x = testobj %>% summary())

    expect_snapshot_value(
      x = testobj,
      tolerance = 10^-3,
      style = "serialize"
    )
  }
)
