
library(dplyr)

xs_data <-
  load_pop_data("https://osf.io/download//n6cp3/") %>%
  filter(Country == "Pakistan")

curve <- load_curve_params("https://osf.io/download/rtw5k/") %>%
  filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) %>%
  slice(1:100, .by = antigen_iso) # Reduce dataset for the purposes of this example

noise <- load_noise_params("https://osf.io/download//hqy4v/")

test_that(
  "`est.incidence.by()` warns users about missing antigen:isotype combinations",
  {

    xs_data2 = xs_data %>%
      filter(
        antigen_iso == "HlyE_IgG" |
          catchment == "aku"
      )

    xs_data2 %>% select(antigen_iso, catchment) %>% table()

    expect_message(
      class = "missing_biomarker",
      {
        ests1 = est.incidence.by(
          strata = "catchment",
          pop_data = xs_data2,
          curve_params = curve,
          curve_strata_varnames = NULL,
          noise_params = noise %>% filter(Country == "Pakistan"),
          noise_strata_varnames = NULL,
          antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
          num_cores = 1
        )
      }
    )
  }
)

test_that(
  desc = "`est.incidence.by()` produces consistent results",

  code =     {
    expect_snapshot_value(
      tolerance = 10^-3,
      style = "serialize",
      est.incidence.by(
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
    )
  }
)
