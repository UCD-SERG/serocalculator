test_that(
  desc = "results are consistent without units",
  code = {

    # Load curve parameters
    dmcmc <- typhoid_curves_nostrat_100

    # d2y <- 365.242198781
    d2y <- 365.25
    withr::with_seed(
      seed = 1,
      code = {

        # Generate cross-sectional data
        csdata <- sim_pop_data_2(
          curve_params = dmcmc,
          lambda = 0.2 / d2y,
          n_samples = 100,
          age_range = c(0, 10) * d2y,
          antigen_isos = c("HlyE_IgA", "HlyE_IgG"),
          n_mcmc_samples = 0,
          renew_params = TRUE,
          add_noise = TRUE,
          noise_limits = rbind(
            "HlyE_IgA" = c(min = 0, max = 0.5),
            "HlyE_IgG" = c(min = 0, max = 0.5)
          ),
          format = "long"
        )


      }
    )

    csdata |> ssdtools:::expect_snapshot_data(name = "sim_cs_data")
    csdata |> expect_snapshot_value(style = "serialize")

  }
)

test_that(
  desc = "results are consistent with units",
  code = {

    library(dplyr)
    # Load curve parameters
    dmcmc <- typhoid_curves_nostrat_100 |>
      mutate(
        alpha = alpha |> units::as_units("1/days"),
        t1 = t1 |> units::as_units("days")
      )

    withr::with_seed(
      seed = 1,
      code = {

        # Generate cross-sectional data
        csdata2 <- sim_pop_data_2(
          curve_params = dmcmc,
          lambda = 0.2 |> units::as_units("1/years"),
          n_samples = 100,
          age_range = c(0, 10) |> units::as_units("years"),
          antigen_isos = c("HlyE_IgA", "HlyE_IgG"),
          n_mcmc_samples = 0,
          renew_params = TRUE,
          add_noise = TRUE,
          noise_limits = rbind(
            "HlyE_IgA" = c(min = 0, max = 0.5),
            "HlyE_IgG" = c(min = 0, max = 0.5)
          ),
          format = "long"
        )
      }
    )

    csdata2 |> ssdtools:::expect_snapshot_data(name = "sim_cs_data-units")
    csdata2 |> expect_snapshot_value(style = "serialize")


  }
)

