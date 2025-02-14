test_that(
  desc = "results are consistent without units",
  code = {
    withr::with_seed(
      seed = 1,
      code = {

        # Load curve parameters
        dmcmc <- typhoid_curves_nostrat_100

        # Specify the antibody-isotype responses to include in analyses
        antibodies <- c("HlyE_IgA", "HlyE_IgG")

        # Set seed to reproduce results
        set.seed(54321)

        # Simulated incidence rate per person-year
        lambda <- 0.2
        # Range covered in simulations
        lifespan <- c(0, 10)
        # Cross-sectional sample size
        nrep <- 100

        # Biologic noise distribution
        dlims <- rbind(
          "HlyE_IgA" = c(min = 0, max = 0.5),
          "HlyE_IgG" = c(min = 0, max = 0.5)
        )

        # Generate cross-sectional data
        csdata <- sim_pop_data_2(
          curve_params = dmcmc,
          lambda = lambda,
          n_samples = nrep,
          age_range = lifespan,
          antigen_isos = antibodies,
          n_mcmc_samples = 0,
          renew_params = TRUE,
          add_noise = TRUE,
          noise_limits = dlims,
          format = "long"
        )

        csdata |> ssdtools:::expect_snapshot_data(name = "sim_cs_data")
        csdata |> expect_snapshot_value(style = "serialize")

      }
    )
  })

test_that(
  desc = "results are consistent without units",
  code = {
    withr::with_seed(
      1,
      code = {


        library(dplyr)
        # Load curve parameters
        dmcmc <- typhoid_curves_nostrat_100 |>
          mutate(
            alpha = alpha |> units::as_units("1/days"),
            t1 = t1 |> units::as_units("days")
          )

        # Specify the antibody-isotype responses to include in analyses
        antibodies <- c("HlyE_IgA", "HlyE_IgG")

        # Simulated incidence rate per person-year
        lambda <- 0.2 |> units::as_units("1/years")
        # Range covered in simulations
        lifespan <- c(0, 10) |> units::as_units("years")
        # Cross-sectional sample size
        nrep <- 100

        # Biologic noise distribution
        dlims <- rbind(
          "HlyE_IgA" = c(min = 0, max = 0.5),
          "HlyE_IgG" = c(min = 0, max = 0.5)
        )

        # Generate cross-sectional data
        csdata <- sim_pop_data_2(
          curve_params = dmcmc,
          lambda = lambda,
          n_samples = nrep,
          age_range = lifespan,
          antigen_isos = antibodies,
          n_mcmc_samples = 0,
          renew_params = TRUE,
          add_noise = TRUE,
          noise_limits = dlims,
          format = "long"
        )

        csdata |> ssdtools:::expect_snapshot_data(name = "sim_cs_data-units")
        csdata |> expect_snapshot_value(style = "serialize")

      }
    )

  })
