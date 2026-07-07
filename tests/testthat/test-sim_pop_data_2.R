test_that(
  desc = "results are consistent without units",
  code = {

    # Load curve parameters
    dmcmc <- typhoid_curves_nostrat_100

    days_per_year <- 365.25
    withr::with_seed(
      seed = 1,
      code = {

        # Generate cross-sectional data
        csdata <- sim_pop_data_2(
          curve_params = dmcmc,
          lambda = 0.2 / days_per_year,
          n_samples = 100,
          age_range = c(0, 10) * days_per_year,
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

    csdata |> expect_snapshot_data(name = "sim_cs_data")
    csdata |> expect_snapshot_value(style = "serialize")

  }
)

test_that(
  desc = "results are consistent with units",
  code = {

    # Load curve parameters
    dmcmc <- typhoid_curves_nostrat_100 |>
      dplyr::mutate(
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

    csdata2 |> expect_snapshot_data(name = "sim_cs_data-units")
    csdata2 |> expect_snapshot_value(style = "serialize")
  }
)

test_that("verbose > 1 prints inputs without erroring", {
  dmcmc <- typhoid_curves_nostrat_100

  withr::with_seed(
    seed = 1,
    code = {
      expect_message(
        sim_pop_data_2(
          curve_params = dmcmc,
          lambda = 0.2,
          n_samples = 5,
          age_range = c(0, 10),
          antigen_isos = c("HlyE_IgA", "HlyE_IgG"),
          n_mcmc_samples = 0,
          renew_params = TRUE,
          add_noise = TRUE,
          noise_limits = rbind(
            "HlyE_IgA" = c(min = 0, max = 0.5),
            "HlyE_IgG" = c(min = 0, max = 0.5)
          ),
          format = "long",
          verbose = 2
        )
      )
    }
  )
})

test_that("add_noise = FALSE skips the noise step", {
  dmcmc <- typhoid_curves_nostrat_100

  withr::with_seed(
    seed = 1,
    code = {
      csdata_no_noise <- sim_pop_data_2(
        curve_params = dmcmc,
        lambda = 0.2,
        n_samples = 5,
        age_range = c(0, 10),
        antigen_isos = c("HlyE_IgA", "HlyE_IgG"),
        n_mcmc_samples = 0,
        renew_params = TRUE,
        add_noise = FALSE,
        format = "long"
      )
    }
  )

  expect_s3_class(csdata_no_noise, "pop_data")
  expect_equal(nrow(csdata_no_noise), 10)
})

test_that("curve_params with a chain column joins correctly", {
  # regression test for a `by =` argument that silently dropped the
  # "mcmc_chain" = "chain" join key when built with
  # `if (chain_in_curve_params) "mcmc_chain" = "chain"` (R parses this as
  # an assignment, not a named c() element, whenever it's nested inside an
  # `if`) -- caused `left_join()` to error with a missing "chain" column
  # whenever `curve_params` had a chain column.
  dmcmc <- typhoid_curves_nostrat_100 |>
    dplyr::mutate(chain = rep_len(1:2, dplyr::n()))

  withr::with_seed(
    seed = 1,
    code = {
      csdata_chain <- sim_pop_data_2(
        curve_params = dmcmc,
        lambda = 0.2,
        n_samples = 5,
        age_range = c(0, 10),
        antigen_isos = c("HlyE_IgA", "HlyE_IgG"),
        n_mcmc_samples = 0,
        renew_params = TRUE,
        add_noise = FALSE,
        format = "long"
      )
    }
  )

  expect_s3_class(csdata_chain, "pop_data")
  expect_equal(nrow(csdata_chain), 10)
})

test_that("format = 'wide' is not yet implemented", {
  dmcmc <- typhoid_curves_nostrat_100

  withr::with_seed(
    seed = 1,
    code = {
      expect_error(
        sim_pop_data_2(
          curve_params = dmcmc,
          lambda = 0.2,
          n_samples = 5,
          age_range = c(0, 10),
          antigen_isos = c("HlyE_IgA", "HlyE_IgG"),
          n_mcmc_samples = 0,
          renew_params = TRUE,
          add_noise = FALSE,
          format = "wide"
        ),
        "not yet implemented"
      )
    }
  )
})
