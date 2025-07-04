test_that("`summary.seroincidence.by()` produces consistent results",
          {
            typhoid_results <-
              est_seroincidence_by(
                strata = "catchment",
                pop_data = sees_pop_data_pk_100,
                sr_param = typhoid_curves_nostrat_100,
                curve_strata_varnames = NULL,
                noise_strata_varnames = NULL,
                noise_param = example_noise_params_pk,
                antigen_isos = c("HlyE_IgG", "HlyE_IgA")
              ) |>
              summary()

            expect_snapshot_value(typhoid_results,
                                  style = "serialize",
                                  tolerance = 1e-4)
          })
