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

test_that(
          "summary.seroincidence.by includes noise_params and
          sr_params metadata",
          {
            est2 <- est_seroincidence_by(
              strata = "catchment",
              pop_data = sees_pop_data_pk_100,
              sr_param = typhoid_curves_nostrat_100 |>
                dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")),
              curve_strata_varnames = NULL,
              noise_strata_varnames = NULL,
              noise_param = example_noise_params_pk,
              antigen_isos = c("HlyE_IgG", "HlyE_IgA")
            )

            summ2 <- summary(est2)

            # Check that noise parameter columns exist for each row
            # with antigen names
            expect_true("measurement.noise.HlyE_IgA" %in% names(summ2))
            expect_true("measurement.noise.HlyE_IgG" %in% names(summ2))
            expect_true("biological.noise.HlyE_IgA" %in% names(summ2))
            expect_true("biological.noise.HlyE_IgG" %in% names(summ2))

            # Check that metadata columns exist
            expect_true("n.seroresponse.params" %in% names(summ2))
            expect_true("seroresponse.params.stratified" %in% names(summ2))

            # Check that object name columns exist with new names
            expect_true("pop_data" %in% names(summ2))
            expect_true("sr_params" %in% names(summ2))
            expect_true("noise_params" %in% names(summ2))

            # Check values for both strata
            expect_equal(nrow(summ2), 2)  # Two strata
            expect_equal(summ2$n.seroresponse.params, c(200, 200))
            expect_true(all(!summ2$seroresponse.params.stratified))
          })
