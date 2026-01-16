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

            # Check that noise_params attribute exists and is a list
            expect_true(!is.null(attr(summ2, "noise_params")))
            noise_params_list <- attr(summ2, "noise_params")
            expect_type(noise_params_list, "list")
            expect_equal(length(noise_params_list), 2)  # Two strata

            # Check each stratum's noise_params
            for (i in seq_along(noise_params_list)) {
              noise_params <- noise_params_list[[i]]
              expect_s3_class(noise_params, "tbl_df")
              expect_true(all(c("antigen_iso", "eps", "nu") %in%
                                names(noise_params)))
              expect_equal(nrow(noise_params), 2)
            }

            # Check that n_sr_params attribute exists and is a named vector
            expect_true(!is.null(attr(summ2, "n_sr_params")))
            n_sr_params <- attr(summ2, "n_sr_params")
            expect_type(n_sr_params, "integer")
            expect_equal(length(n_sr_params), 2)
            expect_equal(as.vector(n_sr_params), c(200, 200))

            # Check that n_pop_data attribute exists and is a named vector
            expect_true(!is.null(attr(summ2, "n_pop_data")))
            n_pop_data <- attr(summ2, "n_pop_data")
            expect_type(n_pop_data, "integer")
            expect_equal(length(n_pop_data), 2)

            # Check that sr_params_stratified attribute exists
            expect_true(!is.null(attr(summ2, "sr_params_stratified")))
            sr_params_stratified <- attr(summ2, "sr_params_stratified")
            expect_type(sr_params_stratified, "logical")
            expect_equal(length(sr_params_stratified), 2)
            # Should be FALSE for both strata
            expect_true(all(!sr_params_stratified))
          })
