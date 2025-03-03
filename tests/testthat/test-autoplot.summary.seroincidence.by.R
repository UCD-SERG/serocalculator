test_that("results are consistent", {

   library(dplyr)
   library(ggplot2)

   xs_data <-
     sees_pop_data_pk_100

   curve <-
     typhoid_curves_nostrat_100 |>
     filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

   noise <-
     example_noise_params_pk

   est2 <- estimate_scr_by(
     strata = c("catchment", "ageCat"),
     pop_data = xs_data,
     curve_params = curve,
     noise_params = noise,
     curve_strata_varnames = NULL,
     noise_strata_varnames = NULL,
     antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
     num_cores = 2 # Allow for parallel processing to decrease run time
   )

   est2sum <- summary(est2)

   plot1 <- autoplot(est2sum,
                     xvar = "ageCat",
                     dodge_width = 0.1,
                     color_var = "catchment",
                     CI = TRUE)

   plot1 |> vdiffr::expect_doppelganger(title = "strat-est-plot-CI")

   plot2 <- autoplot(est2sum, "ageCat",
                     CI = TRUE,
                     dodge_width = 0.1,
                     group_var = "catchment",
                     color_var = "catchment")

   plot2 |> vdiffr::expect_doppelganger(title = "strat-est-plot-CI-lines")

})
