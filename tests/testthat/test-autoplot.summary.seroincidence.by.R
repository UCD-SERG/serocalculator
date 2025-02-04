test_that("results are consistent", {

   library(dplyr)
   library(ggplot2)

   xs_data <-
     sees_pop_data_pk_100

   curve <-
     typhoid_curves_nostrat_100 %>%
     filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

   noise <-
     example_noise_params_pk

   est2 <- est.incidence.by(
     strata = c("catchment"),
     pop_data = xs_data,
     curve_params = curve,
     noise_params = noise,
     curve_strata_varnames= NULL,
     noise_strata_varnames = NULL,
     antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
     num_cores = 8 # Allow for parallel processing to decrease run time
   )

   est2sum <- summary(est2)

   plot1 <- autoplot(est2sum, "catchment")

   plot1 |> vdiffr::expect_doppelganger(title = "strat-est-plot")

})
