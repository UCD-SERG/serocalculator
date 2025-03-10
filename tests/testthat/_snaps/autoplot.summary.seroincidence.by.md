# Invalid plot `type` specified. Please choose either 'scatter' or 'bar'.

    Code
      library(dplyr)
      library(ggplot2)
      xs_data <- sees_pop_data_pk_100
      curve <- filter(typhoid_curves_nostrat_100, antigen_iso %in% c("HlyE_IgA",
        "HlyE_IgG"))
      noise <- example_noise_params_pk
      est2 <- estimate_scr_by(strata = c("catchment", "ageCat"), pop_data = xs_data,
      curve_params = curve, noise_params = noise, curve_strata_varnames = NULL,
      noise_strata_varnames = NULL, antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      num_cores = 2)
      est2sum <- summary(est2)
      plot1 <- autoplot(est2sum, xvar = "ageCat", type = "whisker", dodge_width = 0.1,
        color_var = "catchment", CI = TRUE)
    Condition
      Error in `autoplot()`:
      ! Invalid plot `type` specified: "whisker".
      i Please choose either 'scatter' or 'bar'.

# The variable `{yvar}` specified by argument `yvar` does not exist in `object`. Please choose a column that exists in `object`.

    Code
      library(dplyr)
      library(ggplot2)
      xs_data <- sees_pop_data_pk_100
      curve <- filter(typhoid_curves_nostrat_100, antigen_iso %in% c("HlyE_IgA",
        "HlyE_IgG"))
      noise <- example_noise_params_pk
      est2 <- estimate_scr_by(strata = c("catchment", "ageCat"), pop_data = xs_data,
      curve_params = curve, noise_params = noise, curve_strata_varnames = NULL,
      noise_strata_varnames = NULL, antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      num_cores = 2)
      est2sum <- summary(est2)
      plot1 <- autoplot(est2sum, yvar = "fake", type = "bar", dodge_width = 0.1,
        color_var = "catchment", CI = TRUE)
    Condition
      Error in `strat_ests_barplot()`:
      ! The variable `fake` specified by argument `yvar` does not exist in `object`.
      Please choose a column that exists in `object`.

