library(dplyr)
library(ggplot2)

xs_data <-
  sees_pop_data_pk_100

curve <-
  typhoid_curves_nostrat_100 |>
  filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

noise <-
  example_noise_params_pk

est2 <- est_seroincidence_by(
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

est2sum |> autoplot(
  type = "bar",
  yvar = "ageCat",
  color_var = "catchment",
  CIs = TRUE
)
