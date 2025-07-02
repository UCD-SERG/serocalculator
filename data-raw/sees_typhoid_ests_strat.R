library(dplyr)

xs_data <- readr::read_rds("https://osf.io/download//n6cp3/") |>
  as_pop_data()

curves <-
  "https://osf.io/download/rtw5k/" |>
  load_sr_params()

noise <- "https://osf.io/download//hqy4v/" |> readr::read_rds()

sees_typhoid_ests_strat <- est_seroincidence_by(
  strata = c("ageCat", "Country"),
  pop_data = xs_data,
  sr_params = curve,
  curve_strata_varnames = NULL,
  noise_params = noise,
  noise_strata_varnames = "Country",
  antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
  verbose = TRUE,
  num_cores = 8 # Allow for parallel processing to decrease run time
)

usethis::use_data(sees_typhoid_ests_strat, overwrite = TRUE)
