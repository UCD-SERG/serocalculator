example_noise_params_pk <-
  load_noise_params("https://osf.io/download//hqy4v/") %>%
  filter(Country == "Pakistan")

usethis::use_data(example_noise_params_pk, overwrite = TRUE)


example_noise_params_np <-
  load_noise_params("https://osf.io/download//hqy4v/") %>%
  filter(Country == "Nepal")

usethis::use_data(example_noise_params_np, overwrite = TRUE)
