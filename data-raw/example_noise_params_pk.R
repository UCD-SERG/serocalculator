example_noise_params_pk <-
  load_noise_params("https://osf.io/download//hqy4v/") %>%
  filter(Country == "Pakistan")

usethis::use_data(example_noise_params_pk, overwrite = TRUE)


example_noise_params_sees <-
  load_noise_params("https://osf.io/download//hqy4v/")

usethis::use_data(example_noise_params_sees, overwrite = TRUE)
