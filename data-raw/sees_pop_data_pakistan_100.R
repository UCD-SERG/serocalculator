sees_pop_data_pakistan_100_standardized <- load_pop_data(
  file_path = "https://osf.io/download//n6cp3/",
  age = "Age",
  value = "result",
  id = "index_id",
  standardize = TRUE
) %>%
  filter(Country == "Pakistan") %>%
  slice_head(n = 100, by = antigen_iso)

usethis::use_data(sees_pop_data_pakistan_100_standardized, overwrite = TRUE)
