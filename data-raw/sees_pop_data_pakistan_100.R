sees_pop_data_pk_100 <- load_pop_data(
  file_path = "https://osf.io/download//n6cp3/",
  age = "Age",
  value = "result",
  id = "index_id",
  standardize = TRUE
) %>%
  filter(Country == "Pakistan") %>%
  slice_head(n = 100, by = antigen_iso)

usethis::use_data(sees_pop_data_pk_100, overwrite = TRUE)

sees_pop_data_100 <- load_pop_data(
  file_path = "https://osf.io/download//n6cp3/",
  age = "Age",
  value = "result",
  id = "index_id",
  standardize = TRUE
) %>%
  filter(Country == "Nepal") %>%
  slice_head(n = 100,
             by = all_of(c("antigen_iso", "catchment", "Country")))

usethis::use_data(sees_pop_data_100, overwrite = TRUE)

sees_pop_data_pk_100_old_names <- load_pop_data(
  file_path = "https://osf.io/download//n6cp3/",
  age = "Age",
  value = "result",
  id = "index_id",
  standardize = FALSE
) %>%
  filter(Country == "Pakistan") %>%
  slice_head(n = 100, by = antigen_iso)

usethis::use_data(sees_pop_data_pk_100_old_names, overwrite = TRUE)

readr::write_csv(
  x = sees_pop_data_pk_100_old_names,
  file = here::here("inst/extdata/example_pop_data.csv")
)
