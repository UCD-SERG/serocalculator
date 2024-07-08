
xs_data <- load_pop_data(
  file_path = "https://osf.io/download//n6cp3/",
  age = "Age",
  value = "result",
  id = "index_id",
  standardize = TRUE
)

summary_country <- xs_data %>%
  summary(strata = "Country")

saveRDS(object = summary_country,file = "tests/testthat/fixtures/summary_country.rds")
