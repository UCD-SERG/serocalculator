summary_country <- xs_data %>%
  summary(strata = "Country") %>%
  magrittr::extract2("age_summary")

usethis::use_data(summary_country, overwrite = TRUE, internal = TRUE)
