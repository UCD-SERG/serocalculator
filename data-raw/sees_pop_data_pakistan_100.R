sees_data <- readr::read_rds("vignettes/precomputed/osf/n6cp3.rds")

sees_pop_data_pk_100_source <-
  sees_data |>
  dplyr::filter(Country == "Pakistan") |>
  dplyr::slice_head(n = 100L, by = antigen_iso) |>
  dplyr::mutate(antigen_iso = as.character(antigen_iso))

sees_pop_data_pk_100_old_names <-
  sees_pop_data_pk_100_source |>
  dplyr::mutate(
    antigen_iso = factor(
      antigen_iso,
      levels = levels(sees_data$antigen_iso)
    )
  ) |>
  as_pop_data(
    age = "Age",
    value = "result",
    id = "index_id",
    standardize = FALSE
  )

sees_pop_data_pk_100 <- as_pop_data(
  data = sees_pop_data_pk_100_old_names,
  age = "Age",
  value = "result",
  id = "index_id",
  standardize = TRUE
)

usethis::use_data(sees_pop_data_pk_100, overwrite = TRUE)

sees_pop_data_100 <-
  sees_data |>
  dplyr::slice_head(
    n = 100L,
    by = dplyr::all_of(c("antigen_iso", "catchment", "Country"))
  ) |>
  as_pop_data(
    age = "Age",
    value = "result",
    id = "index_id",
    standardize = TRUE
  )

usethis::use_data(sees_pop_data_100, overwrite = TRUE)

usethis::use_data(sees_pop_data_pk_100_old_names, overwrite = TRUE)

example_pop_data <-
  sees_pop_data_pk_100_source |>
  dplyr::mutate(dplyr::across(where(is.factor), as.character)) |>
  as.data.frame()

readr::write_csv(
  x = example_pop_data,
  file = here::here("inst/extdata/example_pop_data.csv")
)
readr::write_rds(
  x = example_pop_data,
  file = here::here("inst/extdata/example_pop_data.rds"),
  compress = "xz"
)
