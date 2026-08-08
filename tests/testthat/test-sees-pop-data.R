test_that("SEES examples link biomarkers by deidentified subject", {
  id_var <- ids_varname(sees_pop_data_100)
  biomarker_var <- get_biomarker_names_var(sees_pop_data_100)
  observations <-
    sees_pop_data_100 |>
    dplyr::distinct(.data[[id_var]], .data[[biomarker_var]]) |>
    dplyr::count(.data[[id_var]], name = "n_biomarkers")

  expect_true(any(observations$n_biomarkers == 2L))
  expect_true(all(observations$n_biomarkers <= 2L))
  expect_match(sees_pop_data_100[[id_var]], "^[BNP][1-9][0-9]*$")
})

test_that("Pakistan SEES examples contain complete biomarker pairs", {
  examples <- list(sees_pop_data_pk_100, sees_pop_data_pk_100_old_names)

  for (example in examples) {
    id_var <- ids_varname(example)
    biomarker_var <- get_biomarker_names_var(example)
    observations <-
      example |>
      dplyr::distinct(.data[[id_var]], .data[[biomarker_var]]) |>
      dplyr::count(.data[[id_var]], name = "n_biomarkers")

    expect_true(all(observations$n_biomarkers == 2L))
    expect_match(example[[id_var]], "^P[1-9][0-9]*$")
  }
})
