test_that("warns about the independence assumption when verbose", {
  expect_message(
    .warn_biomarker_independence(
      pop_data = sees_pop_data_pk_100,
      antigen_isos = c("HlyE_IgA", "HlyE_IgG"),
      cluster_var = NULL,
      verbose = TRUE
    ),
    "cluster_var"
  )
})

test_that("stays silent when cluster_var is already supplied", {
  expect_no_message(
    .warn_biomarker_independence(
      pop_data = sees_pop_data_pk_100,
      antigen_isos = c("HlyE_IgA", "HlyE_IgG"),
      cluster_var = "id",
      verbose = TRUE
    )
  )
})

test_that("stays silent for a single biomarker", {
  expect_no_message(
    .warn_biomarker_independence(
      pop_data = sees_pop_data_pk_100,
      antigen_isos = "HlyE_IgA",
      cluster_var = NULL,
      verbose = TRUE
    )
  )
})

test_that("stays silent when not verbose", {
  expect_no_message(
    .warn_biomarker_independence(
      pop_data = sees_pop_data_pk_100,
      antigen_isos = c("HlyE_IgA", "HlyE_IgG"),
      cluster_var = NULL,
      verbose = FALSE
    )
  )
})

test_that("degrades gracefully when pop_data has no id_var attribute", {
  plain_df <- data.frame(antigen_iso = "HlyE_IgA", value = 1)
  expect_message(
    .warn_biomarker_independence(
      pop_data = plain_df,
      antigen_isos = c("HlyE_IgA", "HlyE_IgG"),
      cluster_var = NULL,
      verbose = TRUE
    ),
    "independence"
  )
})

test_that("does not leak ids_varname()'s fallback-to-index_id warning", {
  # `ids_varname()` warns (not errors) and falls back to "index_id"
  # in this case -- a `tryCatch(..., error = ...)` alone wouldn't
  # catch that, so this exercises the middle path `.warn_
  # biomarker_independence()`'s own tests didn't originally cover.
  plain_df_with_index_id <- data.frame(
    index_id = "a",
    antigen_iso = "HlyE_IgA",
    value = 1
  )
  expect_no_warning(
    msgs <- testthat::capture_messages(
      .warn_biomarker_independence(
        pop_data = plain_df_with_index_id,
        antigen_isos = c("HlyE_IgA", "HlyE_IgG"),
        cluster_var = NULL,
        verbose = TRUE
      )
    )
  )
  expect_true(any(grepl("index_id", msgs)))
})
