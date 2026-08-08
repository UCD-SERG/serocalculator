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
