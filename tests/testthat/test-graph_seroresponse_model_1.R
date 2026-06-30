test_that("graph_seroresponse_model_1() returns a patchwork object", {
  result <- typhoid_curves_nostrat_100 |>
    dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) |>
    graph_seroresponse_model_1()

  expect_true(inherits(result, "patchwork"))
})

test_that("graph_seroresponse_model_1() `antigen_isos` subsets correctly", {
  result <- typhoid_curves_nostrat_100 |>
    graph_seroresponse_model_1(antigen_isos = c("HlyE_IgA", "HlyE_IgG"))

  expect_true(inherits(result, "patchwork"))
  expect_length(result$patches$plots, 1L)
})

test_that("graph_seroresponse_model_1() works for a single antigen isotype", {
  result <- typhoid_curves_nostrat_100 |>
    dplyr::filter(antigen_iso == "HlyE_IgA") |>
    graph_seroresponse_model_1()

  expect_true(inherits(result, "patchwork"))
})

test_that("graph_seroresponse_model_1() produces consistent output", {
  withr::local_seed(42)

  result <- typhoid_curves_nostrat_100 |>
    dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) |>
    graph_seroresponse_model_1()

  result |>
    vdiffr::expect_doppelganger(title = "graph-seroresponse-model-1")
})

test_that("graph_seroresponse_model_1() respects custom `ncol`", {
  withr::local_seed(42)

  result <- typhoid_curves_nostrat_100 |>
    dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG", "LPS_IgA")) |>
    graph_seroresponse_model_1(ncol = 1)

  result |>
    vdiffr::expect_doppelganger(title = "graph-seroresponse-model-1-ncol1")
})
