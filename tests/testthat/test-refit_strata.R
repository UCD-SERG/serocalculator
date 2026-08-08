refit_small <- function(strata_ids, ...) {
  refit_strata(
    pop_data = serocalculator::sees_pop_data_pk_100,
    strata_ids = strata_ids,
    sr_params = serocalculator::typhoid_curves_nostrat_100,
    noise_params = serocalculator::example_noise_params_pk,
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    num_cores = 1,
    verbose = FALSE,
    iterlim = 5, # keep the example fast; convergence is not the point here
    ...
  )
}

test_that("`refit_strata()` returns NULL when no strata are selected", {
  empty <- tibble::tibble(catchment = character(0))

  expect_null(refit_small(empty))
})

test_that("`refit_strata()` refits only the selected strata", {
  one <- tibble::tibble(catchment = "kgh")

  result <- refit_small(one)

  expect_s3_class(result, "seroincidence.by")
  expect_equal(nrow(summary(result)), 1)
  expect_equal(summary(result)$catchment, "kgh")
})

test_that("`refit_strata()` attaches log-likelihood graphs", {
  # The whole point of the function: the cached fit is built without graphs,
  # so the re-fit must supply them.
  result <- refit_small(tibble::tibble(catchment = "kgh"))

  expect_true(attr(result, "graphs_included"))
})

test_that("`refit_strata()` selects strata by value, not by position", {
  # `count_strata()` names strata positionally, so a subset renumbers them.
  # Selecting the second catchment must return that catchment's data, not
  # whatever happens to land in position 2.
  result <- refit_small(tibble::tibble(catchment = "aku"))

  expect_equal(summary(result)$catchment, "aku")
})

test_that("`refit_strata()` defaults `strata` to the id columns", {
  both <- tibble::tibble(catchment = c("kgh", "aku"))

  result <- refit_small(both)

  expect_equal(nrow(summary(result)), 2)
  expect_setequal(summary(result)$catchment, c("kgh", "aku"))
})
