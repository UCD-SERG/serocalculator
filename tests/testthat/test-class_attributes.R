test_that("`get_biomarker_levels()` works", {
  xs_data <- "https://osf.io/download//n6cp3/" %>%
    load_pop_data()
  biomarker_levels = xs_data %>% get_biomarker_levels()
  expected_levels = structure(1:2,
                              levels = c("HlyE_IgA", "HlyE_IgG"),
                              class = "factor")
  expect_equal(object = biomarker_levels, expected = expected_levels)
})
