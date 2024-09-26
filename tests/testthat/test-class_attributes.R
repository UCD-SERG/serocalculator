test_that("`get_biomarker_levels()` works", {
  xs_data <- "https://osf.io/download//n6cp3/" %>%
    load_pop_data()
  biomarker_levels = xs_data %>% get_biomarker_levels()
  expected_levels = structure(1:2,
                              levels = c("HlyE_IgA", "HlyE_IgG"),
                              class = "factor")
  expect_equal(object = biomarker_levels, expected = expected_levels)
})


test_that("`set_biomarker_var()` halts on misspecified column", {
  xs_data <- "https://osf.io/download//n6cp3/" %>%
    readr::read_rds() %>%
    set_biomarker_var("biomarker") %>%
    expect_error(class = "missing variable")
})

test_that("`get_id()` works", {
  xs_data <- "https://osf.io/download//n6cp3/" %>%
    load_pop_data()

  xs_data %>% get_id() %>%
    sort() %>%
    expect_snapshot_value(style = "deparse")

})

test_that("`get_biomarker_names_var() works", {
  biomarker_names_var <-
    "https://osf.io/download//n6cp3/" %>%
    load_pop_data() %>%
    get_biomarker_names_var()

  expect_equal(object = biomarker_names_var, expected = "antigen_iso")
})


test_that("`set_age()` detects partial matches", {
  "https://osf.io/download//n6cp3/" %>%
    load_pop_data(age = 'age$') %>%
    expect_warning(class = "missing variable")
})
