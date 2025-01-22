test_that("`set_biomarker_var()` halts on misspecified column", {
  xs_data <- load_pop_data(serocalculator_example("example_pop_data.rds")) %>%
    set_biomarker_var("biomarker") %>%
    expect_error(class = "missing variable")
})
