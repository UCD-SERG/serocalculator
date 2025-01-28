test_that("`set_biomarker_var()` halts on misspecified column", {
  xs_data <- sees_pop_data_pk_100 %>%
    set_biomarker_var("biomarker") %>%
    expect_error(class = "missing variable")
})
