test_that("`set_biomarker_var()` halts on misspecified column", {
  xs_data <- "https://osf.io/download//n6cp3/" %>%
    readr::read_rds() %>%
    set_biomarker_var("biomarker") %>%
    expect_error(class = "missing variable")
})
