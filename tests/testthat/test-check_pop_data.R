test_that("`check_pop_data()` works", {
  library(dplyr)

  xs_data <- sees_pop_data_pk_100

  xs_data %>% check_pop_data() |>
    expect_no_condition()

  xs_data %>% check_pop_data(verbose = TRUE) |>
    expect_message("data format is as expected.")

  xs_data_no_age = xs_data %>% select(-age)
  xs_data_no_age %>% check_pop_data() |>
    expect_error(class = "missing-var")

  xs_data_no_value = xs_data %>% select(-value)
  xs_data_no_value %>% check_pop_data() |>
    expect_error(class = "missing-var")

  "string" %>% check_pop_data() |>
    expect_error(class = "not a data.frame")
})
