# `as_curve_params()` produces expected results

    Code
      curve_data <- "https://osf.io/download/rtw5k/" %>% readr::read_rds() %>%
        as_curve_params()

