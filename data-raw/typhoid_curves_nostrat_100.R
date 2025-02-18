typhoid_curves_nostrat_100 <-
  load_curve_params("https://osf.io/download/rtw5k/") %>%
  dplyr::filter(iter %in% 1:100)

usethis::use_data(typhoid_curves_nostrat_100, overwrite = TRUE)
