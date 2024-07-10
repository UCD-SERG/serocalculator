test_that("stratify_data() produces consistent results", {
  library(dplyr)
  xs_data <- load_pop_data("https://osf.io/download//n6cp3/")
  curve <- load_curve_params("https://osf.io/download/rtw5k/") %>%
    filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) %>%
    slice(1:100, .by = antigen_iso) # Reduce dataset for the purposes of this example
  noise <- load_noise_params("https://osf.io/download//hqy4v/")
  stratified_data =
    stratify_data(
      data = xs_data,
      curve_params = curve,
      noise_params = noise,
      strata_varnames = "catchment",
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL
    )
  expect_snapshot(stratified_data)
})
