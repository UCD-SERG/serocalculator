
library(dplyr)

# get pop data
xs_data <- load_pop_data(
  file_path = "https://osf.io/download//n6cp3/",
  age = "Age",
  value = "result",
  id = "index_id",
  standardize = TRUE
) %>%
  filter(Country == "Pakistan") %>%
  slice_head(n = 100)

# get noise data
noise <- load_noise_params("https://osf.io/download//hqy4v/") %>%
  filter(Country == "Pakistan")

# get curve data
curve <- load_curve_params("https://osf.io/download/rtw5k/")

test_that("est.incidence.by() aborts when strata is missing in `pop_data`", {

  expect_error(
    object = est.incidence.by(
      strata = c("catch"),
      pop_data = xs_data %>% filter(Country == "Pakistan"),
      curve_params = curve,
      noise_params = noise %>% filter(Country == "Pakistan"),
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      # num_cores = 8 # Allow for parallel processing to decrease run time
      iterlim = 5 # limit iterations for the purpose of this example
    ),
    class = "missing_var"
  )
})

test_that("est.incidence.by() aborts when multiple elements that don't exactly match the columns of `pop_data` are provided", {

  expect_error(
    object = est.incidence.by(
      strata = c("catch","Count"),
      pop_data = xs_data %>% filter(Country == "Pakistan"),
      curve_params = curve,
      noise_params = noise %>% filter(Country == "Pakistan"),
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      # num_cores = 8 # Allow for parallel processing to decrease run time
      iterlim = 5 # limit iterations for the purpose of this example
    ),
    class = "missing_var"
  )

})

