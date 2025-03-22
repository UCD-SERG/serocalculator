# Filter population data for Pakistan
xs_data <- load_pop_data(
  file_path = "https://osf.io/download//n6cp3/",
  age = "Age",
  value = "result",
  id = "index_id",
  standardize = TRUE
) %>%
  filter(Country == "Pakistan")

# get noise data
noise <- load_noise_params("https://osf.io/download//hqy4v/") %>%
  filter(Country == "Pakistan")

# get curve data
curve <- load_sr_params("https://osf.io/download/rtw5k/")

# Initial estimates for lambda
start <- .05

# Estimate incidence
fit <- estimate_scr(
  pop_data = xs_data,
  sr_params = curve,
  noise_param = noise,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA")
)

typhoid_results <- fit %>%
  summary.seroincidence(
    coverage = .95,
    start = start
  ) %>%
  mutate(
    ageCat = NULL,
    antigen.iso = paste(collapse = "+", "HlyE_IgG")
  ) %>%
  structure(noise.parameters = noise)

saveRDS(object = typhoid_results,file = "tests/testthat/fixtures/typhoid_results.rds")
