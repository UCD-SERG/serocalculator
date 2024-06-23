nlm_exit_codes <- c(
  "1" = "1: relative gradient is close to zero, current iterate is probably solution.",
  "2" = "2: successive iterates within tolerance, current iterate is probably solution.",
  "3" = "3: Last global step failed to locate a point lower than x. Either x is an approximate local minimum of the function, the function is too non-linear for this algorithm, or `stepmin` in `est.incidence()` (a.k.a. `steptol` in `nlm()`) is too large.",
  "4" = "4: iteration limit exceeded; increase `iterlim`.",
  "5" = "5: maximum step size `stepmax` exceeded five consecutive times. Either the function is unbounded below, becomes asymptotic to a finite value from above in some direction or `stepmax` is too small."
)

xs_data <- load_pop_data(
  file_path = "https://osf.io/download//n6cp3/",
  age = "Age",
  value = "result",
  id = "index_id",
  standardize = TRUE
)

summary_country <- xs_data %>%
  summary.pop_data(strata = "Country")

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
curve <- load_curve_params("https://osf.io/download/rtw5k/")

# Initial estimates for lambda
start <- .05

# Estimate incidence
fit <- est.incidence(
  pop_data = xs_data,
  curve_param = curve,
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


usethis::use_data(typhoid_results, summary_country, nlm_exit_codes, overwrite = TRUE, internal = TRUE)
