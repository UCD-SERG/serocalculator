test_that("`log_likelihood()` gives consistent results", {
  library(dplyr)
  library(tibble)

  # load in longitudinal parameters
  dmcmc <- load_curve_params("https://osf.io/download/rtw5k")

  xs_data <- "https://osf.io/download//n6cp3/" |>
    load_pop_data()

  # Load noise params
  cond <- tibble(
    antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
    nu = c(0.5, 0.5),
    # Biologic noise (nu)
    eps = c(0, 0),
    # M noise (eps)
    y.low = c(1, 1),
    # low cutoff (llod)
    y.high = c(5e6, 5e6)
  ) # high cutoff (y.high)

  # Calculate log-likelihood
  ll_AG <- log_likelihood( # nolint: object_name_linter
    pop_data = xs_data,
    curve_params = dmcmc,
    noise_params = cond,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    lambda = 0.1
  )

  expect_snapshot_value(ll_AG)

})
