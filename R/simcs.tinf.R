#' collect cross-sectional data
#'
#' @description output: (age, y(t) set)
#'
#' @param lambda seroconversion rate (in events/person-day)
#' @param n_samples number of samples n_samples (= nr of simulated records)
#' @param age_range age range to use for simulating data, in days
#' @param age_fixed age_fixed for parameter sample (age_fixed = NA for age at infection)
#' @param antigen_isos Character vector with one or more antibody names. Values must match `curve_params`.
#' @param n_mcmc_samples
#' * when `n_mcmc_samples` is in 1:4000 a fixed posterior sample is used
#' * when n_mcmc_samples = 0 a random sample is chosen
#' @param renew_params
#' * `renew_params = TRUE` generates a new parameter set for each infection
#' * `renew_params = FALSE` keeps the one selected at birth, but updates baseline y0
#' @param ... arguments passed to [simresp.tinf()]
#'
#' @return an [array()]
#' @dev
simcs.tinf <- function(
    lambda,
    n_samples,
    age_range,
    age_fixed = NA,
    antigen_isos,
    n_mcmc_samples = 0,
    renew_params = FALSE,
    ...) {
  st.days <- round(age_range[1])
  # from min=age_range[1] days...
  en.days <- round(age_range[2])
  # to   max=age_range[2] days...
  if (st.days == 0) {
    st.days <- 1
  }

  # if(en.days>30000) en.days <- 30000;
  y.smpl <- array(
    NA,
    dim = c(n_samples, length(antigen_isos) + 1),
    dimnames = list(
      obs = 1:n_samples,
      var = c("age", antigen_isos)
    )
  )
  # y and age
  for (k.smpl in 1:n_samples)
  {
    resp <-
      simresp.tinf(
        lambda,
        t.end = en.days,
        age_fixed = age_fixed,
        antigen_isos = antigen_isos,
        n_mcmc_samples = n_mcmc_samples,
        renew_params = renew_params,
        ...
      )

    tinf.smp <-
      sample((st.days:en.days), size = 1)
    # sample at random age
    y.smpl[k.smpl, ] <-
      c(resp$t[tinf.smp], as.matrix(resp$y)[tinf.smp, ])
  }
  return(y.smpl)
}
