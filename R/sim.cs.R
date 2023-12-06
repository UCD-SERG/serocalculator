#' Simulate a cross-sectional serosurvey with noise
#'
#' @description
#' Makes a cross-sectional data set (age, y(t) set)
#'  and adds noise, if desired.

#' @param lambda a [numeric()] scalar indicating the incidence rate (in events per person-years)
#' @param n.smpl number of samples to simulate
#' @param age.rng age range of sampled individuals, in years
#' @param age.fx specify the curve parameters to use by age (does nothing at present?)
#' @param antigen_isos Character vector with one or more antibody names. Values must match `curve_params`.
#' @param n.mc how many MCMC samples to use:
#' * when `n.mc` is in `1:4000` a fixed posterior sample is used
#' * when `n.mc` = `0`, a random sample is chosen
#' @param renew.params whether to generate a new parameter set for each infection
#' * `renew.params= TRUE` generates a new parameter set for each infection
#' * `renew.params = FALSE` keeps the one selected at birth, but updates baseline y0
#' @param add.noise a [logical()] indicating whether to add biological and measurement noise
#' @inheritParams llik

#' @param noise_limits biologic noise distribution parameters
#' @param ... additional arguments passed to `simcs.tinf()`
#' @return a [tibble::tibble()] containing simulated cross-sectional serosurvey data, with columns:
#' * `age`: age (in days)
#' * one column for each element in the `antigen_iso` input argument
#' @export

sim.cs <- function(
    lambda = 0.1,
    n.smpl = 100,
    age.rng = c(0, 20),
    age.fx = NA,
    antigen_isos,
    n.mc = 0,
    renew.params = FALSE,
    add.noise = FALSE,
    curve_params,
    noise_limits,

    ...)
{


# @param predpar an [array()] containing MCMC samples from the Bayesian distribution of longitudinal decay curve model parameters. NOTE: most users should leave `predpar` at its default value and provide `curve_params` instead.

  predpar =
    curve_params %>%
    filter(.data$antigen_iso %in% antigen_isos) %>%
    droplevels() %>%
    prep_curve_params_for_array() %>%
    df.to.array(dim_var_names = c("antigen_iso", "parameter"))

  stopifnot(length(lambda) == 1)

  day2yr = 365.25
  lambda = lambda / day2yr
  age.rng = age.rng * day2yr
  npar = dimnames(predpar)$parameter |> length()


  ablist = 1:length(antigen_isos)

  baseline_limits <- noise_limits

  ysim <- simcs.tinf(
    lambda = lambda,
    n.smpl = n.smpl,
    age.rng = age.rng,
    age.fx = age.fx,
    ablist = ablist,
    n.mc = n.mc,
    renew.params = renew.params,
    predpar = predpar,
    blims = baseline_limits,
    npar = npar,
    ...
  )

  if (add.noise) {
    for (k.ab in 1:(ncol(ysim) - 1)) {
      ysim[, 1 + k.ab] <-
        ysim[, 1 + k.ab] +
        runif(n = nrow(ysim),
              min = noise_limits[k.ab, 1],
              max = noise_limits[k.ab, 2])

    }
  }
  colnames(ysim) <- c("age", antigen_isos)

  to_return =
    ysim |>
    as_tibble() %>%
    mutate(age = round(.data$age / day2yr, 2))

  return(to_return)

}
