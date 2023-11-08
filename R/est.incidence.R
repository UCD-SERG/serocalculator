
#
#' Age specific seroincidence function
#' add some details here
#'
#' @param dpop cross-sectional population data
#' @param c.age age category
#' @param start starting value for incidence rate
#' @param antigen_isos antigen-isotype(s) (a [character()] vector of one or more antigen names)
#' @param noise_params a [data.frame()] containing columns `nu`, etc. specifying conditional noise parameters
#' @param iterlim a positive integer specifying the maximum number of iterations to be performed before the program is terminated.
#' @param dmcmc mcmc samples from distribution of longitudinal decay curve parameters
#' @inheritParams postprocess_fit
#' @inheritDotParams stats::nlm -f -p -hessian -iterlim -stepmax
#'
#' @return A [data.frame()] containing the following:
#' * `est.start`: the starting guess for incidence rate
#' * `ageCat`: the age category we are analyzing
#' * `incidence.rate`: the estimated incidence rate, per person year
#' * `CI.lwr`: lower limit of confidence interval for incidence rate
#' * `CI.upr`: upper limit of confidence interval for incidence rate
#' * `coverage`: coverage probability
#' * `neg.llik`: negative log-likelihood
#' * `iterations`: the number of iterations used
#'
#' @export

est.incidence <- function(
    dpop,
    dmcmc,
    noise_params,
    c.age = NULL,
    antigen_isos = dpop$antigen_iso |> unique(),
    start = 0.1,
    iterlim = 100,
    coverage = .95,
    ...)
{

  lambda = start # initial estimate: starting value
  log.lambda = log(lambda)
  log.lmin = log(lambda/10)
  log.lmax = log(10*lambda) # seroincidence rate interval

  if(!is.null(c.age))
  {
    dpop = dpop %>% filter(ageCat == c.age)
    dmcmc = dmcmc %>% filter(ageCat == c.age)

    if("ageCat" %in% names(noise_params))
    {
      noise_params = noise_params %>% filter(ageCat == c.age)
    }
  }

  ps = list()
  cs = list()
  conds = list()

  for (cur_antigen in antigen_isos)
  {
    ps[[cur_antigen]] = get_xspd_one_antigen(
      dpop = dpop,
      antigen = cur_antigen)

    cs[[cur_antigen]] = get_curve_params_one_antigen(
      params = dmcmc,
      antigen = cur_antigen)

    conds[[cur_antigen]] =
      noise_params %>%
      filter(antigen_iso == cur_antigen)

  }

  # noise parameters
  # cond.hlye.IgG

  objfunc = build_likelihood_function(
    cross_sectional_data = ps,
    longitudinal_parameter_samples = cs,
    noise_params = conds)

  # seroincidence estimation
  fit = nlm(
    f = objfunc,
    p = log.lambda,
    hessian = TRUE,
    iterlim = iterlim,
    stepmax = (log.lmax - log.lmin) / 4,
    ...)

  if(fit$iterations >= iterlim)
  {
    warning(
      "Maximum `nlm()` iterations reached; consider increasing `iterlim` argument.")
  }

  log.lambda.est =
    fit |>
    postprocess_fit(
      coverage = coverage,
      start = start
  ) |>
    mutate(ageCat = c.age,
           antigen.iso = paste(collapse = "+", antigen_isos)) %>%
    structure(noise.parameters = cond)

  return(log.lambda.est)
}
