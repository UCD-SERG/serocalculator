
#
#' Age specific seroincidence function
#' This function models seroincidence using maximum likelihood estimation; that is, it finds the value of the seroincidence parameter which maximizes the likelihood (i.e., joint probability) of the data.
#'
#' @param data cross-sectional population data
#' @param c.age age category
#' @param start starting value for incidence rate
#' @param antigen_isos antigen isotypes: a [character()] vector of one or more antigen isotype names, which should match the values of the `antigen_iso` column in the `data` input argument
#' @param noise_params a [data.frame()] containing columns `nu`, etc. specifying conditional noise parameters
#' @param curve_params mcmc samples from distribution of longitudinal decay curve parameters
#' @param verbose logical: if TRUE, print verbose log information to console
#' @param iterlim an [integer()], which provides an upper limit on the number of computational iterations used to search for the maximum likelihood estimate of incidence (passed to [stats::nlm()]).
#' @param stepmax a [numeric()], which limits how aggressively the [stats::nlm()] algorithm searches for the maximum likelihood estimate of incidence. If this function output an infinite standard error estimate, consider reducing this parameter.
#' @inheritParams summary.seroincidence.est
#' @inheritParams stats::nlm
#' @inheritDotParams stats::nlm -f -p -hessian
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
    data,
    curve_params,
    noise_params,
    c.age = NULL,
    antigen_isos = data$antigen_iso |> unique(),
    start = 0.1,
    iterlim = 100,
    coverage = .95,
    verbose = FALSE,
    stepmax = 1,
    ...)
{

  lambda = start # initial estimate: starting value
  log.lambda = log(lambda)

  if(!is.null(c.age))
  {
    data = data %>% dplyr::filter(.data[["ageCat"]] == c.age)
    curve_params = curve_params %>% dplyr::filter(.data[["ageCat"]] == c.age)

    if("ageCat" %in% names(noise_params))
    {
      noise_params =
        noise_params %>%
        dplyr::filter(.data[["ageCat"]] == c.age)
    }
  }

  ps = list()
  cs = list()
  conds = list()

  for (cur_antigen in antigen_isos)
  {
    ps[[cur_antigen]] = get_xspd_one_antigen(
      data = data,
      antigen = cur_antigen)

    cs[[cur_antigen]] = get_curve_params_one_antigen(
      params = curve_params,
      antigen = cur_antigen)

    conds[[cur_antigen]] =
      noise_params %>%
      dplyr::filter(.data[["antigen_iso"]] == cur_antigen)

  }

  # noise parameters
  # cond.hlye.IgG

  objfunc = build_likelihood_function(
    cross_sectional_data = ps,
    longitudinal_parameter_samples = cs,
    noise_params = conds)

  # seroincidence estimation
  {
    fit = nlm(
    f = objfunc,
    p = log.lambda,
    hessian = TRUE,
    iterlim = iterlim,
    stepmax = stepmax,
    ...)
  } |> system.time() -> time

  if(verbose)
  {
    message('\nElapsed time: ')
    print(time)
  }

  if(fit$iterations >= iterlim)
  {
    warning(
      "Maximum `nlm()` iterations reached; consider increasing `iterlim` argument.")
  }

  log.lambda.est =
    fit |>
    structure(
      lambda.start = start,
      antigen_isos = antigen_isos
    ) |>
    summary.seroincidence.est(
      coverage = coverage
    ) |>
    mutate(
      ageCat = c.age) %>%
    structure(
      noise.parameters = noise_params)

  return(log.lambda.est)
}
