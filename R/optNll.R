#' Find the maximum likelihood estimate of the incidence rate parameter
#'
#' @param data Data frame with cross-sectional serology data per antibody and age
#' @param lambda.start starting guess for incidence; only needed if `loglambda.start` is not used
#' @param loglambda.start starting guess for log(incidence); only needed if `lambda.start` is not used. A starting value for `log.lambda`. Value of -6 corresponds roughly to 1 day (`log(1/365.25)`), -4 corresponds roughly to 1 week (`log(7 / 365.25)`). Default = -6.
#' @param log.lmin used to determine default value of `stepmax`
#' @param log.lmax used to determine default value of `stepmax`
#' @param antigen_isos Character vector with one or more antibody names. Values must match `data`
#' @param dataList Optional argument; as an alternative to passing in `data`, `lnparams`, and `noise_params` individually, you may create a list containing these three elements (with these names) and pass that in instead. This option may be useful for parallel processing across strata.
#' @inheritParams .nll
#' @inheritParams stats::nlm
#' @inheritDotParams .nll
#' @inheritDotParams stats::nlm

#' @returns a [stats::nlm()] fit object

.optNll <- function(
    data = dataList$data,
    lnparams = dataList$lnparams,
    noise_params = dataList$noise_params,
    dataList = NULL,
    antigen_isos = data |> pull(antigen_iso) |> unique(),
    lambda.start = exp(-6),
    loglambda.start = log(lambda.start),
    log.lmin = loglambda.start - log(10),
    log.lmax = loglambda.start + log(10), # seroincidence rate interval
    stepmax = (log.lmax - log.lmin) / 4,
    hessian = TRUE,

    ...)
{
  # incidence can not be calculated if there are zero observations.
  if (nrow(data) == 0) {
    return(NULL)
  }

  # First, check if we find numeric results...
  res <- .nll(
    data = data,
    log.lambda = loglambda.start,
    antibodies = antigen_isos,
    lnparams = lnparams,
    noise_params = noise_params,
    ...)

  if (is.na(res)) {
    warning("Could not calculate the log-likelihood with starting parameter value.")
    return(NULL)
  }

  # Estimate log.lambda
  fit = nlm(
    f = .nll,
    p = loglambda.start,
    data = data,
    antibodies = antigen_isos,
    lnparams = lnparams,
    noise_params = noise_params,
    hessian = hessian,
    stepmax = stepmax,
    ...)

  fit = fit |>
    structure(
      class = "seroincidence.est" |> union(class(fit)),
      lambda.start = lambda.start)

  return(fit)
}
