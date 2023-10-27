#' Find the maximum likelihood estimate of the incidence rate parameter
#'
#' @param lambda.start starting guess for incidence; only needed if `loglambda.start` is not used
#' @param loglambda.start starting guess for log(incidence); only needed if `lambda.start` is not used
#' @param log.lmin used to determine default value of `stepmax`
#' @param log.lmax used to determine default value of `stepmax`
#' @inheritParams .nll
#' @inheritParams stats::nlm
#' @inheritDotParams .nll
#' @inheritDotParams stats::nlm

#' @return
.optNll <- function(
    data,
    antibodies = antibodies,
    lnparams = lnparams,
    noise_params = noise_params,
    lambda.start = exp(loglambda.start),
    loglambda.start = log(lambda.start),
    log.lmin = loglambda.start - log(10),
    log.lmax = loglambda.start + log(10), # seroincidence rate interval
    stepmax = (log.lmax - log.lmin) / 4,
    hessian = TRUE,
    ...)
{
  # Any column but "Stratum" incidence can not be calculated if there are zero observations.
  if (nrow(stratumData) == 0) {
    return(NULL)
  }

  # First, check if we find numeric results...
  res <- .nll(
    data = data,
    `log(lambda)` = start,
    antibodies = antibodies,
    lnparams = lnparams,
    noise_params = noise_params,
    ...)

  if (is.na(res)) {
    return(NULL)
  }

  # Estimate log.lambda
  fit = nlm(
    f = .nll,
    p = start,
    data = data,
    antibodies = antibodies,
    lnparams = lnparams,
    noise_params = noise_params,
    hessian = hessian,
    iterlim = iterlim,
    stepmax = stepmax,
    ...)

  return(fit)
}
