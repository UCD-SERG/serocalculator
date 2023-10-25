.optNll <- function(stratumData, antibodies, params, censorLimits, ivc = FALSE, m, par0, start)
{
  # Any column but "Stratum" incidence can not be calculated if there are zero observations.
  if (nrow(stratumData) == 0) {
    return(NULL)
  }

  # First, check if we find numeric results...
  res <- .nll(stratumData, antibodies, params, censorLimits, ivc, m, par0, start)
  if (is.na(res)) {
    return(NULL)
  }

  # Estimate log.lambda
  fit <- stats::optim(
    par = start,
    fn = .nll,
    stratumData = stratumData,
    antibodies = antibodies, params = params,
    censorLimits = censorLimits,
    ivc = ivc,
    m = 0,
    par0 = par0,
    method = "L-BFGS-B",
    lower = -13, upper = 0, hessian = TRUE,
    control = list(fnscale = 1))
  return(fit)
}
