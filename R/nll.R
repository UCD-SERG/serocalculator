
#' Calculate negative log-likelihood
#' @description
#' Same as [llik()], except negated and requiring lambda on log scale (used in combination with `nlm()`, to ensure that the optimization search doesn't stray into negative values of `lambda`).
#' @param log.lambda natural logarithm of incidence rate
#' @inheritDotParams llik -lambda

#' @return the negative log-likelihood of the data with the current parameter values
#' @keywords internal
.nll = function(log.lambda, ...)
{
  -llik(lambda = exp(log.lambda),...)
}
