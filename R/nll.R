#' Calculate negative log-likelihood
#' @description
#' Same as [log_likelihood()], except negated and requiring lambda on log scale (used in combination with `nlm()`, to ensure that the optimization search doesn't stray into negative values of `lambda`).
#' @param log.lambda natural logarithm of incidence rate
#' @inheritDotParams log_likelihood -lambda

#' @return the negative log-likelihood of the data with the current parameter values
#' @keywords internal
.nll = function(log.lambda, ...)
{
  -log_likelihood(lambda = exp(log.lambda),...)

}
