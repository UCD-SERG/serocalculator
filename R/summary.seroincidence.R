#' @title
#' Summary Method for Seroincidence Object
#'
#' @description
#' Calculate seroincidence from output of the seroincidence calculator
#' [estimateSeroincidence()].
#'
#' @param object A dataframe containing output of function [estimateSeroincidence()].
#' @param ... Additional arguments affecting the summary produced.
#' @param quantiles A vector of length 2 specifying quantiles for lower (first element) and upper
#'   (second element) bounds of `lambda`. Default = `c(0.025, 0.975)`.
#' @param showDeviance Logical flag (`FALSE`/`TRUE`) for reporting deviance
#'   (-2*log(likelihood) at estimated seroincidence. Default = `TRUE`.
#' @param showConvergence Logical flag (`FALSE`/`TRUE`) for reporting convergence (see
#'   help for [optim()] for details). Default = `TRUE`.
#' @param confidence_level desired confidence interval coverage probability
#' @return
#' A list with the following items:
#' \describe{
#' \item{`Results`}{Dataframe with maximum likelihood estimate of `lambda` (the
#'   seroincidence) (column `Lambda`) and corresponding lower (`Lambda.lwr`) and upper
#'   (`Lambda.upr` bounds.\cr
#'   Optionally `Deviance` (Negative log likelihood (NLL) at estimated (maximum likelihood)
#'   `lambda`) and `Covergence` (Convergence indicator returned by [optim()].
#'   Value of 0 indicates convergence) columns are included.}
#' \item{`Antibodies`}{Character vector with names of input antibodies used in
#'   [estimateSeroincidence()].}
#' \item{`Strata`}{Character with names of strata used in [estimateSeroincidence()].}
#' \item{`CensorLimits`}{List of cutoffs for each of the antibodies used in
#'   [estimateSeroincidence()].}
#' }
#'
#' @examples
#'
#' \dontrun{
#' # estimate seroincidence
#' seroincidence <- estimateSeroincidence(...)
#'
#' # calculate summary statistics for the seroincidence object
#' seroincidenceSummary <- summary(seroincidence)
#' }
#'
#' @export
summary.seroincidenceList = summary.seroincidence <- function(
    object,
    ...,
    confidence_level = .95,
    showDeviance = TRUE,
    showConvergence = TRUE)
{

  alpha = 1 - confidence_level
  quantiles = c(alpha/2, 1 - alpha/2)

  # R CMD check warnings workaround
  hessian <- NULL
  value <- NULL
  convergence <- NULL

  if (length(quantiles) != 2 || any(quantiles < 0) || any(quantiles > 1)) {
    stop("Incorrectly specified quantiles")
  }

  if (quantiles[1] > quantiles[2]) {
    stop("Quantile for upper bound of incidence estimate cannot be less than the lower bound.")
  }

  results <-
    object |>
    lapply(
      FUN = postprocess_fit,
      coverage = confidence_level) |>
    bind_rows(.id = "Stratum")

  if (!showDeviance) {
    results$log.lik <- NULL
  }

  if (!showConvergence) {
    results$code <- NULL
  }

  output <- structure(
    results,
    Antibodies = attr(object, "Antibodies"),
    Strata = attr(object, "Strata"),
    Quantiles = quantiles,
    class = c("summary.seroincidence", "list"))

  return(output)
}
