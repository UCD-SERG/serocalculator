#' @title
#' Summary Method for `"seroincidence.by"` Objects
#'
#' @description
#' Calculate seroincidence from output of the seroincidence calculator
#' [est.incidence.by()].
#'
#' @param object A dataframe containing output of function [est.incidence.by()].
#' @param ... Additional arguments affecting the summary produced.
#' @param showDeviance Logical flag (`FALSE`/`TRUE`) for reporting deviance
#'   (-2*log(likelihood) at estimated seroincidence. Default = `TRUE`.
#' @param showConvergence Logical flag (`FALSE`/`TRUE`) for reporting convergence (see
#'   help for [optim()] for details). Default = `FALSE`.
#' @param confidence_level desired confidence interval coverage probability
#' @return
#' A `summary.seroincidence.by` object, which is a [tibble::tibble], with the following columns:
#'  * `incidence.rate` maximum likelihood estimate of `lambda` (seroincidence)
#'  *  `CI.lwr` lower confidence bound for lambda
#'  * `CI.upr` upper confidence bound for lambda
#'  * `Deviance` (included if `showDeviance = TRUE`) Negative log likelihood (NLL) at estimated (maximum likelihood)
#'    `lambda`)
#'    * `nlm.convergence.code` (included if `showConvergence = TRUE`) Convergence information returned by [stats::nlm()]
#' The object also has the following metadata (accessible through [base::attr()]):
#' * `antigen_isos` Character vector with names of input antigen isotypes used in [est.incidence.by()]
#' * `Strata` Character with names of strata used in [est.incidence.by()]
#'
#'
#' @examples
#'
#' \dontrun{
#' # estimate seroincidence
#' seroincidence <- est.incidence.by(...)
#'
#' # calculate summary statistics for the seroincidence object
#' seroincidenceSummary <- summary(seroincidence)
#' }
#'
#' @export
summary.seroincidence.by <- function(
    object,
    confidence_level = .95,
    showDeviance = TRUE,
    showConvergence = TRUE,
    ...)
{

  alpha = 1 - confidence_level
  quantiles = c(alpha/2, 1 - alpha/2)

  if (length(quantiles) != 2 || any(quantiles < 0) || any(quantiles > 1)) {
    stop("Incorrectly specified quantiles")
  }

  if (quantiles[1] > quantiles[2]) {
    stop("Quantile for upper bound of incidence estimate cannot be less than the lower bound.")
  }

  results =
    object %>%
    lapply(
      FUN = summary.seroincidence,
      coverage = confidence_level) %>%
    bind_rows(.id = "Stratum")

  results =
    inner_join(
      object %>% attr("Strata"),
      results,
      by = "Stratum",
      relationship = "one-to-one"
    ) %>%
    relocate("Stratum", .before = everything())


  if (!showDeviance) {
    results$log.lik <- NULL
  }

  if (showConvergence) {
    results = results %>%
      relocate("nlm.convergence.code", .after = everything())
  } else
  {
    results$nlm.convergence.code <- NULL
  }



  output <-
    results %>%
    structure(
      antigen_isos = attr(object, "antigen_isos"),
      Strata = attr(object, "Strata") %>% attr("strata_vars"),
      Quantiles = quantiles,
      class =
        "summary.seroincidence.by" %>%
        union(class(results))
    )

  return(output)
}
