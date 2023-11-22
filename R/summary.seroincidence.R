#' @title
#' Summary Method for `"seroincidence.ests"` Objects
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
#' @param plot_likelihoods whether to plot the log-likelihood curves (they can also be extracted from the fits)
#' @return
#' A list with the following items:
#'
#' * `Results` a [dplyr::tibble()] with columns:
#'    * `incidence.rate` maximum likelihood estimate of `lambda` (seroincidence)
#'    *  `CI.lwr` lower confidence bound for lambda
#'    * `CI.upr` upper confidence bound for lambda
#'    * `Deviance` (included if `showDeviance = TRUE`) Negative log likelihood (NLL) at estimated (maximum likelihood)
#'    `lambda`)
#'    * `nlm.exit.code` (included if `showConvergence = TRUE`) Convergence information returned by [stats::nlm()]
#' * `Antibodies` Character vector with names of input antibodies used in [est.incidence.by()]
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
summary.seroincidence.ests <- function(
    object,
    confidence_level = .95,
    showDeviance = TRUE,
    showConvergence = FALSE,
    plot_likelihoods = FALSE,
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
    object |>
    lapply(
      FUN = postprocess_fit,
      coverage = confidence_level) |>
    bind_rows(.id = "Stratum")

  results =
    inner_join(
      object |> attr("Strata"),
      results,
      by = "Stratum",
      relationship = "one-to-one"
    ) |>
    relocate("Stratum", .before = everything())


  if (!showDeviance) {
    results$log.lik <- NULL
  }

  if (showConvergence) {
    results = results |>
      relocate("nlm.exit.code", .after = everything())
  } else
  {
    results$nlm.exit.code <- NULL
  }

  if(plot_likelihoods)
  {
    if(!attr(object,"graphs_included"))
    {

      warning(
        "Graphs cannot be extracted from the `seroincidence.ests` object.",
        "`build_graph` was not `TRUE` in the call to `est.incidence.by()`")
      figure = NULL
    } else
    {
      requireNamespace("ggpubr", quietly = FALSE)
      labels = sapply(object, FUN = attr, which = "stratum_string")
      figs = lapply(object, FUN = attr, which = "ll_graph")
      figure <- ggpubr::ggarrange(
        figs,
        labels = c("A", "B", "C"),
        ncol = length(figs) |> sqrt() |> ceiling(),
        nrow = length(figs) |> sqrt() |> ceiling())
    }
  } else
  {
    figure = NULL
  }

  output <-
    results |>
    structure(
      Antibodies = attr(object, "Antibodies"),
      Strata = attr(object, "Strata") |> names(),
      Quantiles = quantiles,
      figure = figure,
      class =
        "summary.seroincidence.ests" |>
        union(class(results))
    )

  #todo: write plot.summary.seroincidence.ests

  return(output)
}
