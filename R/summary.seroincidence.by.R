#' @title
#' Summary Method for `"seroincidence.by"` Objects
#'
#' @description
#' Calculate seroincidence from output of the seroincidence calculator
#' [est_seroincidence_by()].
#'
#' @param object A dataframe containing output of [est_seroincidence_by()].
#' @param verbose a [logical]
#' scalar indicating whether to print verbose messages to the console
#' @param ... Additional arguments affecting the summary produced.
#' @param show_deviance Logical flag (`FALSE`/`TRUE`) for reporting deviance
#'   (-2*log(likelihood) at estimated seroincidence.
#'   Default = `TRUE`.
#' @param show_convergence
#' Logical flag (`FALSE`/`TRUE`) for reporting convergence
#' (see help for [optim()] for details).
#' Default = `FALSE`.
#' @param confidence_level desired confidence interval coverage probability
#' @return
#' A `summary.seroincidence.by` object, which is a [tibble::tibble],
#' with the following columns:
#'  * `incidence.rate` maximum likelihood estimate of `lambda` (seroincidence)
#'  *  `CI.lwr` lower confidence bound for lambda
#'  * `CI.upr` upper confidence bound for lambda
#'  * `Deviance` (included if `show_deviance = TRUE`)
#'     Negative log likelihood (NLL) at estimated (maximum likelihood) `lambda`)
#'  * `nlm.convergence.code` (included if `show_convergence = TRUE`)
#'    Convergence information returned by [stats::nlm()]
#'
#' The object also has the following metadata
#' (accessible through [base::attr()]):
#' * `antigen_isos`
#'   Character vector with names of input antigen isotypes
#'   used in [est_seroincidence_by()]
#' * `Strata` Character with names of strata used in [est_seroincidence_by()]
#'
#'
#' @examples
#' library(dplyr)
#'
#' xs_data <-
#'   sees_pop_data_pk_100
#'
#' curve <-
#'   typhoid_curves_nostrat_100 |>
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))
#'
#' noise <-
#'   example_noise_params_pk
#'
#' # estimate seroincidence
#' est2 <- est_seroincidence_by(
#'   strata = c("catchment"),
#'   pop_data = xs_data,
#'   sr_params = curve,
#'   noise_params = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   # num_cores = 8 # Allow for parallel processing to decrease run time
#' )
#'
#' # calculate summary statistics for the seroincidence object
#' summary(est2)
#'
#' @export
summary.seroincidence.by <- function(
    object,
    confidence_level = .95,
    show_deviance = TRUE,
    show_convergence = TRUE,
    verbose = FALSE,
    ...) {
  alpha <- 1 - confidence_level
  quantiles <- c(alpha / 2, 1 - alpha / 2)

  if (length(quantiles) != 2 || any(quantiles < 0) || any(quantiles > 1)) {
    stop("Incorrectly specified quantiles")
  }

  if (quantiles[1] > quantiles[2]) {
    cli::cli_abort("Quantile for upper bound of incidence estimate cannot be
                   less than the lower bound.")
  }

  results <-
    object |>
    lapply(
      FUN = summary.seroincidence,
      coverage = confidence_level,
      verbose = verbose
    ) |>
    bind_rows(.id = "Stratum")

  results <-
    inner_join(
      object |> attr("Strata"),
      results,
      by = "Stratum",
      relationship = "one-to-one"
    ) |>
    relocate("Stratum", .before = everything())


  if (!show_deviance) {
    results$log.lik <- NULL
  }

  if (show_convergence) {
    results <- results |>
      relocate("nlm.convergence.code", .after = everything())
  } else {
    results$nlm.convergence.code <- NULL
  }



  output <-
    results |>
    structure(
      antigen_isos = attr(object, "antigen_isos"),
      Strata = attr(object, "Strata") |> attr("strata_vars"),
      Quantiles = quantiles,
      class = "summary.seroincidence.by" |>
        union(class(results))
    )

  return(output)
}
