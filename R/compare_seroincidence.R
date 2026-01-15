#' Compare seroincidence rates between two groups
#'
#' @description
#' Perform a two-sample z-test to compare seroincidence rates between
#' two groups. Since we use maximum likelihood estimation (MLE) for each
#' seroincidence estimate and estimates from different strata or data sets
#' are uncorrelated, we can use a simple two-sample z-test using the
#' Gaussian distribution. The standard error for the difference is computed
#' by adding the estimated variances and taking the square root.
#'
#' @param x A `"seroincidence"` object from [est_seroincidence()] or
#'   a `"seroincidence.by"` object from [est_seroincidence_by()]
#' @param y A `"seroincidence"` object from [est_seroincidence()]
#'   (optional if `x` is a `"seroincidence.by"` object)
#' @param coverage Desired confidence interval coverage probability
#'   (default = 0.95)
#' @param verbose Logical indicating whether to print verbose messages
#'   (default = FALSE)
#' @param ... Additional arguments (currently unused)
#'
#' @details
#' When comparing two single `"seroincidence"` objects, this function performs a
#' two-sample z-test and returns results in the standard `htest` format.
#'
#' When applied to a `"seroincidence.by"` object (stratified estimates),
#' the function compares all pairs of strata and returns a nicely formatted
#' table with point estimates for the difference in seroincidence, p-values,
#' and confidence intervals.
#'
#' The test statistic is computed as:
#' \deqn{z = \frac{\lambda_1 - \lambda_2}{\sqrt{SE_1^2 + SE_2^2}}}
#'
#' where \eqn{\lambda_1} and \eqn{\lambda_2} are the estimated incidence rates,
#' and \eqn{SE_1} and \eqn{SE_2} are their standard errors.
#'
#' @return
#' * When comparing two `"seroincidence"` objects: An object of class
#'   `"htest"` containing the test statistic, p-value, confidence interval,
#'   and estimates.
#' * When applied to a `"seroincidence.by"` object: A [tibble::tibble()]
#'   with columns for each pair of strata, the difference in incidence rates,
#'   standard error, z-statistic, p-value, and confidence interval bounds.
#'
#' @export
#' @examples
#' \dontrun{
#' # See inst/examples/exm-compare_seroincidence.R for complete examples
#' }
compare_seroincidence <- function(
    x,
    y = NULL,
    coverage = 0.95,
    verbose = FALSE,
    ...) {
  UseMethod("compare_seroincidence")
}

#' @export
compare_seroincidence.default <- function(
    x,
    y = NULL,
    coverage = 0.95,
    verbose = FALSE,
    ...) {
  cli::cli_abort(
    c(
      paste0(
        "{.arg x} must be a {.cls seroincidence} or ",
        "{.cls seroincidence.by} object."
      ),
      "x" = "You supplied an object of class {.cls {class(x)}}."
    )
  )
}

#' @export
#' @describeIn compare_seroincidence Compare two single seroincidence
#'   estimates
compare_seroincidence.seroincidence <- function(
    x,
    y = NULL,
    coverage = 0.95,
    verbose = FALSE,
    ...) {
  if (is.null(y)) {
    cli::cli_abort(
      c(
        paste0(
          "When {.arg x} is a {.cls seroincidence} object, ",
          "{.arg y} must also be provided."
        ),
        "x" = "{.arg y} is {.val NULL}."
      )
    )
  }

  if (!inherits(y, "seroincidence")) {
    cli::cli_abort(
      c(
        "{.arg y} must be a {.cls seroincidence} object.",
        "x" = "You supplied an object of class {.cls {class(y)}}."
      )
    )
  }

  # Get summaries for both estimates
  sum_x <- summary(x, coverage = coverage, verbose = FALSE)
  sum_y <- summary(y, coverage = coverage, verbose = FALSE)

  # Extract estimates and standard errors
  lambda1 <- sum_x$incidence.rate
  lambda2 <- sum_y$incidence.rate
  se1 <- sum_x$SE
  se2 <- sum_y$SE

  # Compute difference and its standard error
  diff <- lambda1 - lambda2
  se_diff <- sqrt(se1^2 + se2^2)

  # Compute z-statistic and p-value
  z_stat <- diff / se_diff
  p_value <- 2 * stats::pnorm(-abs(z_stat))

  # Compute confidence interval for the difference
  alpha <- 1 - coverage
  z_crit <- stats::qnorm(1 - alpha / 2)
  ci_lower <- diff - z_crit * se_diff
  ci_upper <- diff + z_crit * se_diff

  # Create htest object
  result <- list(
    statistic = c(z = z_stat),
    p.value = p_value,
    estimate = c(
      "incidence rate 1" = lambda1,
      "incidence rate 2" = lambda2,
      "difference" = diff
    ),
    conf.int = c(ci_lower, ci_upper),
    null.value = c("difference in incidence rates" = 0),
    alternative = "two.sided",
    method = "Two-sample z-test for difference in seroincidence rates",
    data.name = paste("seroincidence estimates")
  )

  attr(result$conf.int, "conf.level") <- coverage
  class(result) <- "htest"

  return(result)
}

#' @export
#' @describeIn compare_seroincidence Compare all pairs of stratified
#'   seroincidence estimates
compare_seroincidence.seroincidence.by <- function(
    x,
    y = NULL,
    coverage = 0.95,
    verbose = FALSE,
    ...) {
  if (!is.null(y)) {
    cli::cli_warn(
      c(
        paste0(
          "When {.arg x} is a {.cls seroincidence.by} object, ",
          "{.arg y} is ignored."
        ),
        "i" = "Comparisons will be made among all strata in {.arg x}."
      )
    )
  }

  # Get summary of stratified estimates
  sum_x <- summary(x, coverage = coverage, verbose = FALSE)

  # Extract strata information
  strata_vars <- attr(sum_x, "Strata")
  n_strata <- nrow(sum_x)

  if (n_strata < 2) {
    cli::cli_abort(
      c(
        "At least 2 strata are required for comparison.",
        "x" = "Only {n_strata} stratum found in {.arg x}."
      )
    )
  }

  # Create all pairwise comparisons
  comparisons <- list()
  idx <- 1

  # Pre-compute string patterns for column relocation
  strata_patterns1 <- paste0(strata_vars, ".1")
  strata_patterns2 <- paste0(strata_vars, ".2")

  for (i in 1:(n_strata - 1)) {
    for (j in (i + 1):n_strata) {
      # Extract data for this pair
      lambda1 <- sum_x$incidence.rate[i]
      lambda2 <- sum_x$incidence.rate[j]
      se1 <- sum_x$SE[i]
      se2 <- sum_x$SE[j]

      # Compute difference and its standard error
      diff <- lambda1 - lambda2
      se_diff <- sqrt(se1^2 + se2^2)

      # Compute z-statistic and p-value
      z_stat <- diff / se_diff
      p_value <- 2 * stats::pnorm(-abs(z_stat))

      # Compute confidence interval for the difference
      alpha <- 1 - coverage
      z_crit <- stats::qnorm(1 - alpha / 2)
      ci_lower <- diff - z_crit * se_diff
      ci_upper <- diff + z_crit * se_diff

      # Store comparison results
      comparison <- tibble::tibble(
        Stratum_1 = sum_x$Stratum[i],
        Stratum_2 = sum_x$Stratum[j],
        incidence.rate.1 = lambda1,
        incidence.rate.2 = lambda2,
        difference = diff,
        SE = se_diff,
        z.statistic = z_stat,
        p.value = p_value,
        CI.lwr = ci_lower,
        CI.upr = ci_upper
      )

      # Add stratum variables
      for (var in strata_vars) {
        comparison[[paste0(var, ".1")]] <- sum_x[[var]][i]
        comparison[[paste0(var, ".2")]] <- sum_x[[var]][j]
      }

      # Reorder columns to put stratum variables first
      comparison <- comparison |>
        dplyr::relocate(
          tidyselect::starts_with(strata_patterns1),
          tidyselect::starts_with(strata_patterns2),
          .before = "incidence.rate.1"
        )

      comparisons[[idx]] <- comparison
      idx <- idx + 1
    }
  }

  # Combine all comparisons
  result <- dplyr::bind_rows(comparisons)

  # Add metadata
  result <- result |>
    structure(
      coverage = coverage,
      strata_vars = strata_vars,
      antigen_isos = attr(sum_x, "antigen_isos"),
      class = c("comparison.seroincidence.by", class(result))
    )

  return(result)
}
