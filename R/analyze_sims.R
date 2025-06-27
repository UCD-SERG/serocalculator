#' Analyze simulation results
#'
#' @param data a [tibble::tbl_df] with columns:
#' * `lambda.sim`,
#' * `incidence.rate`,
#' * `SE`,
#' * `CI.lwr`,
#' * `CI.upr`
#' for example, as produced by [summary.seroincidence.by()] with
#' `lambda.sim` as a stratifying variable
#'
#' @returns a `sim_results` object (extends [tibble::tbl_df])
#' @export
#'
#' @example inst/examples/exm-analyze_sims.R
#'
analyze_sims <- function(
    data) {
  to_return <-
    data |>
    dplyr::summarize(
      .by = c("lambda.sim", "sample_size"),
      analyze_sims_one_stratum(
        data = across(everything()),
        true_lambda = .data$lambda.sim
      )
    )

  class(to_return) <- union("sim_results", class(to_return))

  return(to_return)
}

analyze_sims_one_stratum <- function(
    data,
    true_lambda = data$lambda.sim) {
  # Filter out rows where CI.lwr or CI.upr is Inf or NaN
  data <- data |>
    filter(is.finite(.data$CI.lwr) & is.finite(.data$CI.upr))

  # Compute Bias
  bias <- mean(data$incidence.rate - true_lambda, na.rm = TRUE)

  # Standard Error (Mean of reported standard errors)
  standard_error <- mean(data$SE, na.rm = TRUE)

  # RMSE (Root Mean Square Error)
  rmse <- mean((data$incidence.rate - true_lambda)^2, na.rm = TRUE) |> sqrt()

  # Confidence Interval Width (Mean of Upper - Lower bounds, without Inf values)
  ci_width <- mean(data$CI.upr - data$CI.lwr, na.rm = TRUE)

  # CI Coverage Calculation
  coverage_count <-
    sum(data$CI.lwr <= true_lambda & data$CI.upr >= true_lambda, na.rm = TRUE)

  coverage_prop <-
    mean(data$CI.lwr <= true_lambda & data$CI.upr >= true_lambda, na.rm = TRUE)

  # Compute Coverage and its Confidence Interval
  compute_coverage_ci <- function(coverage_count, total_count) {
    test_result <- stats::binom.test(
      coverage_count,
      total_count,
      conf.level = 0.95
    )
    # 95% CI
    coverage_proportion <- coverage_count / total_count
    ci_lower <- test_result$conf.int[1]
    ci_upper <- test_result$conf.int[2]

    return(
      list(
        coverage = coverage_proportion,
        ci_lower = ci_lower,
        ci_upper = ci_upper
      )
    )
  }

  coverage_result <- compute_coverage_ci(coverage_count, nrow(data)) # nolint: object_usage_linter

  to_return <- tibble(
    Bias = bias,
    Mean_Est_SE = standard_error,
    Empirical_SE = sd(data$incidence.rate, na.rm = TRUE),
    RMSE = rmse,
    Mean_CI_Width = ci_width,
    CI_Coverage = coverage_prop
  )
  # Return computed statistics as a list
  return(to_return)
}
