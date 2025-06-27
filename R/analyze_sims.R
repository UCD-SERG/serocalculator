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
    split(
      f = ~  sample_size + lambda.sim) |>
    lapply(FUN = analyze_sims_one_stratum) |>
    bind_rows()

  class(to_return) <- union("sim_results", class(to_return))

  return(to_return)
}

analyze_sims_one_stratum <- function(
    data,
    true_lambda = data$lambda.sim,
    sample_size = data$sample_size) {

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



  to_return <- tibble(
    lambda.sim = mean(true_lambda),
    sample_size = mean(sample_size),
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
