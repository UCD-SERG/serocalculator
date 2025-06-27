#' Analyze simulation results
#'
#' @param data a tibble with
#' @param true_lambda data-generating incidence rate
#'
#' @returns a [list]
#' @export
#'
#' @examples
analyze_sims <- function(data, true_lambda = data |> attr("true_lambda")) {
  # Filter out rows where CI.lwr or CI.upr is Inf or NaN
  data <- data %>%
    filter(is.finite(CI.lwr) & is.finite(CI.upr))

  # Compute Bias (Mean - True Value, assuming true value is the mean of incidence rates)
  bias <- mean(data$incidence.rate - true_lambda, na.rm = TRUE)

  # Standard Error (Mean of reported standard errors)
  standard_error <- mean(data$SE, na.rm = TRUE)

  # RMSE (Root Mean Square Error)
  rmse <- sqrt(mean((data$incidence.rate - true_lambda)^2, na.rm = TRUE))

  # Confidence Interval Width (Mean of Upper - Lower bounds, now without Inf values)
  ci_width <- mean(data$CI.upr - data$CI.lwr, na.rm = TRUE)

  # CI Coverage Calculation
  coverage_count <- data %>%
    filter(CI.lwr <= true_lambda & CI.upr >= true_lambda) %>%
    nrow()

  # Compute Coverage and its Confidence Interval
  compute_coverage_ci <- function(coverage_count, total_count) {
    test_result <- binom.test(coverage_count, total_count, conf.level = 0.95) # 95% CI
    coverage_proportion <- coverage_count / total_count
    ci_lower <- test_result$conf.int[1]
    ci_upper <- test_result$conf.int[2]

    return(list(coverage = coverage_proportion, ci_lower = ci_lower, ci_upper = ci_upper))
  }

  coverage_result <- compute_coverage_ci(coverage_count, nrow(data))

  to_return <- list(
    true_lambda = true_lambda,
    Sample_Size = data |> attr("sample_size"),
    Bias = bias,
    Standard_Error = standard_error,
    RMSE = rmse,
    CI_Width = ci_width,
    CI_Coverage = coverage_result$coverage
  )
  # Return computed statistics as a list
  return(to_return)
}
