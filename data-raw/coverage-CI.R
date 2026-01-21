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

# coverage_result <- compute_coverage_ci(coverage_count, nrow(data)) # nolint: object_usage_linter
