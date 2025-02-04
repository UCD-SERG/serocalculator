check_parallel_cores <- function(num_cores) {
  requireNamespace("parallel", quietly = FALSE)

  if (num_cores > (parallel::detectCores() - 1)) {
    num_cores <-
      num_cores |>
      min(parallel::detectCores() - 1)

    cli::cli_inform(
      class = "reduced num_cores",
      c(
        "This computer appears to have
        {parallel::detectCores()} cores available.
        `est.incidence.by()` has reduced its
        `num_cores` argument to {num_cores}
        to avoid destabilizing the computer."
      )
    )
  }

  return(num_cores)
}
