check_parallel_cores <- function(num_cores) {
  requireNamespace("parallel", quietly = FALSE)

  if (num_cores > (parallel::detectCores() - 1)) {
    num_cores <-
      num_cores |>
      min(parallel::detectCores() - 1)

    cli::cli_inform(
      class = "reduced num_cores",
      c(
        "Reducing `num_cores` to {num_cores}
        to avoid destabilizing this computer
        ({parallel::detectCores()} cores detected)."
      )
    )
  }

  return(num_cores)

}
