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
        Reducing `num_cores` argument to {num_cores}
        to avoid destabilizing the computer."
      )
    )
  }

  return(num_cores)

  if (nzchar(chk)) {
    chk_u <- toupper(chk)

    if (chk_u %in% c("TRUE", "T", "YES", "Y")) {
      # In check environments, be polite: cap at 2
      num_cores <- min(num_cores, 2L)

    } else if (chk_u %in% c("FALSE", "F", "NO", "N")) {
      # No cap requested

    } else {
      # Often this is a numeric string like "2"
      chk_n <- suppressWarnings(as.integer(chk))
      if (!is.na(chk_n) && chk_n >= 1L) {
        num_cores <- min(num_cores, chk_n)
      } else {
        # Unrecognized value: be conservative
        num_cores <- min(num_cores, 2L)
      }
    }
  }

  if (verbose) {
    cli::cli_inform("Set up parallel processing with `num_cores`={num_cores}")
  }
}
