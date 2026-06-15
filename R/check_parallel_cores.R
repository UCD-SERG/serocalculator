#' Check and Adjust Core Count Based on Hardware and Environment Limits
#'
#' @param num_cores Integer. The initial number of requested cores.
#' @param chk Character. The value of an environment variable like
#' `_R_CHECK_LIMIT_CORES_`. Defaults to `""`.
#' @param verbose Logical. Whether to report parallel setup information.
#' Defaults to `FALSE`.
#'
#' @return An integer representing the safe number of cores to use.
#' @keywords internal
check_parallel_cores <- function(num_cores, chk = "", verbose = FALSE) {
  requireNamespace("parallel", quietly = FALSE)

  # 1. Apply CRAN environment variable limits if present
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

  # 2. Apply hardware stability limit (cap at max available cores - 1)
  if (num_cores > (parallel::detectCores() - 1)) {
    num_cores <- min(num_cores, parallel::detectCores() - 1)

    cli::cli_inform(
      class = "reduced num_cores",
      c(
        "This computer appears to have
        {parallel::detectCores()} cores available. ",
        "The number of cores has been reduced to {num_cores}
        to avoid destabilizing the computer."
      )
    )
  }

  # 3. Report setup if verbose
  if (verbose) {
    cli::cli_inform(
      "Setting up parallel processing with `num_cores` = {num_cores}."
    )
  }

  return(as.integer(num_cores))
}
