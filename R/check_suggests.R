#' Check that suggested packages are available
#'
#' @param github Logical; whether GitHub downloads are requested,
#'   which requires the `gh` package.
#'
#' @returns `NULL`, invisibly. Called for its side effect of
#'   aborting if required packages are missing.
#'
#' @noRd
.check_suggests <- function(github) {
  if (!requireNamespace("packageRank", quietly = TRUE)) {
    cli::cli_abort(
      paste(
        "Package {.pkg packageRank} is required.",
        "Install with",
        "{.code install.packages('packageRank')}."
      )
    )
  }
  if (github && !requireNamespace("gh", quietly = TRUE)) {
    cli::cli_abort(
      paste(
        "Package {.pkg gh} is required.",
        "Install with",
        "{.code install.packages('gh')}."
      )
    )
  }
}
