.check_suggests <- function(github) {
  if (!requireNamespace("cranlogs", quietly = TRUE)) {
    cli::cli_abort(
      paste(
        "Package {.pkg cranlogs} is required.",
        "Install with",
        "{.code install.packages('cranlogs')}."
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
