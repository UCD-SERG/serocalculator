#' Read an RDS file from a URL
#'
#' Only failures to *read* the resource are reported as
#' `internet_resource_unavailable`;
#' the caller validates the contents separately.
#'
#' @param con a [url()] connection
#'
#' @returns the deserialized R object
#' @noRd
read_sr_params_url <- function(con) {
  tryCatch(
    # Suppress warnings for URLs - we'll handle errors instead
    withCallingHandlers(
      readRDS(con),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) {
      cli::cli_abort(
        class = "internet_resource_unavailable",
        message = c(
          "Unable to load seroresponse parameters from internet resource.",
          "x" = paste(
            "The resource at {.url {summary(con)$description}}",
            "is not available or has changed."
          ),
          "i" = paste(
            "Please check your internet connection",
            "and verify the URL is correct."
          ),
          "i" = "Original error: {e$message}"
        )
      )
    }
  )
}
