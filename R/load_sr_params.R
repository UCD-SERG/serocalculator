#' Load longitudinal seroresponse parameter samples
#'
#' @param file_path path to an RDS file containing MCMC samples of antibody
#' seroresponse parameters `y0`, `y1`, `t1`, `alpha`, and `r`,
#' stored as a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector of antigen isotypes used in analyses
#'
#' @returns a `curve_params` object (a [tibble::tbl_df]
#' with extra attribute `antigen_isos`)
#' @export
#' @examples
#' curve <- load_sr_params(serocalculator_example("example_curve_params.rds"))
#'
#' print(curve)
#'
load_sr_params <- function(file_path, antigen_isos = NULL) {
  is_url <- file_path |> substr(1, 4) == "http"

  if (is_url) {
    file_path <- url(file_path)
    data <- read_sr_params_url(file_path)
  } else {
    data <- readr::read_rds(file_path)
  }

  # Validation errors (e.g., wrong data format) are never network failures,
  # so they are raised as-is, for URLs and local paths alike.
  # Warnings from validation are preserved.
  curve_params <- as_sr_params(data, antigen_isos = antigen_isos)

  return(curve_params)
}

#' Read an RDS file from a URL
#'
#' Only failures to *read* the resource are reported as
#' `internet_resource_unavailable`;
#' the caller validates the contents separately.
#'
#' @param con a [url()] connection
#'
#' @returns the deserialized R object
#' @keywords internal
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

#' @title Load antibody decay curve parameter samples
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `load_curve_params()` was renamed to [load_sr_params()] to create a more
#' consistent API.
#' @keywords internal
#' @export
load_curve_params <- function(...) {
  lifecycle::deprecate_soft("1.3.1", "load_curve_params()", "load_sr_params()")
  load_sr_params(
    ...
  )
}
