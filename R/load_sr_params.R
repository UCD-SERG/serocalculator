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
  }

  curve_params <- tryCatch(
    {
      # Read the RDS file with warning suppression for URLs
      data <- if (is_url) {
        withCallingHandlers(
          readRDS(file_path),
          warning = function(w) {
            # Suppress warnings for URLs - we'll handle errors instead
            invokeRestart("muffleWarning")
          }
        )
      } else {
        readRDS(file_path)
      }
      
      # Convert to sr_params (warnings from validation will be preserved)
      as_sr_params(data, antigen_isos = antigen_isos)
    },
    error = function(e) {
      if (is_url) {
        cli::cli_abort(
          class = "internet_resource_unavailable",
          message = c(
            "Unable to load seroresponse parameters from internet resource.",
            "x" = paste(
              "The resource at {.url {summary(file_path)$description}}",
              "is not available or has changed."
            ),
            "i" = paste(
              "Please check your internet connection",
              "and verify the URL is correct."
            ),
            "i" = "Original error: {e$message}"
          )
        )
      } else {
        # Re-throw the original error for non-URL paths unchanged
        rlang::cnd_signal(e)
      }
    }
  )

  return(curve_params)
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
load_curve_params <- function(
    ...) {
  lifecycle::deprecate_soft("1.3.1", "load_curve_params()", "load_sr_params()")
  load_sr_params(
    ...
  )
}
