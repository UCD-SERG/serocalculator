#' Load noise parameters
#'
#' @param file_path path to an RDS file containing biologic
#' and measurement noise of antibody decay curve parameters
#' `y.low`, `eps`, `nu`, and `y.high`,
#' stored as a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector
#' of antigen isotypes to be used in analyses
#'
#' @returns a `noise` object (a [tibble::tbl_df]
#' with extra attribute `antigen_isos`)
#' @export
#' @examples
#' noise <- load_noise_params(serocalculator_example("example_noise_params.rds"))
#' print(noise)
#'
load_noise_params <- function(file_path, antigen_isos = NULL) {
  is_url <- file_path %>% substr(1, 4) == "http"
  
  if (is_url) {
    file_path <- url(file_path)
  }

  noise <- tryCatch(
    {
      withCallingHandlers(
        {
          file_path %>%
            readRDS() %>%
            as_noise_params()
        },
        warning = function(w) {
          if (is_url) {
            # Suppress warnings for URLs - we'll handle errors instead
            invokeRestart("muffleWarning")
          }
        }
      )
    },
    error = function(e) {
      if (is_url) {
        cli::cli_abort(
          class = "internet_resource_unavailable",
          message = c(
            "Unable to load noise parameters from internet resource.",
            "x" = "The resource at {.url {summary(file_path)$description}} is not available or has changed.",
            "i" = "Please check your internet connection and verify the URL is correct.",
            "i" = "Original error: {e$message}"
          )
        )
      } else {
        # Re-throw the original error for non-URL paths
        stop(e)
      }
    }
  )

  return(noise)
}
