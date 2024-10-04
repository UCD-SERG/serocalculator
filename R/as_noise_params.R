#' Load noise parameters
#'
#' @param data a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector of antigen isotypes
#' to be used in analyses
#'
#' @returns a `noise_params` object (a [tibble::tbl_df] with
#' extra attribute `antigen_isos`)
#' @export
#' @examples
#' library(magrittr)
#' noise_data <-
#'   "https://osf.io/download//hqy4v/" %>%
#'   readr::read_rds() %>%
#'   as_noise_params()
#'
#' print(noise_data)
#'
as_noise_params <- function(data, antigen_isos = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort(
      class = "not data.frame",
      message = c(
        "Can't convert {.arg data} to {.cls noise_params}.",
        paste("x" = "{.arg data} must be a {.cls data.frame}",
          "(or a subclass of {.cls data.frame})."
        ),
        "i" = "You have supplied a {.cls {class(data)}}."
      )
    )
  }

  noise_data <- data %>% tibble::as_tibble()

  # Define noise columns
  noise_cols <- c("antigen_iso", "y.low", "eps", "nu", "y.high")

  # Get any missing columns
  missing_cols <- setdiff(x = noise_cols, y = names(data))

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      class = "not noise_params",
      message = c(
        "Can't convert {.arg data} to {.cls noise_params}.",
        "i" = paste("The column{?s} : {.strong {.var {missing_cols}}}",
          "{?is/are} missing."
        )
      )
    )
  }

  # Assign noise_params class
  class(noise_data) <- c("noise_params", class(noise_data))

  # Handle antigen_isos
  if (is.null(antigen_isos)) {
    antigen_isos <- unique(noise_data$antigen_iso)
  } else {
    missing_antigen <- setdiff(antigen_isos, unique(noise_data$antigen_iso))

    if (length(missing_antigen) > 0) {
      cli::cli_abort(
        class = "missing-antigen",
        message = c(
          "x" = "Can't convert {.var data} to {.cls noise_params}.",
          "i" = paste("The antigen type{?s} {.str {missing_antigen}}",
                      "{?is/are} missing in {.var data}.")
        )
      )
    }
  }

  # Assign antigen attribute
  attr(noise_data, "antigen_isos") <- antigen_isos

  return(noise_data)
}
