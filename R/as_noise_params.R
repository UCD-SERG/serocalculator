#' Load noise parameters
#'
#' @param data a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector of antigen isotypes to be used in analyses
#'
#' @returns a `noise_params` object (a [tibble::tbl_df] with extra attribute `antigen_isos`)
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
        "x" = "{.arg data} must be a {.cls data.frame}
        (or a subclass of {.cls data.frame}).",
        "i" = "You have supplied a {.cls {class(data)}}."
      )
    )
  }

  noise_data <-
    data %>%
    tibble::as_tibble()

  # define noise columns
  noise_cols <- c("antigen_iso", "y.low", "eps", "nu", "y.high")

  # get columns from provided data
  data_cols <- data %>% names()

  # get any missing column(s)
  missing_cols <- setdiff(x = noise_cols, y = data_cols)

  if (!all(is.element(noise_cols, noise_data %>% names()))) {
    cli::cli_abort(
      class = "not noise_params",
      message = c(
        "Can't convert {.arg data} to {.cls noise_params}.",
        "i" = "The column{?s} : {.strong {.var {missing_cols}}} {?is/are} missing.",
        "x" = "You have supplied {.cls {class(data)}}."
      )
    )
  }


  # assign curve class
  class(noise_data) <-
    c("noise_params", class(noise_data))

  if (is.null(antigen_isos)) {
    antigen_isos <- unique(noise_data$antigen_iso)
  } else {
    if (!all(
      is.element(antigen_isos, noise_data$antigen_iso)
    )) {

      missing_antigen <- setdiff(antigen_isos,
                                 unique(noise_data$antigen_iso))
      cli::cli_abort(
        class = "missing_antigen",
        message = c(
          "x" = "Can't convert {.var data} to {.cls noise_params}.",
          "i" = "The antigen type{?s} {missing_antigen} {?is/are} missing in {.var data}"
        )
      )
    }
  }

  # assign antigen attribute
  attr(noise_data, "antigen_isos") <- antigen_isos

  return(noise_data)
}
