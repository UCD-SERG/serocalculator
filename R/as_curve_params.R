#' Load antibody decay curve parameters
#'
#' @param data a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector of antigen
#' isotypes to be used in analyses
#' @returns a `curve_data` object (a [tibble::tbl_df]
#' with extra attribute `antigen_isos`)
#' @export
#' @examples
#' library(magrittr)
#' curve_data <-
#'   "https://osf.io/download/rtw5k/" %>%
#'   readr::read_rds() %>%
#'   as_curve_params()
#'
#' print(curve_data)
as_curve_params <- function(data, antigen_isos = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort(
      class = "not data.frame",
      message = c(
        "Can't convert {.arg data} to {.cls curve_params}.",
        "x" = "{.arg data} must be a {.cls data.frame}" %>%
          "(or a subclass of {.cls data.frame}).",
        "i" = "You have supplied a {.cls {class(data)}}."
      )
    )
  }

  curve_data <-
    data %>%
    tibble::as_tibble()

  # define curve columns
  curve_cols <- c("antigen_iso", "y0", "y1", "t1", "alpha", "r")

  # Check if object is a curve (with columns)
  if (!all(is.element(curve_cols, names(curve_data)))) {
    cli::cli_abort(
      class = "not curve_params",
      message = c(
        "Can't convert {.arg data} to {.cls curve_params}.",
        "x" = paste0("The column{?s}: {.strong {.var ",
          "{setdiff(curve_cols, names(data))}}} are missing.")
      )
    )
  }

  # Assign curve_params class
  class(curve_data) <-
    c("curve_params", class(curve_data))

  # Handle antigen_isos
  if (is.null(antigen_isos)) {
    antigen_isos <- unique(curve_data$antigen_iso)
  } else {
    stopifnot(all(is.element(antigen_isos, curve_data$antigen_iso)))
  }

  # assign antigen attribute
  attr(curve_data, "antigen_isos") <- antigen_isos

  return(curve_data)
}
