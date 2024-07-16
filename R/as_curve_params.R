#' Load antibody decay curve parameter
#'
#' @param data a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector of antigen isotypes to be used in analyses
#' @returns a `curve_data` object (a [tibble::tbl_df] with extra attribute `antigen_isos`)
#' @export
#' @examples
#' curve_data <- load_curve_params("https://osf.io/download/rtw5k/")
#'
#' print(curve_data)
#'
curve_converter <- function(data, antigen_isos = NULL) {
  # define curve columns
  curve_cols <- c("antigen_iso", "y0", "y1", "t1", "alpha", "r")

  # check if object is curve (with columns)
  if (!all(is.element(curve_cols, data %>% names()))) {
    cli::cli_abort("Please provide curve data")
  }

  # assign curve class
  class(data) <-
    c("curve_params", class(data))


  if (is.null(antigen_isos)) {
    antigen_isos <- unique(data$antigen_iso)
  } else {
    stopifnot(all(
      is.element(antigen_isos, data$antigen_iso)
    ))
  }

  # assign antigen attribute
  attr(data, "antigen_isos") <- antigen_isos

  return(data)
}

as_curve_params <- function(data, antigen_isos = NULL) {
  curve_data <-
    data %>%
    tibble::as_tibble()

  curve_data <- curve_converter(
    data = curve_data
  )
}
