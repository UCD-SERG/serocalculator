#' Load antibody decay curve parameter
#'
#' @param data a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector of antigen isotypes to be used in analyses
#' @returns a `curve_data` object (a [tibble::tbl_df] with extra attribute `antigen_isos`)
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
  curve_data <-
    data %>%
    tibble::as_tibble()

  # define curve columns
  curve_cols <- c("antigen_iso", "y0", "y1", "t1", "alpha", "r")

  # check if object is curve (with columns)
  if (!all(is.element(curve_cols, curve_data %>% names()))) {
    cli::cli_abort(
      class = "not curve_params",
      message = c("Please provide curve data.")
    )
  }

  # assign curve class
  class(curve_data) <-
    c("curve_params", class(curve_data))


  if (is.null(antigen_isos)) {
    antigen_isos <- unique(curve_data$antigen_iso)
  } else {
    stopifnot(all(
      is.element(antigen_isos, curve_data$antigen_iso)
    ))
  }

  # assign antigen attribute
  attr(curve_data, "antigen_isos") <- antigen_isos

  return(curve_data)
}
