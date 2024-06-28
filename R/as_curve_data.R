#' Load antibody decay curve parameter
#'
#' @param data a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector of antigen isotypes to be used in analyses
#' @param age a [character()] identifying the age column
#' @param id a [character()] identifying the id column
#' @param value a [character()] identifying the value column
#' @param standardize a [logical()] to determine standardization of columns
#' @returns a `pop_data` object (a [tibble::tbl_df] with extra attribute `antigen_isos`)
#' @export
#' @examples
#' xs_data <- load_curve_params("https://osf.io/download/rtw5k/")
#'
#' print(xs_data)
#'

as_curve_data <- function(data, antigen_isos = NULL){

  curve_data <-
    data %>%
    tibble::as_tibble()

  class(curve_data) <-
    c("curve_params", class(curve_data))

  if (is.null(antigen_isos)) {
    antigen_isos <- unique(curve_data$antigen_iso)
  } else {
    stopifnot(all(is.element(antigen_isos, curve_data$antigen_iso)))
  }

  attr(curve_data, "antigen_isos") <- antigen_isos

  return(curve_data)
}
