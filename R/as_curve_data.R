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
as_curve_data <- function(data, antigen_isos = NULL) {

  # convert data to a tibble data type
  tibble_data <-
    data %>%
    tibble::as_tibble()

  # convert data to curve class
  converted_data <-
    tibble_data %>% curve_converter()

  return(curve_data)
}

curve_converter <- function(data, antigen_isos = NULL) {
  class(data) <- c(
    "curve_data",
    data
  )

  if (is.null(antigen_isos)) {
    antigen_isos <- unique(curve_data$antigen_iso)
  } else {
    stopifnot(
      all(
        is.element(
          antigen_isos,
          curve_data$antigen_iso
        )
      )
    )
  }

  if (
    all(
      is.element(
        c("antigen_iso", "iter", "y0", "y1", "alpha", "r"),
        curve_data %>% names()
      )
    )) {
    attr(curve_data, "antigen_isos") <- antigen_isos

    return(curve_data)
  } else {
    cli::cli_abort("Please provide curve data")
  }
  return(data)
}

