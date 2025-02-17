#' Load longitudinal antibody seroresponse parameters
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `as_curve_params()` was renamed to [as_sr_params()] to use more accurate terminology.
#'
#' @keywords internal
#' @export
as_curve_params <- function(...) {
  lifecycle::deprecate_warn("1.3.0", "as_curve_params()", "as_sr_params()")
  as_sr_params(...)
}

#'
#' @param data a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos a [character()] vector of antigen isotypes
#' to be used in analyses
#' @returns a `sr_data` object
#' (a [tibble::tbl_df] with extra attribute `antigen_isos`)
#' @export
#' @examples
#' library(magrittr)
#' sr_data <-
#'   serocalculator_example("example_sr_params.csv") %>%
#'   read.csv() %>%
#'   as_sr_params()
#'
#' print(sr_data)
as_sr_params <- function(data, antigen_isos = NULL) {

  if (!is.data.frame(data)) {
    cli::cli_abort(
      class = "not data.frame",
      message = c(
        "Can't convert {.arg data} to {.cls curve_params}.",
        "x" = "{.arg data} must be a {.cls data.frame}
        (or a subclass of {.cls data.frame}).",
        "i" = "You have supplied a {.cls {class(data)}}."
      )
    )
  }

  sr_data <-
    data %>%
    tibble::as_tibble()

  # check if object has expected columns:

  # define curve columns
  sr_cols <- c("antigen_iso", "y0", "y1", "t1", "alpha", "r")

  # get columns from provided data
  data_cols <- data %>% names()

  # get any missing column(s)
  missing_cols <- setdiff(x = curve_cols, y = data_cols)

  if (length(missing_cols) > 0) {

    cli::cli_abort(
      class = "not sr_params",
      message = c(
        "Can't convert {.arg data} to {.cls sr_params}.",
        "x" = "The column{?s}: {.strong {.var {missing_cols}}} are missing."
      )
    )
  }

  # assign sr class
  class(sr_data) <-
    c("sr_params", class(sr_data))


  if (is.null(antigen_isos)) {
    antigen_isos <- unique(curve_data$antigen_iso)
  } else {
    stopifnot(all(
      is.element(antigen_isos, curve_data$antigen_iso)
    ))
  }

  # assign antigen attribute
  attr(sr_data, "antigen_isos") <- antigen_isos

  sr_data <- sr_data %>%
    set_biomarker_var(biomarker = "antigen_iso", standardize = FALSE)

  return(sr_data)
}
