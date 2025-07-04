#' Load longitudinal seroresponse parameters
#'
#' @param data a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos a [character()] vector of antigen isotypes
#' to be used in analyses
#' @returns a `curve_data` object
#' (a [tibble::tbl_df] with extra attribute `antigen_isos`)
#' @export
#' @examples
#' library(magrittr)
#' curve_data <-
#'   serocalculator_example("example_curve_params.csv") %>%
#'   read.csv() %>%
#'   as_curve_params()
#'
#' print(curve_data)
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

  curve_data <-
    data |>
    tibble::as_tibble()

  # check if object has expected columns:

  # define curve columns
  curve_cols <- c("antigen_iso", "y0", "y1", "t1", "alpha", "r")

  # get columns from provided data
  data_cols <- data |>  names()

  # get any missing column(s)
  missing_cols <- setdiff(x = curve_cols, y = data_cols)

  if (length(missing_cols) > 0) {

    cli::cli_abort(
      class = "not curve_params",
      message = c(
        "Can't convert {.arg data} to {.cls curve_params}.",
        "x" = "The column{?s}: {.strong {.var {missing_cols}}} are missing."
      )
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

  # if `object` lacks an `iter` column, add it:
  if (!is.element("iter", names(curve_data))) {
    cli::cli_warn(
      c(
        "`data` is missing `iter` column",
        "It will be inferred from row ordering."
      )
    )
    curve_data <-
      curve_data |>
      mutate(
        .by = any_of(c("antigen_iso", "chain")),
        iter = row_number()
      )
  }

  # assign antigen attribute
  attr(curve_data, "antigen_isos") <- antigen_isos

  curve_data <- curve_data |>
    set_biomarker_var(biomarker = "antigen_iso", standardize = FALSE)

  return(curve_data)
}

#' @title Load antibody decay curve parameter
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `as_curve_params()` was renamed to [as_sr_params()] to create a more
#' consistent API.
#' @keywords internal
#' @export
as_curve_params <- function( # nolint: object_name_linter
    ...) {
  lifecycle::deprecate_soft("1.3.1", "as_curve_params()", "as_sr_params()")
  as_sr_params(
    ...
  )
}
