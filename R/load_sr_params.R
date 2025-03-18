#' Load longitudinal seroresponse parameter samples
#'
#' @param file_path path to an RDS file containing MCMC samples of antibody
#' seroresponse parameters `y0`, `y1`, `t1`, `alpha`, and `r`,
#' stored as a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector of antigen isotypes used in analyses
#'
#' @returns a `curve_params` object (a [tibble::tbl_df]
#' with extra attribute `antigen_isos`)
#' @export
#' @examples
#' curve <- load_sr_params(serocalculator_example("example_curve_params.rds"))
#'
#' print(curve)
#'
load_sr_params <- function(file_path, antigen_isos = NULL) {
  if (file_path |>  substr(1, 4) == "http") {
    file_path <- url(file_path)
  }

  curve_params <-
    file_path |>
    readRDS() |>
    as_curve_params()

  return(curve_params)
}

#' @title Load antibody decay curve parameter samples
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `load_curve_params()` was renamed to [load_sr_params()] to create a more
#' consistent API.
#' @keywords internal
#' @export
load_curve_params <- function(
    ...) {
  lifecycle::deprecate_soft("1.3.1", "load_curve_params()", "load_sr_params()")
  load_sr_params(
    ...
  )
}
