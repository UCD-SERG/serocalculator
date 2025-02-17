#' Load longitudinal antibody seroresponse parameters
#'
#'#' `r lifecycle::badge("deprecated")`
#'
#' `load_curve_params()` was renamed to [load_sr_params()] to use more accurate terminology.
#'
#' @keywords internal
#' @export
load_curve_params <- function(...) {
  lifecycle::deprecate_warn("1.3.0", "load_curve_params()", "load_sr_params()")
  load_sr_params(...)
}

#' @param file_path path to an RDS file containing MCMC samples of longitudinal antibody seroresponse parameters `y0`, `y1`, `t1`, `alpha`, and `r`, stored as a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector of antigen isotypes to be used in analyses
#'
#' @returns a `sr_params` object (a [tibble::tbl_df] with extra attribute `antigen_isos`)
#' @export
#' @examples
#' sr_params <- load_sr_params(serocalculator_example("example_sr_params.rds"))
#'
#' print(sr_params)
#'
load_sr_params <- function(file_path, antigen_isos = NULL) {
  if (file_path %>% substr(1, 4) == "http") {
    file_path <- url(file_path)
  }

  sr_params <-
    file_path %>%
    readRDS() %>%
    as_sr_params()

  return(sr_params)
}
