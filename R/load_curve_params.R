#' Load antibody decay curve parameter samples
#'
#' @param file_path path to an RDS file containing MCMC samples of antibody decay curve parameters `y0`, `y1`, `t1`, `alpha`, and `r`, stored as a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector of antigen isotypes to be used in analyses
#'
#' @returns a `curve_params` object (a [tibble::tbl_df] with extra attribute `antigen_isos`)
#' @export
#' @examples
#' curve <- load_curve_params(serocalculator_example("example_curve_params.rds"))
#'
#' print(curve)
#'
load_curve_params <- function(file_path, antigen_isos = NULL) {
  if (file_path %>% substr(1, 4) == "http") {
    file_path <- url(file_path)
  }

  curve_params <-
    file_path %>%
    readRDS() %>%
    as_sr_params()

  return(curve_params)
}
