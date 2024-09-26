#' Load a cross-sectional antibody survey data set
#'
#' @param file_path path to an RDS file containing a cross-sectional antibody
#' survey data set, stored as a [data.frame()] or [tibble::tbl_df]
#' @inheritDotParams as_pop_data
#' @returns a `pop_data` object (a [tibble::tbl_df] with extra attributes)
#' @export
#' @examples
#' xs_data <- load_pop_data("https://osf.io/download//n6cp3/")
#'
#' print(xs_data)
load_pop_data <- function(file_path, ...) {
  pop_data <-
    file_path %>%
    readr::read_rds() %>%
    as_pop_data(...)

  return(pop_data)
}
