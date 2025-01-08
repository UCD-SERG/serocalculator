#' Load a cross-sectional antibody survey data set
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
#' library(magrittr)
#' xs_data <-
#'   fs::path_package(
#'      package = "serocalculator",
#'      "extdata/example_pop_data.csv") %>%
#'   read.csv() %>%
#'   as_pop_data()
#'
#' print(xs_data)
as_pop_data <- function(data,
                        antigen_isos = NULL,
                        age = "Age",
                        value = "result",
                        id = "index_id",
                        standardize = TRUE) {


  pop_data <-
    data %>%
    tibble::as_tibble()

  class(pop_data) <-
    c("pop_data", class(pop_data))

  if (is.null(antigen_isos)) {
    antigen_isos <- unique(pop_data$antigen_iso)
  } else {
    stopifnot(all(is.element(antigen_isos, pop_data$antigen_iso)))
  }

  attr(pop_data, "antigen_isos") <- antigen_isos

  pop_data <- pop_data %>%
    set_age(age = age, standardize = standardize) %>%
    set_value(value = value, standardize = standardize) %>%
    set_id(id = id, standardize = standardize)  %>%
    set_biomarker_var(biomarker = "antigen_iso", standardize = standardize)

  return(pop_data)
}
