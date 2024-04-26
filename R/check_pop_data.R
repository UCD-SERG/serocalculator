#' Check the formatting of a cross-sectional antibody survey dataset.
#'
#' @param pop_data dataset to check
#'
#' @returns NULL (invisibly)
#' @export
#' @examples
#' library(dplyr)
#'
#' #Import cross-sectional data from OSF and rename required variables
#' xs_data <- load_pop_data(file_path = "https://osf.io/download//n6cp3/")
#'
#' xs_data %>% check_pop_data()
#'
check_pop_data <- function(pop_data)
{
  if (!is.data.frame(pop_data)) {
    stop(.pasteN("Argument `pop_data` is not a `data.frame()`.",
                 "Provide a `data.frame()` with cross-sectional serology data per antigen isotype."))
  }

  #if (!(xor(x = is.element(c(attributes(pop_data)$age_var,'age'), names(pop_data))[1],
  #               y = is.element(c(attributes(pop_data)$age_var,'age'), names(pop_data))[2] ))) {
  #  stop("Argument `pop_data` is missing column `age` (age, in years).")
  #}


  if (!is.element("age", names(pop_data))) {
    stop("Argument `pop_data` is missing column `age` (age, in years).")
  }

  #if (!(xor(x = is.element(c(attributes(pop_data)$value_var,'value'), names(pop_data))[1],
  #               y = is.element(c(attributes(pop_data)$value_var,'value'), names(pop_data))[2]))) {
  #  stop("Argument `pop_data` is missing column `value` (antibody measurement).")
  #}

  if (!is.element("value", names(pop_data))) {
    stop("Argument `pop_data` is missing column `value` (antibody measurement).")
  }

  message("data format is as expected.")
  invisible(NULL)
}
