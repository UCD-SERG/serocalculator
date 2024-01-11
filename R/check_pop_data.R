#' Check the formatting of a cross-sectional antibody survey dataset.
#'
#' @param data a [data.frame]
#' @param antigen_isos
#'
#' @returns `NULL` (invisibly)
#' @export
check_pop_data <- function(
    pop_data,
    antigen_isos)
{
  if (!is.data.frame(pop_data)) {
    stop(.pasteN("Argument `pop_data` is not a `data.frame()`.",
                 "Provide a `data.frame()` with cross-sectional serology data per antigen isotype."))
  }

  if (!is.element("age", names(pop_data))) {
    stop("Argument `pop_data` is missing column `age` (age, in years).")
  }

  if (!is.element("value", names(pop_data))) {
    stop("Argument `pop_data` is missing column `value` (antibody measurement).")
  }

  invisible(NULL)
}
