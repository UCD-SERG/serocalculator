#' Extract biomarker names column
#'
#' @param object
#' a long-format dataset (`pop_data`, `curve_params`, etc)
#' @param ... unused
#'
#' @returns a [character] or [factor] [vector] of biomarker names
#' @export
#'
#' @examples
#' sees_pop_data_100 |> get_biomarker_names() |> head()
get_biomarker_names <- function(object, ...) {
  # get biomarker name data
  biomarker_names_var <- get_biomarker_names_var(object)
  biomarker_data <- object |> pull(biomarker_names_var)

  return(biomarker_data)
}
