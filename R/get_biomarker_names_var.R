#' Get biomarker variable name
#'
#' @param object a `pop_data` object
#' @param ... unused
#'
#' @returns
#' a [character] string identifying the biomarker names column in `object`
#' @export
#'
#' @examples
#' sees_pop_data_100 |> get_biomarker_names_var()
get_biomarker_names_var <- function(object, ...) {
  # get value attribute
  biomarker_var <- attributes(object)[["biomarker_var"]]

  if (is.null(biomarker_var)) {
    if ("antigen_iso" %in% colnames(object)) {
      biomarker_var <- "antigen_iso"
      cli::cli_warn(
        class = "biomarker_var attribute missing",
        message = c(
          "{.var object} lacks attribute {.attr biomarker_var}.",
          "i" = "Assuming {.attr biomarker_var} = {.str antigen_iso}"
        )
      )
    } else {
      cli::cli_abort(
        class = "biomarker_var attribute missing",
        message = c(
          "{.var object} lacks attribute {.attr biomarker_var},
        and we could not guess which column it should be.",
          "i" = "Please specify the correct column manually`."
        )
      )
    }

  }

  return(biomarker_var)
}
