#' Check the formatting of a cross-sectional antibody survey dataset.
#'
#' @param pop_data dataset to check
#' @param verbose whether to print an "OK" message when all checks pass
#' @returns NULL (invisibly)
#' @export
#' @examples
#' library(dplyr)
#' xs_data <- readr::read_rds("https://osf.io/download//n6cp3/") %>%
#'             as_pop_data()
#'   check_pop_data(xs_data, verbose = TRUE)
#'
check_pop_data <- function(pop_data, verbose = FALSE) {
  if (!is.data.frame(pop_data)) {
    cli::cli_abort(
      message = .pasteN(
        "Argument `pop_data` is not a `data.frame()`.",
        "Provide a `data.frame()` with cross-sectional serology data per antigen isotype."
      )
    )
  }

  missing_age <- is.element(attributes(pop_data)$age_var, names(pop_data))

  if (!missing_age) {
    "Argument {.arg pop_data} is missing column {.var {attributes(pop_data)$age_var}} (age, in years)" %>%
      cli::cli_abort(class = "missing-var")
  }

  missing_value <- is.element(attributes(pop_data)$value_var, names(pop_data))

  if (!missing_value) {
    "Argument {.arg pop_data} is missing column {.var {pop_data %>% get_value_var()}} (antibody measurement)" %>%
      cli::cli_abort(class = "missing-var")
  }

  if (verbose)
    cli::cli_inform("data format is as expected.")
  invisible(NULL)
}
