#' Plot distribution of antibodies
#' @description
#' `autoplot()` method for `pop_data` objects
#'
#' @param object A `pop_data` object (from [load_pop_data()])
#' @param log whether to show antibody responses on logarithmic scale
#' @param strata the name of a variable in `pop_data`
#' to stratify by (or `NULL` for no stratification)
#' @param ... unused
#' @param type an option to choose type of chart:
#' the current options are `"density"` or `"age-scatter"`
#'
#' @return a [ggplot2::ggplot] object
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(ggplot2)
#' library(magrittr)
#'
#' xs_data <-
#'   serocalculator_example("example_pop_data.csv") |>
#'   read.csv() |>
#'   as_pop_data()
#'
#' xs_data |> autoplot(strata = "catchment", type = "density")
#' xs_data |> autoplot(strata = "catchment", type = "age-scatter")
#' }
#' @export
autoplot.pop_data <- function(
    object,
    log = FALSE,
    type = "density",
    strata = NULL,
    ...) {
  if (!is.null(strata) && !is.element(strata, names(object))) {
    cli::cli_abort(
      class = "unavailable_strata",
      message = c(
        x = "The variable {.var {strata}} specified by argument {.arg strata}
        does not exist in {.arg object}.",
        i = "Please choose a column that exists in {.arg object}."
      )
    )
  }

  if (type == "age-scatter") {
    age_scatter(object, strata)
  } else if (type == "density") {
    density_plot(object, strata, log)
  } else {
    cli::cli_abort(
      class = "unavailable_type",
      message = c(
        x = "{.fn autoplot.pop_data} does not currently have an option for
         {.arg type} = {.str {type}}.",
        i = "The {.arg type} argument accepts options
        {.str density} or {.str age-scatter}."
      )
    )
  }
}

