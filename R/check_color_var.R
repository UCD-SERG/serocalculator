check_color_var <- function(
    color_var,
    object
) {

  # Check if color_var exists
  if (!is.null(color_var) && !is.element(color_var, names(object))) {
    cli::cli_abort(
      class = "unavailable_color_var",
      message = c(
        "The variable `{color_var}` specified by argument `color_var` does not exist in `object`.",
        "Please choose a column that exists in `object`."
      )
    )
  }
}
