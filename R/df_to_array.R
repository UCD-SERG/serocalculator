#' Convert a data.frame (or tibble) into a multidimensional array
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `df.to.array()` was renamed to `df_to_array()` to create a more
#' consistent API.
#'
#' @keywords internal
#'
df.to.array <- function( # nolint: object_name_linter
    df,
    dim_var_names,
    value_var_name = "value") {
  lifecycle::deprecate_warn("1.0.0", "df.to.array()", "df_to_array()")
  df_to_array(df, dim_var_names, value_var_name)
}

#' Convert a data.frame (or tibble) into a multidimensional array
#'
#' @param df a [data.frame()] (or [tibble::tibble()]) in long format
#' (each row contains one value for the intended array)
#' @param dim_var_names a [character()] vector of variable names in `df`.
#' All of these variables should be factors, or a warning will be produced.
#' @param value_var_name a [character()] variable containing a variable name
#' from `df` which contains the values for the intended array.
#' @return an [array()] with dimensions defined by the variables in `df`
#' listed in `dim_var_names`
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#'
#' df <- iris %>%
#'   tidyr::pivot_longer(
#'     names_to = "parameter",
#'     cols = c("Sepal.Length", "Sepal.Width", "Petal.Width", "Petal.Length")
#'   ) %>%
#'   mutate(parameter = factor(parameter, levels = unique(parameter)))
#' arr <- df %>%
#'   serocalculator::df_to_array(
#'      dim_var_names = c("parameter", "Species"))
#' ftable(arr[,,1:5])
#' @dev
df_to_array <- function(
    df,
    dim_var_names,
    value_var_name = "value") {
  stopifnot(all(dim_var_names %in% names(df)))
  stopifnot(value_var_name %>% length() == 1)
  stopifnot(value_var_name %in% names(df))
  if (is.grouped_df(df)) {
    stop("ungroup the data frame first before running")
  }

  all_factors <-
    df %>%
    select(all_of(dim_var_names)) %>%
    sapply(FUN = is.factor) %>%
    all()

  if (!all_factors) {
    warning(
      "Some dimension variables are not factors.",
      "\nThese dimensions will be ordered by first appearance.",
      "\nCheck results using `dimnames()`"
    )
  }

  df <- df %>%
    mutate(
      across(
        all_of(dim_var_names),
        .fns = function(x) factor(x, levels = union(levels(x), unique(x)))
      )
    )

  xtabs_formula <-
    paste(
      value_var_name,
      " ~ ",
      paste(c(dim_var_names, "obs"), collapse = " + ")
    )

  df %>%
    mutate(.by = all_of(dim_var_names), obs = row_number()) %>%
    xtabs(formula = formula(xtabs_formula))
}
