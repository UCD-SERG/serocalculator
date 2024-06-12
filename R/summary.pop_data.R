#'
#' @title Summarize a cross-sectional antibody survey data set
#' @description
#' This function is a `summary()` method for `pop_data` objects
#'
#' @param object a `pop_data` object
#' @param strata a [character()] providing the grouping column
#' @param ...  unused
#'
#' @returns a list containing two summary tables: one of `age` and one of `value`, stratified by `antigen_iso`
#' @export
#' @examples
#' library(dplyr)
#'
#' xs_data <- load_pop_data("https://osf.io/download//n6cp3/")
#' summary(xs_data, strata = "Country")
#'
summary.pop_data <- function(object, strata = NULL, ...) {
  # get relevant columns from object
  age_column <- object %>% get_age_var()
  value_column <- object %>% get_value_var()
  id_column <- object %>% get_id_var()

  # create a list of the columns
  cols <- c(age_column, id_column, strata)

  ages <-
    object %>%
    distinct(
      across(all_of(cols))
    )

  cat("\nn =", nrow(ages), "\n")

  cat("\nDistribution of age: \n\n")

  # columns to use
  cols_select <- c(age_column, strata)

  age_summary <-
    ages %>%
    select(all_of(cols_select)) %>%
    summarise(
      min = min(.data[[age_column]]),
      first_quartile = quantile(.data[[age_column]], 0.25),
      median = median(.data[[age_column]]),
      mean = mean(.data[[age_column]]),
      third_quartile = quantile(.data[[age_column]], 0.75),
      max = max(.data[[age_column]]),
      .by = strata
    ) %>%
    print()

  cat("\nDistributions of antigen-isotype measurements:\n\n")

  ab_summary <-
    object %>%
    dplyr::summarize(
      .by = all_of(c("antigen_iso", strata)),
      Min = object %>%
        get_value() %>%
        min(na.rm = TRUE),
      `1st Qu.` = object %>%
        get_value() %>%
        quantile(.25, na.rm = TRUE),
      Median = object %>%
        get_value() %>%
        median(),
      `3rd Qu.` = object %>%
        get_value() %>%
        quantile(.75, na.rm = TRUE),
      Max = object %>%
        get_value() %>%
        max(na.rm = TRUE),
      `# NAs` = object %>%
        get_value() %>%
        is.na() %>%
        sum()
    ) %>%
    as.data.frame() %>%
    print()

  to_return <- list(
    n = nrow(ages),
    age_summary = age_summary,
    ab_summary = ab_summary
  )

  return(invisible(to_return))
}
