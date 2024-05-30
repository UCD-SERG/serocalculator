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
summary.pop_data <- function(object, strata = "Country", ...) {
  # get relevant columns from object
  age_column <- object %>% get_age_var()
  value_column <- object %>% get_value_var()
  id_column <- object %>% get_id_var()

  ages <-
    object %>%
    distinct(
      .data[[age_column]],
      .data[[id_column]],
      .data[[strata]]
    )

  cat("\nn =", nrow(ages), "\n")

  cat("\nDistribution of age: \n\n")
  age_summary <-
    ages %>%
    select(
      age_column,
      all_of(strata)
    ) %>%
    group_by(across(all_of(strata))) %>%
    summarise(
      age_min = min(.data[[age_column]]),
      age_first_quartile = quantile(.data[[age_column]], 0.25),
      age_median = median(.data[[age_column]]),
      age_mean = mean(.data[[age_column]]),
      age_third_quartile = quantile(.data[[age_column]], 0.75),
      age_max = max(.data[[age_column]])
    ) %>%
    print()

  cat("\nDistributions of antigen-isotype measurements:\n\n")

  ab_summary <-
    object %>%
    dplyr::summarize(
      .by = .data$antigen_iso,
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
