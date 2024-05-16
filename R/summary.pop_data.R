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
#' xs_data <- "https://osf.io/download//n6cp3/" %>%
#'   load_pop_data() %>%
#'   clean_pop_data()
#'
#' xs_data %>% summary()
#'
summary.pop_data <- function(object, strata = "Country", ...) {
  ages <-
    object %>%
    distinct(.data$id, .data$age, .data[[strata]])

  cat("\nn =", nrow(ages), "\n")

  cat("\nDistribution of age: \n\n")
  age_summary <-
    ages %>%
    select(age, all_of(strata)) %>%
    group_by(across(all_of(strata))) %>%
    summarise(
      age_min = min(age),
      age_first_quartile = quantile(age, 0.25),
      age_median = median(age),
      age_mean = mean(age),
      age_third_quartile = quantile(age, 0.75),
      age_max = max(age)
    ) %>%
    print()

  cat("\nDistributions of antigen-isotype measurements:\n\n")

  ab_summary <-
    object %>%
    dplyr::summarize(
      .by = .data$antigen_iso,
      Min = .data$value %>% min(na.rm = TRUE),
      `1st Qu.` = .data$value %>% quantile(.25, na.rm = TRUE),
      Median = .data$value %>% median(),
      `3rd Qu.` = .data$value %>% quantile(.75, na.rm = TRUE),
      Max = .data$value %>% max(na.rm = TRUE),
      `# NAs` = .data$value %>% is.na() %>% sum()
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
