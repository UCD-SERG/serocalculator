#'
#' @title Summarize a cross-sectional antibody survey data set
#' @description
#' This function is a `summary()` method for `pop_data` objects
#'
#' @param object a `pop_data` object
#' @param ...  unused
#'
#' @returns a list containing two summary tables: one of `age` and one of `value`, stratified by `antigen_iso`
#' @export
#' @examples
#' library(dplyr)
#'
#' xs_data <- load_pop_data(file_path = "https://osf.io/download//n6cp3/",
#'                          age = "Age",
#'                          id = "index_id",
#'                          value = "result")
#'
#' xs_data %>% summary()
#'
summary.pop_data = function(object, ...)
{

  ages =
    object %>%
    distinct(.data$id,.data$age)

  cat("\nn =", nrow(ages),"\n")

  cat("\nDistribution of age: \n\n")
  age_summary =
    ages %>%
    pull('age') %>%
    summary() %>%
    print()

  cat('\nDistributions of antigen-isotype measurements:\n\n')

  ab_summary =
    object %>%
    dplyr::summarize(
      .by = .data$antigen_iso,
      Min = object %>% pull('value') %>% min(na.rm = TRUE),
      `1st Qu.` = object %>% pull('value') %>% quantile(.25, na.rm = TRUE),
      Median = object %>% pull('value') %>% median(),
      `3rd Qu.` = object %>% pull('value') %>% quantile(.75, na.rm = TRUE),
      Max = object %>% pull('value') %>% max(na.rm = TRUE),
      `# NAs` = object %>% pull('value') %>% is.na() %>% sum()
    ) %>%
    as.data.frame() %>%
    print()

  to_return = list(
    n = nrow(ages),
    age_summary = age_summary,
    ab_summary = ab_summary)

  return(invisible(to_return))
}
