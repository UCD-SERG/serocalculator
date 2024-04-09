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
    distinct(eval(parse(text=attributes(object)$id_var)),
             eval(parse(text=attributes(object)$age_var)))

  names(ages) <- c(attributes(object)$id_var,
                        attributes(object)$age_var)

  cat("\nn =", nrow(ages),"\n")

  cat("\nDistribution of age: \n\n")
  age_summary =
    ages %>%
    pull(attributes(object)$age_var) %>%
    summary() %>%
    print()

  cat('\nDistributions of antigen-isotype measurements:\n\n')

  ab_summary =
    object %>%
    dplyr::summarize(
      .by = .data$antigen_iso,
      Min = object %>% pull(attributes(object)$value_var) %>% min(na.rm = TRUE),
      `1st Qu.` = object %>% pull(attributes(object)$value_var) %>% quantile(.25, na.rm = TRUE),
      Median = object %>% pull(attributes(object)$value_var) %>% median(),
      `3rd Qu.` = object %>% pull(attributes(object)$value_var) %>% quantile(.75, na.rm = TRUE),
      Max = object %>% pull(attributes(object)$value_var) %>% max(na.rm = TRUE),
      `# NAs` = object %>% pull(attributes(object)$value_var) %>% is.na() %>% sum()
    ) %>%
    as.data.frame() %>%
    print()

  to_return = list(
    n = nrow(ages),
    age_summary = age_summary,
    ab_summary = ab_summary)

  return(invisible(to_return))
}
