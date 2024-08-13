#'
#' @title Summarize cross-sectional antibody survey data
#' @description
#' [summary()] method for `pop_data` objects
#'
#' @param object a `pop_data` object (from [as_pop_data()])
#' @param strata a [character()] specifying grouping column(s)
#' @param ...  unused
#'
#' @returns a `summary.pop_data` object, which is a list containing two summary tables:
#'
#' * `age_summary` summarizing `age`
#' * `ab_summary` summarizing `value`, stratified by `antigen_iso`
#'
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
    distinct(across(all_of(cols)))

  age_summary <-
    ages %>%
    summarise(
      .by = all_of(strata),
      n = n(),
      min = min(.data[[age_column]]),
      first_quartile = quantile(.data[[age_column]], 0.25),
      median = median(.data[[age_column]]),
      mean = mean(.data[[age_column]]),
      third_quartile = quantile(.data[[age_column]], 0.75),
      max = max(.data[[age_column]])
    )

  ab_summary <-
    object %>%
    dplyr::summarize(
      .by = all_of(c("antigen_iso", strata)),
      across(
        .cols = all_of(value_column),
        .fns = list(
          Min = ~ min(.x, na.rm = TRUE),
          `1st Qu.` = ~ quantile(.x, p = .25, na.rm = TRUE),
          Median = ~ median(.x, na.rm = TRUE),
          `3rd Qu.` = ~ quantile(.x, p = .75, na.rm = TRUE),
          Max = ~ max(.x, na.rm = TRUE),
          `# NAs` = ~ is.na(.x) %>% sum()
        ),
        .names = "{.fn}"
      ))

  to_return <- list(n = nrow(ages),
                    age_summary = age_summary,
                    ab_summary = ab_summary)

  class(to_return) = "summary.pop_data"

  return(to_return)
}


#' Print method for [summary.pop_data] objects
#' @param x an object of class `"summary.pop_data"`; usually, the result of a call to [summary.pop_data()]
#' @rdname summary.pop_data
#' @export
print.summary.pop_data = function(x, ...)
{
  n_obs = x$age_summary %>% pull("n") %>% sum()

  cat("\nn =", n_obs, "\n")

  cat("\nDistribution of age: \n\n")

  x$age_summary %>% print()

  cat("\nDistributions of antigen-isotype measurements:\n\n")

  x$ab_summary %>% print()

  cat("\n")

  invisible(x)
}
