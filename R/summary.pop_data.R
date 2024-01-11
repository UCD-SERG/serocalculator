#' @title Summarize a cross-sectional antibody survey data set
#' @description
#' This function is a `summary()` method for `pop_data` objects
#'
#' @returns a list containing two summary tables: one of `age` and one of `value`, stratified by `antigen_iso`
#' @export
summary.pop_data = function(pop_data)
{

  ages =
    pop_data |>
    distinct(id, age)

  cat("\nn =", nrow(ages),"\n")

  cat("\nDistribution of age: \n\n")
  age_summary =
    ages$age |>
    summary() |>
    print()

  cat('\nDistributions of antigen-isotype measurements:\n\n')

  ab_summary =
    pop_data |>
    dplyr::summarize(
      .by = antigen_iso,
      Min = value |> min(na.rm = TRUE),
      `1st Qu.` = value |> quantile(.25, na.rm = TRUE),
      Median = value |> median(),
      `3rd Qu.` = value |> quantile(.75, na.rm = TRUE),
      Max = value |> max(na.rm = TRUE),
      `# NAs` = value |> is.na() |> sum()
    ) |>
    as.data.frame() |>
    print()

  to_return = list(
    n = nrow(ages),
    age_summary = age_summary,
    ab_summary = ab_summary)

  return(invisible(to_return))
}
