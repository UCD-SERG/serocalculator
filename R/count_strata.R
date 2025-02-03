count_strata <- function(data,
                         strata_varnames,
                         biomarker_names_var = get_biomarker_names_var(data)
                           ) {
  to_return <-
    data %>%
    count(across(any_of(c(strata_varnames, biomarker_names_var))))

  uneven_counts <-
    to_return %>%
    dplyr::filter(
      .by = all_of(strata_varnames),
      n_distinct(n) > 1
    )

  if (nrow(uneven_counts) > 0) {

    c(
      "The number of observations in `data` varies between antigen
      isotypes, for at least one stratum.",
      "Sample size for each stratum will be calculated as
      the minimum number of observations across all antigen isotypes."
    ) |>
    cli::cli_warn(class = "incomplete-obs",
                  body = capture.output(uneven_counts))
  }

  to_return <-
    to_return %>%
    dplyr::summarize(
      .by = all_of(strata_varnames),
      n = min(n)
    )

  if (!("Stratum" %in% strata_varnames)) {
    to_return <-
      to_return %>%
      mutate(Stratum = paste("Stratum", row_number())) %>%
      dplyr::relocate("Stratum", .before = everything())
  }

  if (any(duplicated(to_return$Stratum))) {
    c(
      "The data contain multiple strata with the same value
      of the {.var Stratum} variable.",
      "Please disambiguate."
    ) %>%
    cli::cli_abort()
  }

  attr(to_return, "strata_vars") <- strata_varnames

  return(to_return)
}
