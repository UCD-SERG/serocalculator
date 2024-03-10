count_strata = function(data, strata_varnames)
{
  to_return =
    data %>%
    count(across(any_of(c(strata_varnames, "antigen_iso"))))

  uneven_counts =
    to_return %>%
    dplyr::filter(
      .by = all_of(strata_varnames),
      n_distinct(n) > 1)

  if(nrow(uneven_counts) > 0)
  {
    warning('The number of observations in `data` varies between antigen isotypes, for at least one stratum. Sample size for each stratum will be calculated as the minimum number of observations across all antigen isotypes.\n')
  }

  to_return =
    to_return %>%
    dplyr::summarize(
      .by = all_of(strata_varnames),
      n = min(n)
    )

  if(!("Stratum" %in% strata_varnames))
  {
    to_return =
      to_return %>%
      mutate(Stratum = paste("Stratum", row_number())) %>%
      dplyr::relocate("Stratum", .before = everything())

  }

  if(any(duplicated(to_return$Stratum)))
  {
    stop("The data contain multiple strata with the same value of the `Stratum` variable. Please disambiguate.")
  }

  attr(to_return, "strata_vars") = strata_varnames

  return(to_return)

}
