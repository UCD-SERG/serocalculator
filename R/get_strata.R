get_strata = function(data, strata_varnames)
{
  to_return =
    data |>
    count(across(any_of(c(strata_varnames, "antigen_iso")))) |>
    distinct(across(any_of(c(strata_varnames, "n"))))

  if(!("Stratum" %in% strata_varnames))
  {
    to_return =
      to_return |>
      mutate(Stratum = paste("Stratum", row_number())) |>
      dplyr::relocate("Stratum", .before = everything())

  }

  if(any(duplicated(to_return$Stratum)))
  {
    stop("The data contain multiple strata with the same value of the `Stratum` variable. Please disambiguate.")
  }

  attr(to_return, "strata_vars") = strata_varnames

  return(to_return)

}
