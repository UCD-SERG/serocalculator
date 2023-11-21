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
      mutate(Stratum = paste("Stratum", row_number()))

  }

  if(any(duplicated(to_return$Stratum)))
  {
    stop("The data contain multiple strata with the same value of the `Stratum` variable. Please disambiguate.")
  }

  return(to_return)

}
