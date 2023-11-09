get_strata = function(data, strata_varnames)
{
  to_return =
    data |>
    count(across(any_of(c(strata_varnames, "antigen_iso")))) |>
    distinct(across(any_of(c(strata_varnames, "n")))) |>
    mutate(Stratum = paste("Stratum", row_number()))

}
