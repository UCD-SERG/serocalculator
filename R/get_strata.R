get_strata = function(data, strata_varnames)
{
  to_return =
    data |>
    distinct(across(any_of(strata_varnames))) |>
    mutate(Stratum = paste("Stratum", row_number()))

}
