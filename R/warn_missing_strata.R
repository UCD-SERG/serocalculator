warn_missing_strata = function(
    data,
    strata,
    dataname)
{
  present_strata_vars = intersect(
    names(strata),
    names(data))

  missing_strata_vars = setdiff(
    names(strata),
    names(data))



  if(length(missing_strata_vars) > 0)
  {
    warning(
      dataname,
      " is missing some strata variables: ",
      missing_strata_vars |> paste(collapse = ", "),
      "\n", dataname, " will only be stratified by: ",
      present_strata_vars |> paste(collapse = ","))
  }

  if(length(present_strata_vars) > 0)
  {
    strata2 = data |> get_strata(present_strata_vars)

    missing_strata =
      anti_join(
        strata,
        strata2,
        by = present_strata_vars
      ) |>
      distinct(across(all_of(present_strata_vars)))

    if(nrow(missing_strata) > 0)
    {
      message("The following strata are missing in ", dataname, ":")
      print(missing_strata)
      stop("Missing strata in `", dataname, "`")
    }
  }

  return(present_strata_vars)


}
