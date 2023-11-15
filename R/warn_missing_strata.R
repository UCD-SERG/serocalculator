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



    if(length(present_strata_vars) > 0)
    {

      message =
        c(
          dataname,
          " is missing some strata variables: ",
          missing_strata_vars |> paste(collapse = ", "),
          "\n", dataname, " will only be stratified by: ",
          present_strata_vars |> paste(collapse = ",")
      )
    } else
    {
      message = c(
        dataname,
        " is missing all strata variables, and will be used unstratified.")
    }

    message2 = c(
      "\To avoid this warning, specify the desired set of stratifying",
      " variables in the `curve_strata_varnames` and `noise_strata_varnames`",
      " arguments to `est.incidence.by()`"
    )

    warning(message, message2)
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
