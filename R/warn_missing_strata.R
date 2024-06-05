#' Warn about missing stratifying variables in a dataset
#'
#' @param data the dataset that should contain the strata
#' @param strata a [data.frame()] showing the strata levels that are expected to be in the dataset
#' @param dataname the name of the dataset, for use in warning messages if some strata are missing.
#'
#' @return a [character()] vector of the subset of stratifying variables that are present in `pop_data`
#'
warn.missing.strata = function(
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
          "`",
          dataname,
          "` is missing some strata variables: `",
          missing_strata_vars %>% paste(collapse = "`, `"),
          "`\n`", dataname, "` will only be stratified by: `",
          present_strata_vars %>% paste(collapse = "` , `"),
          "`"
      )
    } else
    {
      message = c(
        dataname,
        " is missing all strata variables, and will be used unstratified.")
    }

    message2 = c(
      "\n\nTo avoid this warning, specify the desired set of stratifying",
      " variables in the `curve_strata_varnames` and `noise_strata_varnames`",
      " arguments to `est.incidence.by()`.\n"
    )

    warning(message, message2)
  }

  if(length(present_strata_vars) > 0)
  {
    strata2 = data %>% count_strata(present_strata_vars)

    missing_strata =
      anti_join(
        strata,
        strata2,
        by = present_strata_vars
      ) %>%
      distinct(across(all_of(present_strata_vars)))

    if(nrow(missing_strata) > 0)
    {
      message(
        "The following strata variables are present in `",
        dataname,
        "`, but the following specific combinations of those strata are missing:")
      print(missing_strata)
      stop("Missing strata levels in `", dataname, "`.\n\n")
    }
  }

  return(present_strata_vars)


}
