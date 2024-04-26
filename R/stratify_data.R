stratify_data <- function(
    data,
    antigen_isos,
    curve_params,
    noise_params,
    strata_varnames = "",
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL)
{

  if(is.null(strata_varnames) || all(strata_varnames == ""))
  {
    all_data =
      list(
        pop_data = data %>% select(value,
                                   age,
                                   "antigen_iso"),
        curve_params = curve_params %>% select("y1", "alpha", "r", "antigen_iso"),
        noise_params = noise_params %>% select("nu", "eps", "y.low", "y.high", "antigen_iso")
      ) %>%
      structure(
        class = union(
          "biomarker_data_and_params",
          "list")
      )

    stratumDataList =
      list(# est.incidence.by() expects a list.
        `all data` = all_data
        ) %>%
      structure(
        antigen_isos = antigen_isos,
        strata = tibble(Stratum = NA)
      )


    return(stratumDataList)

  }

  # Make stratum variable (if needed)

  strata = data %>% count_strata(strata_varnames)

  strata_vars_curve_params =
    warn.missing.strata(
      data = curve_params,
      strata = strata %>% select(curve_strata_varnames),
      dataname = "curve_params"
    )

  strata_vars_noise_params =
    warn.missing.strata(
      data = noise_params,
      strata = strata %>% select(noise_strata_varnames),
      dataname = "noise_params"
    )

  # xs_dataStrata <- data %>% .makeStrata(strata_varnames)
  # curve_paramsStrata = curve_params %>% .makeStrata(strata_varnames)
  # noise_params_Strata = noise_params %>% .makeStrata(strata_varnames)
  # levelsStrata <- levels(xs_dataStrata$Stratum)

  stratumDataList = list()

  for (cur_stratum in strata$Stratum)
  {

    cur_stratum_vals =
      strata %>% dplyr::filter(.data$Stratum == cur_stratum)

    data_and_params_cur_stratum =
      list(
        pop_data =
          data %>%
          semi_join(
            cur_stratum_vals,
            by = strata_varnames) %>%
          select(value,
                 age,
                 "antigen_iso")
      )

    if(length(strata_vars_curve_params) == 0)
    {
      data_and_params_cur_stratum$curve_params =
        curve_params %>% select("y1", "alpha", "r", "antigen_iso")
    } else
    {
      data_and_params_cur_stratum$curve_params =
        curve_params %>%
        semi_join(
          cur_stratum_vals,
          by = strata_vars_curve_params) %>%
        select("y1", "alpha", "r", "antigen_iso")
    }

    if(length(strata_vars_noise_params) == 0)
    {
      data_and_params_cur_stratum$noise_params =
        noise_params %>%
        select("nu", "eps", "y.low", "y.high", "antigen_iso")
    } else
    {
      data_and_params_cur_stratum$noise_params =
        noise_params %>%
        semi_join(
          cur_stratum_vals,
          by = strata_vars_noise_params) %>%
        select("nu", "eps", "y.low", "y.high", "antigen_iso")
    }

    stratumDataList[[cur_stratum]] =
      data_and_params_cur_stratum %>%
      structure(
        class = union(
          "biomarker_data_and_params",
          class(data_and_params_cur_stratum))
      )

  }



  return(
    structure(
      stratumDataList,
      antigen_isos = antigen_isos,
      strata = strata,
      class = c("biomarker_data_and_params.list", "list")

    ))

}
