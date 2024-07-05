stratify_data <- function(
    data,
    antigen_isos,
    curve_params,
    noise_params,
    strata_varnames = "",
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL)
{

  no_strata = is.null(strata_varnames) || all(strata_varnames == "")

  if (no_strata) {
    pop_data <-
      data %>%
      select(
        data %>% get_value_var(),
        data %>% get_age_var(),
        data %>% get_biomarker_names_var()
      )

    all_data <-
      list(
        pop_data = pop_data,
        curve_params =
          curve_params %>% select(all_of(curve_param_names)),
        noise_params =
          noise_params %>% select(all_of(noise_param_names)),
        antigen_isos =
          antigen_isos %>% intersect(data %>% get_biomarker_names())
      ) %>%
      structure(class = union("biomarker_data_and_params", "list"))

    stratumataList <-
      # est.incidence.by() expects a list:
      list(`all data` = all_data) %>%
      structure(
        antigen_isos = antigen_isos, # might be able to remove
        strata = tibble(Stratum = NA)
      )

    return(stratumDataList)

  }

  # Make stratum variable (if needed)

  strata <- data %>% count_strata(strata_varnames)

  strata_vars_curve_params <-
    warn.missing.strata(
      data = curve_params,
      strata = strata %>% select(all_of(curve_strata_varnames)),
      dataname = "curve_params"
    )

  strata_vars_noise_params <-
    warn.missing.strata(
      data = noise_params,
      strata = strata %>% select(all_of(noise_strata_varnames)),
      dataname = "noise_params"
    )

  stratumDataList <- list()

  for (cur_stratum in strata$Stratum)
  {
    cur_stratum_vals <-
      strata %>% dplyr::filter(.data$Stratum == cur_stratum)

    pop_data_cur_stratum <-
      data %>%
      semi_join(
        cur_stratum_vals,
        by = strata_varnames
      ) %>%
      select(
        data %>% get_value_var(),
        data %>% get_age_var(),
        data %>% get_biomarker_names_var()
      )

    antigen_isos_cur_stratum =
       intersect(
        antigen_isos,
        pop_data_cur_stratum %>% get_biomarker_names())

    data_and_params_cur_stratum <-
      list(pop_data = pop_data_cur_stratum,
           antigen_isos = antigen_isos_cur_stratum)

    if (length(strata_vars_curve_params) == 0) {
      data_and_params_cur_stratum$curve_params <-
        curve_params %>% select("y1", "alpha", "r", "antigen_iso")
    } else {
      data_and_params_cur_stratum$curve_params <-
        curve_params %>%
        semi_join(
          cur_stratum_vals,
          by = strata_vars_curve_params
        ) %>%
        select("y1", "alpha", "r", "antigen_iso")
    }

    if (length(strata_vars_noise_params) == 0) {
      data_and_params_cur_stratum$noise_params <-
        noise_params %>%
        select("nu", "eps", "y.low", "y.high", "antigen_iso")
    } else {
      data_and_params_cur_stratum$noise_params <-
        noise_params %>%
        semi_join(
          cur_stratum_vals,
          by = strata_vars_noise_params
        ) %>%
        select("nu", "eps", "y.low", "y.high", "antigen_iso")
    }

    stratumDataList[[cur_stratum]] <-
      data_and_params_cur_stratum %>%
      structure(
        class = union(
          "biomarker_data_and_params",
          class(data_and_params_cur_stratum)
        )
      )
  }



  return(
    structure(
      stratumDataList,
      antigen_isos = antigen_isos,
      strata = strata,
      class = c("biomarker_data_and_params.list", "list")
    )
  )
}
