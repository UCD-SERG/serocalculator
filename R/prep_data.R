prep_data <- function(
    data,
    antibodies,
    curve_params,
    noise_params,
    strata_varnames = "",
    strata_curves = NULL,
    strata_noise = NULL)
{

  if(is.null(strata_varnames) || all(strata_varnames == ""))
  {
    stratumDataList =
      list(# est.incidence.by() expects a list.
        `all data` =
          list(
            data = data |> select("y", "a"),
            curve_params = curve_params |> select("y1", "alpha", "d"),
            noise_params = noise_params |> select("nu", "eps", "y.low", "y.high")
          )) |>
      structure(
        Antibodies = antibodies,
        strata = tibble(Stratum = NA)
      )


    return(stratumDataList)

  }

  # Make stratum variable (if needed)

  strata = data |> get_strata(strata_varnames)

  strata_vars_curve_params =
    warn_missing_strata(
    data = curve_params,
    strata = strata_curves,
    dataname = "curve_params"
  )

  strata_vars_noise_params =
    warn_missing_strata(
    data = noise_params,
    strata = strata_noise,
    dataname = "noise_params"
  )

  # xs_dataStrata <- data |> .makeStrata(strata_varnames)
  # curve_paramsStrata = curve_params |> .makeStrata(strata_varnames)
  # noise_params_Strata = noise_params |> .makeStrata(strata_varnames)
  # levelsStrata <- levels(xs_dataStrata$Stratum)

  stratumDataList = list()

  for (cur_stratum in rownames(strata))
  {

    stratumDataList[[cur_stratum]] =
      list(
        data =
          data |>
          semi_join(
            strata[cur_stratum,, drop = FALSE],
            by = strata_varnames) |>
          select("y", "a")
      )

    if(length(strata_vars_curve_params) == 0)
    {
      stratumDataList[[cur_stratum]]$curve_params =
        curve_params |> select("y1", "alpha", "d")
    } else
    {
      stratumDataList[[cur_stratum]]$curve_params =
        curve_params |>
        semi_join(
          strata[cur_stratum,, drop = FALSE],
          by = strata_vars_curve_params) |>
        select("y1", "alpha", "d")
    }

    if(length(strata_vars_noise_params) == 0)
    {
      stratumDataList[[cur_stratum]]$noise_params =
        noise_params |> select("nu", "eps", "y.low", "y.high")
    } else
    {
      stratumDataList[[cur_stratum]]$noise_params =
        noise_params |>
        semi_join(
          strata[cur_stratum,, drop = FALSE],
          by = strata_vars_noise_params) |>
        select("nu", "eps", "y.low", "y.high")
    }

  }

  return(
    structure(
      stratumDataList,
      Antibodies = antibodies,
      strata = strata

    ))

}
