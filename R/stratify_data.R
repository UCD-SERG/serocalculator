#' @title Split data by stratum
#' @description Split biomarker data, decay curve parameters, and noise parameters
#' to prepare for stratified incidence estimation.
#' @param strata_varnames [character()] vector of names of variables in `data` to stratify by
#' @inheritParams est.incidence.by
#'
#' @returns a `"biomarker_data_and_params.list"` object (a [list] with extra attributes `"strata"`, `"antigen_isos"`, etc)
#' @keywords internal
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' xs_data <-
#'   sees_pop_data_pk_100
#'
#' curve <-
#'   typhoid_curves_nostrat_100 %>%
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))
#'
#' noise <-
#'   example_noise_params_pk
#'
#' stratified_data =
#'   stratify_data(
#'    data = xs_data,
#'    curve_params = curve,
#'    noise_params = noise,
#'    strata_varnames = "catchment",
#'    curve_strata_varnames = NULL,
#'    noise_strata_varnames = NULL
#'    )
#' }

stratify_data <- function(data,
                          curve_params,
                          noise_params,
                          strata_varnames = "",
                          curve_strata_varnames = NULL,
                          noise_strata_varnames = NULL,
                          antigen_isos = data %>% get_biomarker_levels()) {
  curve_params =
    curve_params %>%
    filter(.data[["antigen_iso"]] %in% antigen_isos)

  noise_params =
    noise_params %>%
    filter(.data[["antigen_iso"]] %in% antigen_isos)

  no_strata = is.null(strata_varnames) ||
    all(strata_varnames == "")

  if (no_strata) {
    pop_data <-
      data %>% select(all_of(
        c(
          data %>% get_value_var(),
          data %>% get_age_var(),
          data %>% get_biomarker_names_var()
        )
      ))

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

    # est.incidence.by() expects a list:
    stratumDataList <-
      list(`all data` = all_data) %>%
      structure(antigen_isos = antigen_isos, # might be able to remove
                strata = tibble(Stratum = NA))

    return(stratumDataList)

  }

  # Make stratum variable (if needed)

  strata <- data %>% count_strata(strata_varnames)

  strata_vars_curve_params <-
    warn_missing_strata(
      data = curve_params,
      strata = strata %>% select(all_of(curve_strata_varnames)),
      dataname = "curve_params"
    )

  strata_vars_noise_params <-
    warn_missing_strata(
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
      semi_join(cur_stratum_vals, by = strata_varnames) %>%
      select(
        data %>% get_value_var(),
        data %>% get_age_var(),
        data %>% get_biomarker_names_var()
      )

    antigen_isos_cur_stratum =
      intersect(antigen_isos,
                pop_data_cur_stratum %>% get_biomarker_names())

    data_and_params_cur_stratum <-
      list(pop_data = pop_data_cur_stratum, antigen_isos = antigen_isos_cur_stratum)

    if (length(strata_vars_curve_params) == 0) {
      data_and_params_cur_stratum$curve_params <-
        curve_params %>% select(all_of(curve_param_names))
    } else {
      data_and_params_cur_stratum$curve_params <-
        curve_params %>%
        semi_join(cur_stratum_vals, by = strata_vars_curve_params) %>%
        select(all_of(curve_param_names))
    }

    if (length(strata_vars_noise_params) == 0) {
      data_and_params_cur_stratum$noise_params <-
        noise_params %>%
        select(all_of(noise_param_names))
    } else {
      data_and_params_cur_stratum$noise_params <-
        noise_params %>%
        semi_join(cur_stratum_vals, by = strata_vars_noise_params) %>%
        select(all_of(noise_param_names))
    }

    stratumDataList[[cur_stratum]] <-
      data_and_params_cur_stratum %>%
      structure(class = union(
        "biomarker_data_and_params",
        class(data_and_params_cur_stratum)
      ))
  }

  return(structure(
    stratumDataList,
    antigen_isos = antigen_isos,
    strata = strata,
    class = c("biomarker_data_and_params.list", "list")
  ))
}
