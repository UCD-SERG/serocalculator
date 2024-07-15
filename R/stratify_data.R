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
#' xs_data <- load_pop_data("https://osf.io/download//n6cp3/")
#'
#' curve <- load_curve_params("https://osf.io/download/rtw5k/") %>%
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) %>%
#'   slice(1:100, .by = antigen_iso) # Reduce dataset for the purposes of this example
#'
#' noise <- load_noise_params("https://osf.io/download//hqy4v/")
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
                          antigen_isos = data %>% attr("antigen_isos"),
                          curve_params,
                          noise_params,
                          strata_varnames = "",
                          curve_strata_varnames = NULL,
                          noise_strata_varnames = NULL) {
  no_strata = is.null(strata_varnames) || all(strata_varnames == "")
  if (no_strata) {
    all_data <-
      list(
        pop_data = data %>% select(all_of(
          c(
            data %>% get_value_var(),
            data %>% get_age_var(),
            "antigen_iso"
          )
        )),
        curve_params =
          curve_params %>%
          select(all_of(c(
            "y1", "alpha", "r", "antigen_iso"
          ))),
        noise_params =
          noise_params %>%
          select(all_of(
            c("nu", "eps", "y.low", "y.high", "antigen_iso")
          ))
      ) %>%
      structure(class = union("biomarker_data_and_params", "list"))

    stratumDataList <-
      list(# est.incidence.by() expects a list.
        `all data` = all_data) %>%
      structure(antigen_isos = antigen_isos, strata = tibble(Stratum = NA))


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

  # xs_dataStrata <- data %>% .makeStrata(strata_varnames)
  # curve_paramsStrata = curve_params %>% .makeStrata(strata_varnames)
  # noise_params_Strata = noise_params %>% .makeStrata(strata_varnames)
  # levelsStrata <- levels(xs_dataStrata$Stratum)

  stratumDataList <- list()

  for (cur_stratum in strata$Stratum)
  {
    cur_stratum_vals <-
      strata %>% dplyr::filter(.data$Stratum == cur_stratum)

    data_and_params_cur_stratum <-
      list(
        pop_data =
          data %>%
          semi_join(cur_stratum_vals, by = strata_varnames) %>%
          select(all_of(
            c(
              data %>% get_value_var(),
              data %>% get_age_var(),
              "antigen_iso"
            )
          ))
      )

    if (length(strata_vars_curve_params) == 0) {
      data_and_params_cur_stratum$curve_params <-
        curve_params %>% select(all_of(c("y1", "alpha", "r", "antigen_iso")))
    } else {
      data_and_params_cur_stratum$curve_params <-
        curve_params %>%
        semi_join(cur_stratum_vals, by = strata_vars_curve_params) %>%
        select(all_of(c("y1", "alpha", "r", "antigen_iso")))
    }

    if (length(strata_vars_noise_params) == 0) {
      data_and_params_cur_stratum$noise_params <-
        noise_params %>%
        select(all_of(c(
          "nu", "eps", "y.low", "y.high", "antigen_iso"
        )))
    } else {
      data_and_params_cur_stratum$noise_params <-
        noise_params %>%
        semi_join(cur_stratum_vals, by = strata_vars_noise_params) %>%
        select(all_of(c(
          "nu", "eps", "y.low", "y.high", "antigen_iso"
        )))
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
