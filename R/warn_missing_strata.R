#' Warn about missing stratifying variables in a dataset
#'
#' @param data the dataset that should contain the strata
#' @param strata a [data.frame()] showing the strata levels
#' that are expected to be in the dataset
#' @param dataname the name of the dataset,
#' for use in warning messages if some strata are missing.
#'
#' @return a [character()] vector of the subset of stratifying variables
#' that are present in `pop_data`
#'
#' @examples
#' \dontrun{
#' expected_strata <- data.frame(Species = "banana", type = "orchid")
#'
#' warn_missing_strata(iris, expected_strata, dataname = "iris")
#' }
#' @keywords internal
warn_missing_strata <- function(
    data,
    strata,
    dataname) {
  present_strata_vars <- intersect(names(strata), names(data))

  missing_strata_vars <- setdiff(names(strata), names(data))

  if (length(missing_strata_vars) > 0) {
    if (length(present_strata_vars) > 0) {
      message <- paste(
        "{.var {dataname}} is missing {.var {missing_strata_vars}}",
        "and will only be stratified by",
        "{.var {present_strata_vars}}"
      )
    } else {
      message <- paste(
        "{.var {dataname}} is missing all strata variables",
        "and will be used unstratified."
      )

    }

    message2 <- paste(
      "To avoid this warning,",
      "specify the desired set of stratifying variables",
      "in the `curve_strata_varnames` and",
      "`noise_strata_varnames` arguments to `estimate_scr_by()`."
    )


    cli::cli_warn(
      class = "missing strata vars",
      c(message, i = message2)
    )
  }

  if (length(present_strata_vars) > 0) {
    missing_strata <-
      anti_join(
        strata,
        data,
        by = present_strata_vars
      ) |>
      distinct(across(all_of(present_strata_vars)))

    if (nrow(missing_strata) > 0) {
      cli::cli_abort(
        class = "absent strata levels",
        c(
          "Missing strata levels in {.arg {dataname}}",
          "i" = "The following strata variables are present,
        but the following specific combinations of those strata are missing:"),
        body = missing_strata |> capture.output()
      )

    }
  }

  return(present_strata_vars)
}
