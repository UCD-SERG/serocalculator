#' @title Check a `pop_data` object for requested strata variables
#'
#' @param pop_data a `pop_data` object
#' @param biomarker_names_var name of column in `pop_data` indicating
#' biomarker type
#' @param strata a [character] vector
#'
#' @returns [NULL], invisibly
#' @examples
#' sees_pop_data_pk_100 |>
#'   check_strata(strata = c("ag", "catch", "Count")) |>
#'   try()
#' @dev
check_strata <- function(pop_data,
                         strata,
                         biomarker_names_var =
                           get_biomarker_names_var(pop_data)) {
  if (!is.character(strata)) {
    cli::cli_abort(
      class = "strata are not strings",
      message = c(
        "x" = "Argument `strata` is not a character vector.",
        "i" = "Provide a character vector with names of stratifying variables."
      )
    )
  }

  present_strata_vars <- intersect(strata, names(pop_data))
  missing_strata_vars <- setdiff(strata, present_strata_vars)

  if (length(missing_strata_vars) > 0) {
    message0 <- c(
      "Can't stratify provided {.arg pop_data}
       with the provided {.arg strata}:",
      "x" = "variable {.var {missing_strata_vars}}
             {?is/are} missing in {.arg pop_data}."
    )

    f1 <- function(x) {
      temp <- stringr::str_subset(string = names(pop_data), pattern = x)

      if (length(temp) > 0) {
        temp <- temp |>
          glue::backtick() |>
          and::or()
      } else {
        temp = character()
      }

      return(temp)
    }

    partial_matches <-
      purrr::map(missing_strata_vars, f1) |>
      rlang::set_names(missing_strata_vars) |>
      purrr::keep(~ length(.x) > 0)

    inputs_with_partial_matches <- names(partial_matches) # nolint: object_usage_linter

    if (length(partial_matches) > 0) {
      partial_matches <-
        glue::glue("\"{names(partial_matches)}\": {partial_matches}")

      message0 <- c(
        message0,
        "i" = "The following input{?s} to {.arg strata}
                  might be misspelled:
                  {.str {inputs_with_partial_matches}}",
        "i" = "Did you mean:",
        partial_matches |> rlang::set_names("*")
      )
    }

    cli::cli_abort(
      class = "missing_var",
      call = rlang::caller_env(),
      message = message0
    )
  }

  antigen_iso_counts <-
    pop_data |>
    dplyr::select(all_of(c(strata, biomarker_names_var)))  |>
    table()

  if (any(antigen_iso_counts == 0L)) {
    cli::cli_warn(
      class = "strata missing some biomarkers",
      message =
        "Some strata are completely missing one or more biomarkers.",
      body = antigen_iso_counts |> capture.output()
    )
  }

  invisible(NULL)
}
