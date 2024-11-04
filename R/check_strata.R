#' @examples
#' sees_pop_data_pakistan_100_standardized |>
#    check_strata(strata = c("ag", "catch","Count"))
#'
check_strata <- function(pop_data, strata) {

  if (!is.character(strata)) {
    cli::cli_abort(c(
      "x" = "Argument `strata` is not a character vector.",
      "i" = "Provide a character vector with names of stratifying variables."
    ))
  }

  present_strata_vars = intersect(strata, names(pop_data))
  missing_strata_vars = setdiff(strata, present_strata_vars)

  if (length(missing_strata_vars) > 0) {

    partial_matches =
      purrr::map(
        missing_strata_vars,
        function(x)
          stringr::str_subset(
            string = names(pop_data),
            pattern = x
          )
      ) %>%
      rlang::set_names(missing_strata_vars) %>%
      purrr::keep(~ length(.x) > 0)

    # strata with no match
    no_match_vars <- missing_strata_vars %>%
      setdiff(names(partial_matches))

    f1 = function() {
      ul <- cli::cli_ul()
      for (i in names(partial_matches))
      {
        cli::cli_text("{.str i}:")
        cli::cli_ul(partial_matches[[i]])
      }
      cli::cli_end(ul)
    }

    message1 = function() {
      cli::cli_div()

      if (length(partial_matches) > 0) {
        cli::cli_bullets(
          c(
            "i" = "The following input{?s} to {.arg strata}
                  {?is/are} likely misspelled:
                  {.var {names(partial_matches)}}",
            "i" = "Did you mean:"
          ))
        f1()

      }

      cli::cli_end()

    }

    cli::cli_warn(
      class = "missing_var",
      call = rlang::caller_env(),
      message = c(
        "x" = "Can't stratify with the provided {.arg strata}:",
        "i" = "{.var {missing_strata_vars}} {?is/are} missing in {.arg pop_data}."),
      body = message1()
    )

  }

  invisible(NULL)
}
