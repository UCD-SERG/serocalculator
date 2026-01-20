.paste_n <- function(...) {
  paste(..., sep = "\n")
}

.append_names <- function(ab_names) {
  res <- c()
  for (k in seq_along(ab_names)) {
    res <- c(
      res,
      paste0(ab_names[k], ".lo"),
      paste0(ab_names[k], ".hi")
    )
  }
  return(res)
}

.strip_names <- function(ab_names) {
  if (grepl(".", ab_names, fixed = TRUE)) {
    return(substr(ab_names, 1, nchar(ab_names) - 3))
  }

  return(ab_names)
}

.error_check <- function(data, antigen_isos, curve_params) {
  .check_antibodies(pop_data = data, antigen_isos = antigen_isos)
  check_pop_data(pop_data = data)
  .check_params(antigen_isos = antigen_isos, params = curve_params)

  invisible(NULL)
}

.check_antibodies <- function(
    pop_data,
    antigen_isos = pop_data |>  attr("antigen_isos")) {

  if (!is.character(antigen_isos) && !is.factor(antigen_isos)) {
    cli::cli_abort(
      c(
        "In `est_seroincidence()`, the argument `antigen_isos` should be a ",
        "`character()` or `factor()` variable, but ",
        'currently, `class(antigen_isos) == "{class(antigen_isos)}"`.',
        "Please provide a character vector with at least one antibody name."
      )
    )
  }

  if (setequal(antigen_isos, "")) {
    cli::cli_abort(
      c(
        "Argument `antigen_isos` is empty.",
        "Provide a character vector with at least one antibody name."
      )
    )
  }

  missing_ais <-
    antigen_isos |>
    setdiff(pop_data |>  get_biomarker_names())

  if (length(missing_ais) != 0) {
    cli::cli_inform(
      c(
        "`pop_data` has no observations for the following ",
        "{pop_data |> get_biomarker_names_var()}s: ",
        "{paste(missing_ais, collapse = ', ')}"
      ),
      class = "missing_biomarker"
    )
  }

  invisible(NULL)
}

.check_params <- function(antigen_isos, params) {
  message1 <- paste(
    "Please provide a `data.frame()` containing Monte Carlo samples",
    "of the longitudinal parameters `y1`, `alpha`, and `r`",
    "for each value of `antigen_iso` in `pop_data`"
  )

  if (!is.data.frame(params)) {
    cli::cli_abort(
      c(
        "Argument `params` is not a `data.frame()`.",
        message1
      )
    )
  }

  if (!all(c("y1", "alpha", "r") %in% names(params))) {
    cli::cli_abort(
      c(
        "The parameter names do not match.",
        message1
      )
    )
  }

  if (!all(antigen_isos %in% params$antigen_iso)) {
    cli::cli_abort("Some `antigen_iso` values are missing.")
  }

  invisible(NULL)
}
