.pasteN <- function(...) {
  paste(..., sep = "\n")
}

.appendNames <- function(abNames) {
  res <- c()
  for (k in seq_along(abNames)) {
    res <- c(
      res,
      paste0(abNames[k], ".lo"),
      paste0(abNames[k], ".hi")
    )
  }
  return(res)
}

.stripNames <- function(abNames) {
  if (grepl(".", abNames, fixed = TRUE)) {
    return(substr(abNames, 1, nchar(abNames) - 3))
  }

  return(abNames)
}

.errorCheck <- function(data, antigen_isos, curve_params) {
  .checkAntibodies(pop_data = data, antigen_isos = antigen_isos)
  check_pop_data(pop_data = data)
  .checkParams(antigen_isos = antigen_isos, params = curve_params)

  invisible(NULL)
}

.checkAntibodies <- function(pop_data,
                             antigen_isos = pop_data |>  attr("antigen_isos")) {

  if (!is.character(antigen_isos) && !is.factor(antigen_isos)) {
    stop(
      paste0(
        "In `est_seroincidence()`, the argument `antigen_isos` should be a ",
        "`character()` or `factor()` variable, but ",
        'currently, `class(antigen_isos) == "',
        class(antigen_isos),
        '"`.',
        "\nPlease provide a character vector with at least one antibody name."
      )
    )
  }

  if (setequal(antigen_isos, "")) {
    stop(
      .pasteN(
        "Argument `antigen_isos` is empty.",
        "Provide a character vector with at least one antibody name."
      )
    )
  }

  missing_AIs <-
    antigen_isos |>
    setdiff(pop_data |>  get_biomarker_names())

  if (length(missing_AIs) != 0) {
    message = "`pop_data` has no observations for the following
    {pop_data %>% get_biomarker_names_var()}s: {missing_AIs}"

    cli::cli_warn(message = message, class = "missing_biomarker")
  }

  invisible(NULL)
}

.checkParams <- function(antigen_isos, params) {
  message1 <- paste(
    "Please provide a `data.frame()` containing Monte Carlo samples",
    "of the longitudinal parameters `y1`, `alpha`, and `r`",
    "for each value of `antigen_iso` in `pop_data`"
  )

  if (!is.data.frame(params)) {
    stop(
      .pasteN(
        "Argument `params` is not a `data.frame()`.",
        message1
      )
    )
  }

  if (!all(c("y1", "alpha", "r") %in% names(params))) {
    stop(
      .pasteN(
        "The parameter names do not match.",
        message1
      )
    )
  }

  if (!all(antigen_isos %in% params$antigen_iso)) {
    stop("Some `antigen_iso` values are missing.")
  }

  invisible(NULL)
}
