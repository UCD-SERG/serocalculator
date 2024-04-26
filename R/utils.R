.pasteN <- function(...)
{
  paste(..., sep = "\n")
}

.appendNames <- function(abNames)
{
  res <- c()
  for (k in seq_len(length(abNames))) {
    res <- c(res,
             paste0(abNames[k], ".lo"),
             paste0(abNames[k], ".hi"))
  }
  return(res)
}

.stripNames <- function(abNames)
{
  if (grepl(".", abNames, fixed = TRUE)) {
    return(substr(abNames, 1, nchar(abNames) - 3))
  }

  return(abNames)
}

.errorCheck <- function(check_data, antigen_isos, curve_params)
{
  .checkAntibodies(antigen_isos = antigen_isos)
  check_pop_data(pop_data = check_data)
  .checkParams(antigen_isos = antigen_isos, params = curve_params)

  invisible(NULL)
}

.checkAntibodies <- function(antigen_isos)
{

  if (!is.character(antigen_isos) && !is.factor(antigen_isos)) {
    stop(
      paste0(
        'In `est.incidence()`, the argument `antigen_isos` should be a `character()` or `factor()` variable, but ',
        'currently, `class(antigen_isos) == "', class(antigen_isos), '"`.',
        '\nPlease provide a character vector with at least one antibody name.'))
  }

  if (setequal(antigen_isos, "")) {
    stop(.pasteN("Argument `antigen_isos` is empty.",
                 "Provide a character vector with at least one antibody name."))
  }

  invisible(NULL)
}

.checkParams <- function(antigen_isos, params)
{

  message1 = paste(
    "Please provide a `data.frame()` containing Monte Carlo samples of the longitudinal parameters",
    "`y1`, `alpha`, and `r`",
    "for each value of `antigen_iso` in `pop_data`")

  if (!is.data.frame(params)) {
    stop(
      .pasteN(
        "Argument `params` is not a `data.frame()`.",
        message1))
  }

  if (!all(c("y1", "alpha", "r") %in% names(params)))
  {
    stop(
      .pasteN(
        "The parameter names do not match.",
        message1))
  }

  if (!all(antigen_isos %in% params$antigen_iso)) {
    stop("Some `antigen_iso` values are missing.")
  }

  invisible(NULL)
}

.checkStrata <- function(check_data, strata) {
  if (!is.character(strata)) {
    stop(.pasteN("Argument `strata` is not a character vector.",
                 "Provide a character vector with names of stratifying variables."))
  }

  if (!all(is.element(strata, union("", names(check_data))))) {
    stop("Strata names in argument \"data\" and argument \"strata\" do not match.")
  }

  invisible(NULL)
}




