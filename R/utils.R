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

.errorCheck <- function(data, antibodies, strata, params)
{
  .checkAntibodies(antibodies = antibodies)
  .checkCsData(data = data, antibodies = antibodies)
  .checkStrata(data = data, strata = strata)
  .checkParams(antibodies = antibodies, params = params)

  invisible(NULL)
}

.checkAntibodies <- function(antibodies)
{
  stopifnot(!missing(antibodies))

  if (!is.character(antibodies)) {
    stop(.pasteN("Argument `antibodies` is not a character vector.",
                 "Provide a character vector with at least one antibody name."))
  }

  if (all(antibodies == "")) {
    stop(.pasteN("Argument `antibodies` is empty.",
                 "Provide a character vector with at least one antibody name."))
  }

  invisible(NULL)
}

.checkCsData <- function(data, antibodies)
{
  if (!is.data.frame(data)) {
    stop(.pasteN("Argument `data` is not a `data.frame()`.",
                 "Provide a `data.frame()` with cross-sectional serology data per antibody."))
  }

  if (!is.element("a", names(data))) {
    stop("Argument `data` is missing column `a` (age, in years).")
  }

  invisible(NULL)
}

.checkParams <- function(antibodies, params)
{

  message1 = paste(
    "Please provide a `data.frame()` containing Monte Carlo samples of the longitudinal parameters",
    "`y1`, `alpha`, and `r`",
    "for each value of `antigen_iso` in `data`")


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

  if (!all(antibodies %in% params$antigen_iso)) {
    stop("Some `antigen_iso` values are missing.")
  }

  invisible(NULL)
}

.checkStrata <- function(data, strata) {
  if (!is.character(strata)) {
    stop(.pasteN("Argument `strata` is not a character vector.",
                 "Provide a character vector with names of stratifying variables."))
  }

  if (!all(is.element(strata, union("", names(data))))) {
    stop("Strata names in argument \"data\" and argument \"strata\" do not match.")
  }

  invisible(NULL)
}




