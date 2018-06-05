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
    stop(.pasteN("Argument \"antibodies\" is not a character vector.",
                 "Provide a character vector with at least one antibody name."))
  }

  if (all(antibodies == "")) {
    stop(.pasteN("Argument \"antibodies\" is empty.",
                 "Provide a character vector with at least one antibody name."))
  }

  invisible(NULL)
}

.checkCsData <- function(data, antibodies)
{
  if (!is.data.frame(data)) {
    stop(.pasteN("Argument \"data\" is not a dataframe.",
                 "Provide a dataframe with cross-sectional serology data per antibody."))
  }

  if (!is.element("Age", names(data))) {
    stop("Argument \"data\" is missing column \"Age\".")
  }

  if (!all(is.element(.checkIvc(data, antibodies)$Antibodies, names(data)))) {
    stop("Antibody names in argument \"data\" and argument \"antibodies\" do not match.")
  }

  invisible(NULL)
}

.checkIvc <- function(data, antibodies) {
  # Assume we are not dealing with interval censored data
  ivc <- FALSE
  if (length(intersect(names(data), antibodies)) != length(antibodies)) {
    # Add .lo and .hi to ab names
    antibodies <- .appendNames(antibodies)
    # We are dealing with interval censored data
    ivc <- TRUE
  }
  return(list(Ivc = ivc, Antibodies = antibodies))
}

.checkParams <- function(antibodies, params) {
  if (!is.list(params)) {
    stop(.pasteN("Argument \"params\" is not a list of dataframes.",
                 "Provide a list of three dataframes with names of the antibodies to be tested.",
                 "Each of the dataframes should contain a Monte Carlo sample of the longitudinal",
                 "parameters named y1, alpha, r, y0, mu1 and t1."))
  }

  if (!all(sapply(params, is.data.frame))) {
    stop(.pasteN("Argument \"params\" is not a list of dataframes.",
                 "Provide a list of three dataframes with names of the antibodies to be tested.",
                 "Each of the dataframes should contain a Monte Carlo sample of the longitudinal",
                 "parameters named y1, alpha, r, y0, mu1 and t1."))
  }

  if (!all(is.element(names(params[[1]]),
                      c("y1", "alpha", "yb", "r", "y0", "mu1", "t1")))) {
    stop(.pasteN("The parameter names do not match.",
                 "Provide a list of three dataframes with names of the antibodies to be tested.",
                 "Each of the dataframes should contain a Monte Carlo sample of the longitudinal",
                 "parameters named y1, alpha, r, y0, mu1 and t1."))
  }

  if (length(params[[1]]$y0) != length(params[[1]]$alpha) |
      length(params[[1]]$y0) != length(params[[1]]$yb) |
      length(params[[1]]$y0) != length(params[[1]]$r) |
      length(params[[1]]$y0) != length(params[[1]]$y1) |
      length(params[[1]]$y0) != length(params[[1]]$mu1) |
      length(params[[1]]$y0) != length(params[[1]]$t1)) {
    stop("The parameter lists \"params\" are of different length.")
  }

  if (!all(is.element(antibodies, names(params)))) {
    stop("Antibody names in argument \"antibodies\" and argument \"params\" do not match.")
  }

  invisible(NULL)
}

.checkStrata <- function(data, strata) {
  if (!is.character(strata)) {
    stop(.pasteN("Argument \"strata\" is not a character vector.",
                 "Provide a character vector with strata names."))
  }

  if (!all(is.element(strata, union("", names(data))))) {
    stop("Strata names in argument \"data\" and argument \"strata\" do not match.")
  }

  invisible(NULL)
}

.prepData <- function(data, antibodies, strata = "")
{
  ivcAb <- .checkIvc(data, antibodies)

  # Make stratum variable (if needed)
  dataStrata <- .makeStrata(data, strata)
  levelsStrata <- levels(dataStrata$Stratum)
  return(list(Ivc = ivcAb$Ivc,
              Antibodies = ivcAb$Antibodies,
              Levels = levelsStrata,
              Data = dataStrata))
}

.makeStrata <- function(data, strata = "")
{
  dataStrata <- data

  if (all(strata != "")) {
    dataStrata$Stratum <- interaction(dataStrata[, strata])
  } else {
    dataStrata$Stratum <- factor(1)
  }
  return(dataStrata)
}

.selectModel <- function(param)
{
  model1 <- any(param$r == 1 & param$yb == 0 & param$t1 == 0)
  model2 <- any(param$r != 1 & param$yb == 0 & param$t1 == 0)
  model3 <- any(param$r == 1 & !is.na(param$y0))
  model4 <- any(param$r != 1 & !is.na(param$y0))
  model5 <- any(param$r == 1 & param$yb != 0 & param$t1 == 0)
  res <- which(c(model1, model2, model3, model4, model5))
  if (length(res) == 0) {
    res <- 0
  }
  return(res)
}
