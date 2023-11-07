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

  if (!is.element("Age", names(data))) {
    stop("Argument \"data\" is missing column \"Age\".")
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
    stop(.pasteN("Argument \"strata\" is not a character vector.",
                 "Provide a character vector with names of stratifying variables."))
  }

  if (!all(is.element(strata, union("", names(data))))) {
    stop("Strata names in argument \"data\" and argument \"strata\" do not match.")
  }

  invisible(NULL)
}

.prepData <- function(data, antibodies, lnparams, noise_params, strata = "")
{

  # Make stratum variable (if needed)
  xs_dataStrata <- data |> .makeStrata(strata)
  lnparamsStrata = lnparams |> .makeStrata(strata)
  noise_params_Strata = noise_params |> .makeStrata(strata)
  levelsStrata <- levels(xs_dataStrata$Stratum)

  stratumDataList = list()

  for (cur_stratum in levelsStrata)
  {
    stratumDataList[[cur_stratum]] =
      list(
        data =
          xs_dataStrata |>
          filter(Stratum == cur_stratum)
      )

    if("Stratum" %in% names(lnparamsStrata))
    {
      stratumDataList[[cur_stratum]]$lnparams =
        lnparamsStrata |> filter(Stratum == cur_stratum)

    } else
    {
      stratumDataList[[cur_stratum]]$lnparams = lnparams
    }

    if("Stratum" %in% names(noise_params_Strata))
    {
      stratumDataList[[cur_stratum]]$noise_params =
        noise_params_Strata |> filter(Stratum == cur_stratum)

    } else
    {
      stratumDataList[[cur_stratum]]$noise_params = noise_params
    }

  }

  return(
    structure(
      stratumDataList,
      Antibodies = antibodies,
      StratumLevels = levelsStrata
    ))

}

.makeStrata <- function(data, strata = "")
{
  dataStrata <- data

  if (all(strata != ""))
  {
    if(all(strata %in% names(data)))
    {
      dataStrata$Stratum <- interaction(dataStrata[, strata])
    } else
    {
      return(dataStrata) # no stratum variable
    }
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
