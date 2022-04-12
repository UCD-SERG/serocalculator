#' Calculate log-likelihood
#'
#' @param stratumData Data frame with cross-sectional serology data per antibody and age, and additional
#'   columns, for one stratum
#' @param antibodies Character vector with one or more antibody names. Values must match \code{data}.
#' @param params List of data frames of all longitudinal parameters. Each data frame contains
#'   Monte Carlo samples for each antibody type.
#' @param censorLimits List of cutoffs for one or more named antibody types (corresponding to
#'   \code{stratumData}).
#' @param ivc If `ivc = TRUE`, the biomarker data are interval-censored.
#' @param m [not sure what this is]
#' @param par0 List of parameters for the (lognormal) distribution of antibody concentrations
#'   for true seronegatives (i.e. those who never seroconverted), by named antibody type
#'   (corresponding to \code{data}).
#' @param start  starting value for \code{log(lambda)}. Value of -6 corresponds roughly to 1 day
#'   (log(1/365.25)), -4 corresponds roughly to 1 week (log(7 / 365.25)). Default = -6.
#'
#' @return
.nll <- function(stratumData, antibodies, params, censorLimits, ivc = FALSE, m = 0, par0, start)
{
  # Start with zero total
  nllTotal <- 0
  if (ivc) {
    # Loop over antibodies
    for (abIdx in seq_along(antibodies)) {
      antibody <- .stripNames(antibodies[abIdx])
      param <- params[[antibody]]
      p0 <- par0[[antibody]]
      modelType <- .selectModel(param)

      data <- cbind(stratumData[[antibodies[abIdx]]],
                    stratumData[[antibodies[abIdx + 1]]],
                    stratumData$Age)

      nllSingle <- .nllByType(data = data, param = param, censorLimit = NULL, ivc = ivc, m = m,
                              par0 = p0, start = start, modelType = modelType)
      if (!is.na(nllSingle)) {
        nllTotal <- nllTotal + nllSingle
      }
    }
  } else {
    # Loop over antibodies
    for (abIdx in seq_along(antibodies)) {
      antibody <- .stripNames(antibodies[abIdx])
      param <- params[[antibody]]
      p0 <- par0[[antibody]]
      censorLimit <- censorLimits[[antibody]]
      modelType <- .selectModel(param)

      data <- cbind(stratumData[[antibodies[abIdx]]],
                    stratumData$Age)
      nllSingle <- .nllByType(data = data, param = param, censorLimit = censorLimit, ivc = ivc,
                              m = m, par0 = p0, start = start, modelType = modelType)
      if (!is.na(nllSingle)) {
        nllTotal <- nllTotal + nllSingle # DEM note: summing log likelihoods represents an independence assumption for multiple Antibodies, given time since seroconversion
      }
    }
  }

  # Return total log-likelihood
  return(nllTotal)
}
