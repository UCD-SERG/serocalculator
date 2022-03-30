#' Calculate log-likelihood
#'
#' @param stratumData
#' @param antibodies
#' @param params
#' @param censorLimits
#' @param ivc If `ivc = TRUE`, the biomarker data are interval-censored.
#' @param m
#' @param par0
#' @param start
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
