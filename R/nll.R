#' Calculate log-likelihood
#'
#' @param `log(lambda)` natural logarithm of incidence parameter, in log(years). Value of -6 corresponds roughly to 1 day (log(1/365.25)), -4 corresponds roughly to 1 week (log(7 / 365.25)). Default = -6.
#' @param data Data frame with cross-sectional serology data per antibody and age, and additional columns, for one stratum
#' @param antibodies Character vector with one or more antibody names. Values must match `data`.
#' @param lnparams List of data frames of all longitudinal parameters. Each data frame contains
#'   Monte Carlo samples for each antibody type.
#' @param noise_params a [list()] (or [data.frame()], or [tibble()]) containing noise parameters
#'
#' @return the log-likelihood of the data with the current parameter values
.nll <- function(
    `log(lambda)`,
    data,
    antibodies,
    lnparams,
    noise_params)
{
  # Start with zero total
  nllTotal <- 0

  # Loop over antibodies
  for (cur_antibody in antibodies) {
    antibody <- .stripNames(cur_antibody)
    param <- lnparams[[antibody]]

    data <- cbind(
      stratumData[[cur_antibody]],
      stratumData$Age)

    nllSingle <-
      fdev(
        log.lambda = `log(lambda)`,
        csdata = data,
        lnpars = param,
        cond = noise_params
      )

    if (!is.na(nllSingle)) {
      nllTotal <- nllTotal + nllSingle # DEM note: summing log likelihoods represents an independence assumption for multiple Antibodies, given time since seroconversion
    }

  }

  # Return total log-likelihood
  return(nllTotal)
}
