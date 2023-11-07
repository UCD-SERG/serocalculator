#' Calculate log-likelihood
#'
#' @param data Data frame with cross-sectional serology data per antibody and age, and additional columns
#' @param antibodies Character vector with one or more antibody names. Values must match `data`.
#' @param lnparams List of data frames of all longitudinal parameters. Each data frame contains
#'   Monte Carlo samples for each antibody type.
#' @param noise_params a [list()] (or [data.frame()], or [tibble()]) containing noise parameters
#' @inheritParams fdev
#' @return the log-likelihood of the data with the current parameter values
.nll <- function(
    log.lambda,
    data,
    antibodies,
    lnparams,
    noise_params)
{
  # Start with zero total
  nllTotal <- 0

  # Loop over antibodies
  for (cur_antibody in antibodies)
  {
    cur_data = data |> filter(antigen_iso == cur_antibody)
    cur_lnparams <- lnparams |> filter(antigen_iso == cur_antibody)
    cur_noise_params = noise_params |> filter(antigen_iso == cur_antibody)

    nllSingle <-
      fdev(
        log.lambda = log.lambda,
        csdata = cur_data,
        lnpars = cur_lnparams,
        cond = cur_noise_params
      )

    if (!is.na(nllSingle)) {
      nllTotal <- nllTotal + nllSingle # DEM note: summing log likelihoods represents an independence assumption for multiple Antibodies, given time since seroconversion
    }

  }

  # Return total log-likelihood
  return(nllTotal)
}
