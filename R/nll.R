#' Calculate log-likelihood
#'
#' @param data Data frame with cross-sectional serology data per antibody and age, and additional columns
#' @param antigen_isos Character vector with one or more antibody names. Values must match `data`.
#' @param curve_params List of data frames of all longitudinal parameters. Each data frame contains
#'   Monte Carlo samples for each antibody type.
#' @param noise_params a [list()] (or [data.frame()], or [tibble()]) containing noise parameters
#' @param ... additional arguments passed to other functions (not currently used).
#' @inheritParams fdev

#' @return the log-likelihood of the data with the current parameter values
.nll <- function(
    log.lambda,
    data,
    antigen_isos,
    curve_params,
    noise_params,
    ...)
{
  # Start with zero total
  nllTotal <- 0

  # Loop over antibodies
  for (cur_antibody in antigen_isos)
  {
    cur_data = data |> filter(.data[["antigen_iso"]] == cur_antibody)
    cur_curve_params <- curve_params |> filter(.data[["antigen_iso"]] == cur_antibody)
    cur_noise_params = noise_params |> filter(.data[["antigen_iso"]] == cur_antibody)

    nllSingle <-
      fdev(
        log.lambda = log.lambda,
        csdata = cur_data,
        lnpars = cur_curve_params,
        cond = cur_noise_params
      )

    if (!is.na(nllSingle)) {
      nllTotal <- nllTotal + nllSingle # DEM note: summing log likelihoods represents an independence assumption for multiple Antibodies, given time since seroconversion
    }

  }

  # Return total log-likelihood
  return(nllTotal)
}
