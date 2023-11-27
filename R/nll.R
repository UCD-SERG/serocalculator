#' Calculate log-likelihood
#'
#' @details
#' Calculates the log-likelihood of a set of cross-sectional antibody response data, for a given incidence rate (`lambda`) value.
#'
#' @param data Data frame with cross-sectional serology data per antibody and age, and additional columns
#' @param antigen_isos Character vector listing one or more antigen isotypes. Values must match `data`.
#' @param curve_params List of data frames of all longitudinal parameters. Each data frame contains Monte Carlo samples for each antibody type.
#' @param noise_params a [data.frame()] (or [tibble::tibble()]) containing the following variables, specifying noise parameters for each antigen isotype:
#' * `antigen_iso`: antigen isotype whose noise parameters are being specified on each row
#' * `nu`: biological noise
#' * `eps`: measurement noise
#' * `y.low`: lower limit of detection for the current antigen isotype
#' * `y.high`: upper limit of detection for the current antigen isotype
#'
#' @param verbose logical: if TRUE, print verbose log information to console
#' @param ... additional arguments passed to other functions (not currently used).
#' @inheritParams fdev
#' @export
#' @return the log-likelihood of the data with the current parameter values
llik <- (
  # vectorize.args = "lambda",
  function(
    lambda,
    data,
    antigen_isos,
    curve_params,
    noise_params,
    verbose = FALSE,
    ...)
  {
    # Start with zero total
    nllTotal <- 0

    # Loop over antigen_isos
    for (cur_antibody in antigen_isos)
    {

      # the inputs can be lists, after `split(~antigen_ios)`
      # this gives some speedups compared to running filter() every time .nll() is called
      if(!is.data.frame(data))
      {
        cur_data = data[[cur_antibody]]
        cur_curve_params = curve_params[[cur_antibody]]
        cur_noise_params = noise_params[[cur_antibody]]
      } else
      {
        cur_data =
          data |> dplyr::filter(.data$antigen_iso == cur_antibody)

        cur_curve_params =
          curve_params |> dplyr::filter(.data$antigen_iso == cur_antibody)

        cur_noise_params =
          noise_params |> dplyr::filter(.data$antigen_iso == cur_antibody)
      }

      nllSingle <-
        fdev(
          lambda = lambda,
          csdata = cur_data,
          lnpars = cur_curve_params,
          cond = cur_noise_params
        )

      # if (!is.na(nllSingle))  # not meaningful for vectorized fdev()
      {
        nllTotal <- nllTotal + nllSingle # DEM note: summing log likelihoods represents an independence assumption for multiple Antibodies, given time since seroconversion
      }

    }

    # Return total log-likelihood
    return(-nllTotal)
  })

#' Calculate negative log-likelihood
#' @details
#' Same as [llik()], except negated
#' @param log.lambda log.lambda
#' @inheritDotParams llik -lambda

#' @return the negative log-likelihood of the data with the current parameter values
.nll = function(log.lambda, ...)
{
  -llik(lambda = exp(log.lambda),...)
}
