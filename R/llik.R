#' Calculate log-likelihood
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `llik()` was renamed to `log_likelihood()` to create a more
#' consistent API.
#'
#' @keywords internal
#' @export
#'
llik <- function(
    lambda,
    pop_data,
    antigen_isos,
    curve_params,
    noise_params,
    verbose = FALSE,
    ...)
{
  lifecycle::deprecate_warn("1.0.0", "llik()", "log_likelihood()")
  log_likelihood(lambda, pop_data, antigen_isos, curve_params, noise_params, verbose = FALSE)


}

#' Calculate log-likelihood
#'
#' @description
#' Calculates the log-likelihood of a set of cross-sectional antibody response data, for a given incidence rate (`lambda`) value.
#' @param pop_data a [data.frame()] with cross-sectional serology data per antibody and age, and additional columns
#' @param antigen_isos Character vector listing one or more antigen isotypes. Values must match `pop_data`.
#' @param curve_params a [data.frame()] containing MCMC samples of parameters from the Bayesian posterior distribution of a longitudinal decay curve model. The parameter columns must be named:
#' - `antigen_iso`: a [character()] vector indicating antigen-isotype combinations
#' - `iter`: an [integer()] vector indicating MCMC sampling iterations
#' - `y0`: baseline antibody level at \eqn{t=0} (\eqn{y(t=0)})
#' - `y1`: antibody peak level (ELISA units)
#' - `t1`: duration of infection
#' - `alpha`: antibody decay rate (1/days for the current longitudinal parameter sets)
#' - `r`: shape factor of antibody decay
#' @param noise_params a [data.frame()] (or [tibble::tibble()]) containing the following variables, specifying noise parameters for each antigen isotype:
#' * `antigen_iso`: antigen isotype whose noise parameters are being specified on each row
#' * `nu`: biological noise
#' * `eps`: measurement noise
#' * `y.low`: lower limit of detection for the current antigen isotype
#' * `y.high`: upper limit of detection for the current antigen isotype
#' @param verbose logical: if TRUE, print verbose log information to console
#' @param ... additional arguments passed to other functions (not currently used).
#' @inheritParams f_dev
#' @return the log-likelihood of the data with the current parameter values
#' @export
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' #load in longitudinal parameters
#' dmcmc = load_curve_params("https://osf.io/download/rtw5k")
#'  \donttest{
#' xs_data <- "https://osf.io/download//n6cp3/" %>%
#' load_pop_data() %>%
#' clean_pop_data()
#'
#' #Load noise params
#'   cond <- tibble(
#'   antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
#'   nu = c(0.5, 0.5),                          # Biologic noise (nu)
#'   eps = c(0, 0),                             # M noise (eps)
#'   y.low = c(1, 1),                           # low cutoff (llod)
#'   y.high = c(5e6, 5e6))                      # high cutoff (y.high)
#'
#' #Calculate log-likelihood
#'   ll_AG = log_likelihood(
#'   pop_data = xs_data,
#'   curve_params = dmcmc,
#'   noise_params = cond,
#'   antigen_isos = c("HlyE_IgG","HlyE_IgA"),
#'   lambda = 0.1) %>% print()
#' }
log_likelihood <- function(
    lambda,
    pop_data,
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
    if(!is.data.frame(pop_data))
    {
      cur_data = pop_data[[cur_antibody]]
      cur_curve_params = curve_params[[cur_antibody]]
      cur_noise_params = noise_params[[cur_antibody]]
    } else
    {
      cur_data =
        pop_data %>%
        dplyr::filter(.data$antigen_iso == cur_antibody)

      cur_curve_params =
        curve_params %>%
        dplyr::filter(.data$antigen_iso == cur_antibody)

      cur_noise_params =
        noise_params %>%
        dplyr::filter(.data$antigen_iso == cur_antibody)

      if(!is.element('d', names(cur_curve_params)))
      {
        cur_curve_params =
          cur_curve_params %>%
          dplyr::mutate(
            alpha = .data$alpha * 365.25,
            d = .data$r - 1)
      }
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
}
