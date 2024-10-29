#' Calculate negative log-likelihood (deviance)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `fdev()` was renamed to [f_dev()] to create a more
#' consistent API.
#' @keywords internal
#' @export
fdev <- function(lambda, csdata, lnpars, cond)
{
  lifecycle::deprecate_warn("1.0.0", "fdev()", "f_dev()")
  f_dev(lambda, csdata, lnpars, cond)
}

#' @title Calculate negative log-likelihood (deviance) for one antigen:isotype pair and incidence rate
#'
#' @details interface with C lib `serocalc.so`
#' @param lambda [numeric()] incidence parameter, in events per person-year
#' @param csdata cross-sectional sample data containing variables `value` and `age`
#' @param lnpars longitudinal antibody decay model parameters `alpha`, `y1`, and `d`
#' @param cond measurement noise parameters `nu`, `eps`, `y.low`, and `y.high`
#' @returns a [numeric()] negative log-likelihood,
#' corresponding to input `lambda`
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' # load in longitudinal parameters
#' curve_params = load_curve_params("https://osf.io/download/rtw5k")
#' xs_data <- "https://osf.io/download//n6cp3/" |>
#' load_pop_data()
#'
#' #Load noise params
#' noise_params <- tibble(
#'   antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
#'   nu = c(0.5, 0.5),                          # Biologic noise (nu)
#'   eps = c(0, 0),                             # M noise (eps)
#'   y.low = c(1, 1),                           # low cutoff (llod)
#'   y.high = c(5e6, 5e6))                      # high cutoff (y.high)
#'
#' cur_antibody = "HlyE_IgA"
#'
#' cur_data <-
#'   xs_data |>
#'   dplyr::filter(
#'    .data$catchment == "dhaka",
#'    .data$antigen_iso == cur_antibody) |>
#'   dplyr::slice_head(n = 100)
#'
#' cur_curve_params <-
#'   curve_params |>
#'   dplyr::filter(.data$antigen_iso == cur_antibody) |>
#'   dplyr::slice_head(n = 100)
#'
#' cur_noise_params <-
#'   noise_params |>
#'   dplyr::filter(.data$antigen_iso == cur_antibody)
#'
#' if(!is.element('d', names(cur_curve_params)))
#' {
#'   cur_curve_params <-
#'     cur_curve_params |>
#'     dplyr::mutate(
#'       alpha = .data$alpha * 365.25,
#'       d = .data$r - 1)
#' }
#'
#' lambda = 0.1
#' f_dev0(
#'     lambda = lambda,
#'     csdata = cur_data,
#'     lnpars = cur_curve_params,
#'     cond = cur_noise_params
#'   )
f_dev0 <- function(
      lambda,
      csdata,
      lnpars,
      cond) {
    res <- 0
    lambda <- as.double(lambda)
    y <- as.double(get_value(csdata))
    a <- as.double(get_age(csdata))
    nsubj <- as.integer(nrow(csdata))
    y1 <- as.double(lnpars$y1)
    alpha <- as.double(lnpars$alpha)
    d <- as.double(lnpars$d)
    nmc <- as.integer(length(y1))
    step <- as.double(max(y1) / 100) # hack for numerical integrations
    nu <- as.double(cond$nu)
    eps <- as.double(cond$eps)
    y.low <- as.double(cond$y.low)
    y.high <- as.double(cond$y.high)
    llpp <- .C(
      "negloglik",
      res = as.double(res),
      lambda = lambda,
      y = y,
      a = a,
      nsubj = nsubj,
      nu = nu,
      eps = eps,
      step = step,
      y.low = y.low,
      y.high = y.high,
      y1 = y1,
      alpha = alpha,
      d = d,
      nmc = nmc
    )
    return(llpp$res)
  }

#' @title Calculate negative log-likelihood (deviance) for one antigen:isotype pair and several values of incidence
#'
#' @description Calculates negative log-likelihood (deviance) for one antigen:isotype pair and several values of incidence (`lambda`).
#' @details Vectorized version of [f_dev0()]; interface with C lib `serocalc.so`
#' @param lambda a [numeric] vector of incidence parameters, in events per person-year
#' @inheritParams f_dev
#' @returns a [numeric] vector of negative log-likelihoods,
#' corresponding to the elements of input `lambda`
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tibble)
#'
#' # load in longitudinal parameters
#' curve_params = load_curve_params("https://osf.io/download/rtw5k")
#' xs_data <- "https://osf.io/download//n6cp3/" |>
#' load_pop_data()
#'
#' #Load noise params
#' noise_params <- tibble(
#'   antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
#'   nu = c(0.5, 0.5),                          # Biologic noise (nu)
#'   eps = c(0, 0),                             # M noise (eps)
#'   y.low = c(1, 1),                           # low cutoff (llod)
#'   y.high = c(5e6, 5e6))                      # high cutoff (y.high)
#'
#' cur_antibody = "HlyE_IgA"
#'
#' cur_data =
#'   xs_data |>
#'   dplyr::filter(
#'    .data$catchment == "dhaka",
#'    .data$antigen_iso == cur_antibody) |>
#'   dplyr::slice_head(n = 100)
#'
#' cur_curve_params =
#'   curve_params |>
#'   dplyr::filter(.data$antigen_iso == cur_antibody) |>
#'   dplyr::slice_head(n = 100)
#'
#' cur_noise_params =
#'   noise_params |>
#'   dplyr::filter(.data$antigen_iso == cur_antibody)
#'
#' if(!is.element('d', names(cur_curve_params)))
#' {
#'   cur_curve_params =
#'     cur_curve_params |>
#'     dplyr::mutate(
#'       alpha = .data$alpha * 365.25,
#'       d = .data$r - 1)
#' }
#'
#' lambdas = seq(.1, .2, by = .01)
#' f_dev(
#'     lambda = lambdas,
#'     csdata = cur_data,
#'     lnpars = cur_curve_params,
#'     cond = cur_noise_params
#'   )
#' }
#' @export
#' @keywords internal
f_dev <- Vectorize(FUN = f_dev0, vectorize.args = "lambda")
