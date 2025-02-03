#' Small example curve parameters MCMC sample set
#'
#' A subset of MCMC samples for typhoid curve parameters
#'
#' @format ## `typhoid_curves_nostrat_100`
#' A `curve_params` object (from [as_curve_params()])
#' with 500 rows and 7 columns:
#' \describe{
#'   \item{antigen_iso}{which antigen and isotype are being measured
#'   (data is in long format)}
#'   \item{iter}{MCMC sampling iteration}
#'   \item{y0}{baseline antibody response}
#'   \item{y1}{peak antibody response}
#'   \item{t1}{elapsed time from baseline to peak antibody response}
#'   \item{alpha}{decay rate coefficient}
#'   \item{r}{decay power function exponent}
#' }
#' @source <https://osf.io/rtw5k/>
"typhoid_curves_nostrat_100"
