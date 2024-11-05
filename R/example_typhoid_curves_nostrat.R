#' Small example of antibody response curve parameters for typhoid
#'
#' A subset of data from the SEES study, for examples and testing.
#'
#' @format ## `typhoid_curves_nostrat_100`
#' A `curve_params` object (from [as_curve()]) with 500 rows and 7 columns:
#' \describe{
#'   \item{antigen_iso}{which antigen and isotype are being measured
#'   (data is in long format)}
#'   \item{iter}{MCMC iteration}
#'   \item{y0}{Antibody concentration at t = 0 (start of active infection)}
#'   \item{y1}{Antibody concentration at t = `t1` (end of active infection)}
#'   \item{t1}{Duration of active infection}
#'   \item{alpha}{Antibody decay rate coefficient}
#'   \item{r}{Antibody decay rate exponent parameter}
#' }
#' @source <https://osf.io/rtw5k>
"typhoid_curves_nostrat_100"

