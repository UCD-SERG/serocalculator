#' @title Small example of noise parameters for typhoid
#'
#' @description A subset of noise parameter estimates from the SEES study,
#' for examples and testing, for Pakistan
#'
#' @format ## `example_noise_params_pk`
#' A `curve_params` object (from [as_sr_params()]) with 4 rows and 7 columns:
#' \describe{
#'   \item{antigen_iso}{which antigen and isotype are being measured
#'   (data is in long format)}
#'   \item{Country}{Location for which the noise parameters were estimated}
#'   \item{y.low}{Lower limit of detection}
#'   \item{eps}{Measurement noise: the bound on the relative measurement
#'   error. The relative error is modeled as uniform on the interval from
#'   `-eps` to `eps`, so `eps` is the largest relative deviation a
#'   measurement can have from its true value. Assay precision is often
#'   reported as a coefficient of variation (CV), the ratio of the standard
#'   deviation to the mean for replicates, ideally measured across plates
#'   rather than within the same plate; under this uniform model the CV
#'   equals `eps / sqrt(3)`, so a measured CV corresponds to
#'   `eps = sqrt(3) * CV`.}
#'   \item{nu}{Biological noise: error from
#'   cross-reactivity to other antibodies.
#'   It is defined as the 95th percentile of
#'   the distribution of antibody responses to the antigen-isotype
#'   in a population with no exposure.}
#'   \item{y.high}{Upper limit of detection}
#'   \item{Lab}{Lab for which noise was estimated.}
#' }
#' @source <https://osf.io/rtw5k>
"example_noise_params_pk"

#' @title Small example of noise parameters for typhoid
#'
#' @description A subset of noise parameter estimates from the SEES study,
#' for examples and testing.
#'
#' @format ## `example_noise_params_pk`
#' A `curve_params` object (from [as_sr_params()]) with 4 rows and 7 columns:
#' \describe{
#'   \item{antigen_iso}{which antigen and isotype are being measured
#'   (data is in long format)}
#'   \item{Country}{Location for which the noise parameters were estimated}
#'   \item{y.low}{Lower limit of detection}
#'   \item{eps}{Measurement noise: the bound on the relative measurement
#'   error. The relative error is modeled as uniform on the interval from
#'   `-eps` to `eps`, so `eps` is the largest relative deviation a
#'   measurement can have from its true value. Assay precision is often
#'   reported as a coefficient of variation (CV), the ratio of the standard
#'   deviation to the mean for replicates, ideally measured across plates
#'   rather than within the same plate; under this uniform model the CV
#'   equals `eps / sqrt(3)`, so a measured CV corresponds to
#'   `eps = sqrt(3) * CV`.}
#'   \item{nu}{Biological noise: error from
#'   cross-reactivity to other antibodies.
#'   It is defined as the 95th percentile of
#'   the distribution of antibody responses to the antigen-isotype
#'   in a population with no exposure.}
#'   \item{y.high}{Upper limit of detection}
#'   \item{Lab}{Lab for which noise was estimated.}
#' }
#' @source <https://osf.io/rtw5k>
"example_noise_params_sees"
