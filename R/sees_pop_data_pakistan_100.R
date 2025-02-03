#' Small example cross-sectional data set
#'
#' A subset of data from the SEES data, for examples and testing.
#'
#' @format ## `sees_pop_data_pk_100`
#' A `pop_data` object (from [as_pop_data()]) with 200 rows and 8 columns:
#' \describe{
#'   \item{id}{Observation ID}
#'   \item{Country}{Country where the participant was living}
#'   \item{cluster}{survey sampling cluster}
#'   \item{catchment}{survey catchment area}
#'   \item{age}{participant's age when sampled, in years}
#'   \item{antigen_iso}{which antigen and isotype are being measured
#'   (data is in long format)}
#'   \item{value}{concentration of antigen isotype, in ELISA units}
#' }
#' @source <https://osf.io/n6cp3>
"sees_pop_data_100"

#' Small example cross-sectional data set
#'
#' A subset of data from the SEES data, for examples and testing,
#' data from Pakistan only.
#'
#' @format ## `sees_pop_data_pk_100`
#' A `pop_data` object (from [as_pop_data()]) with 200 rows and 8 columns:
#' \describe{
#'   \item{id}{Observation ID}
#'   \item{Country}{Country where the participant was living}
#'   \item{cluster}{survey sampling cluster}
#'   \item{catchment}{survey catchment area}
#'   \item{age}{participant's age when sampled, in years}
#'   \item{antigen_iso}{which antigen and isotype are being measured
#'   (data is in long format)}
#'   \item{value}{concentration of antigen isotype, in ELISA units}
#' }
#' @source <https://osf.io/n6cp3>
"sees_pop_data_pk_100"

#' Small example cross-sectional data set
#'
#' A subset of data from the SEES data, for examples and testing,
#' data from Pakistan only, variable names not normalized by [as_pop_data()].
#'
#' @format ## `sees_pop_data_pk_100_old_names`
#' A `pop_data` object (from [as_pop_data()]) with 200 rows and 8 columns:
#' \describe{
#'   \item{index_id}{Observation ID}
#'   \item{Country}{Country where the participant was living}
#'   \item{cluster}{survey sampling cluster}
#'   \item{catchment}{survey catchment area}
#'   \item{Age}{participant's age when sampled, in years}
#'   \item{antigen_iso}{which antigen and isotype are being measured
#'   (data is in long format)}
#'   \item{result}{concentration of antigen isotype, in ELISA units}
#' }
#' @source <https://osf.io/n6cp3>
"sees_pop_data_pk_100_old_names"
