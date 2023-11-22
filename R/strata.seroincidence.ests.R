#' Extract strata from an object
#'
#' Generic method for extracting strata from objects. See [strata.seroincidence.by()]
#' @param x an object
#'
#' @return the strata of `x`
#' @export
#'
strata <- function(x) {
  UseMethod("strata")
}

#' Extract information about strata from a `seroincidence.by` object
#'
#' @param x a `seroincidence.by` object (from [est.incidence.by()])
#'
#' @return a [dplyr::tibble()] with strata in rows
#' @export
#'
strata.seroincidence.by = function(x)
{
  attr(x, "Strata")
}

