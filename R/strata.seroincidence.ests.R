#' Extract strata from an object
#'
#' Generic method for extracting strata from objects. See [strata.seroincidence.ests()]
#' @param x an object
#'
#' @return the strata of `x`
#' @export
#'
strata <- function(x) {
  UseMethod("strata")
}

#' Extract information about strata from a `seroincidence.ests` object
#'
#' @param object a `seroincidence.ests` object (from [est.incidence.by()])
#'
#' @return a [dplyr::tibble()] with strata in rows
#' @export
#'
strata.seroincidence.ests = function(x)
{
  attr(x, "Strata")
}

