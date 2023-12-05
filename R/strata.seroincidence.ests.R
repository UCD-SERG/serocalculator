#' Extract strata from an object
#'
#' Generic method for extracting strata from objects. See [strata.default()]
#' @param x an object
#'
#' @return the strata of `x`
#'
strata <- function(x) {
  UseMethod("strata")
}

#' Extract the `Strata` attribute from an object, if present
#'
#' @param x any R object
#'
#' @return a [tibble::tibble()] with strata in rows, or `NULL` if `x` does not have a `"strata"` attribute
#' @export
#'
strata.default = function(x)
{
  attr(x, "Strata")
}

