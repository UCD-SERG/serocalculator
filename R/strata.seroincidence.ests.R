#' Extract `Strata` metadata from an object
#'
#' Generic method for extracting strata metadata from objects.
#' See [strata.default()]
#' @param x an object
#' @export
#' @return the strata metadata of `x`
#'
strata <- function(x) {
  UseMethod("strata")
}

#' Extract the `Strata` attribute from an object, if present
#'
#' @param x any R object
#'
#' @return
#' * a [tibble::tibble()] with strata in rows, or
#' * `NULL` if `x` does not have a `"strata"` attribute
#' @export
#' @keywords internal
strata.default <- function(x) {
  attr(x, "Strata")
}
