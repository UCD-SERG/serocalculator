#' Extract or replace parts of a `seroincidence.ests` object
#'
#' @inheritParams base::`[`
#'
#' @returns the subset specified
#' @export
#'
`[.seroincidence.ests` <- function(x,i,...) {
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  r
}
