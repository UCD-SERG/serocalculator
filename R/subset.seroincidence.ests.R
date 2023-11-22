#' Extract or replace parts of a `seroincidence.by` object
#'
#' @param x the object to subset/replace elements of
#' @param i the indices to subset/replace
#' @param ... passed to `[.list`
#' @returns the subset specified
#' @export
#'
`[.seroincidence.by` <- function(x,i,...) {
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  names(r) = names(x)[i]
  r
}
