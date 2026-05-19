#' Prepend a subclass to an object's class vector
#'
#' @param x An object.
#' @param subclass Character string to prepend to
#'   the class vector.
#'
#' @returns `x` with `subclass` prepended to its class.
#'
#' @keywords internal
.subclass <- function(x, subclass) {
  existing_classes <- x |> class()
  new_classes <- c(subclass, existing_classes)
  x |> structure(class = new_classes)
}
