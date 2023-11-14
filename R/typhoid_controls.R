#' @docType data
#'
#' @name typhoid_controls
#'
#' @title
#' Example cross-sectional data for typhoid (`HlyE_IgA` and `HlyE_IgG`)
#'
#' @description
#' Data frame of ELISA assay results, by `pop` (`"CA Facts"`, `"MGH"`), `antigen_iso` (`"HlyE_IgA"`, `"HlyE_IgG"`), and `Age` (in years).
#'
#' @usage
#' typhoid_controls
#'
#' @format
#'  The measurements are in the variable `elisa`. Variable `nu` provides an estimate of a conditional noise parameter.
#'
#' @examples
#'
#' # Print the data:
#' typhoid_controls
#'
#' # Plot the data
#' library(ggplot2)
#' typhoid_controls |>
#'   ggplot(aes(x = Age, y = elisa, col = antigen_iso)) +
#'   geom_point() +
#'   geom_smooth()
#'
NULL
