#' Launch the interactive Shiny app for the antigen-antibody kinetics model
#' @returns `NULL` (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' curve_app()
#' }
curve_app <- function() {
  shiny::shinyApp(
    ui = curve_app_ui, # nolint: object_usage_linter
    server = curve_app_server # nolint: object_usage_linter
  )
}
