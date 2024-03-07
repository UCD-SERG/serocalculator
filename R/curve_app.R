curve_app = function()
{
  shiny::shinyApp(
    ui = curve_app_ui,
    server = curve_app_server
  )
}
