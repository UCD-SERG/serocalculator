exp10 = function(x) 10^x

curve_app_server = function(input, output, session)
{
  derived_params =
    {
      tibble(
        mu_y = input$mu_y |> exp10(),
        mu_b = input$mu_b |> exp10(),
        gamma = input$gamma |> exp10(),
        y0 = input$y0 |> exp10(),
        b0 = input$b0 |> exp10(),
        alpha = input$alpha |> exp10(),
        rho = input$rho,
        t1 = t1f(
          mu_y = mu_y,
          mu_b = mu_b,
          gamma = gamma,
          y0 = y0,
          b0 = b0),
        y1 = y1f(y0 = input$y0 |> exp10(), mu_y = mu_y, t1 = t1)
      )
    }|> reactive()


  output$text = renderText(input$mu_y)
  output$tab1 =
    derived_params() |> renderTable(digits = 4)

  plot1 =
    {
      plot_decay_curve(
        decay_function = antibody_decay_curve,
        mu_y = input$mu_y |> exp10(),
        mu_b = input$mu_b |> exp10(),
        gamma = input$gamma |> exp10(),
        y0 = input$y0 |> exp10(),
        b0 = input$b0 |> exp10(),
        alpha = input$alpha |> exp10(),
        rho = input$rho,
        # title = input$inputtext1,
        ymax = 10^input$ymax

      )} |> reactive()

  plot2 =
    plot_decay_curve(
      decay_function = pathogen_decay_curve,
      mu_y = input$mu_y |> exp10(),
      mu_b = input$mu_b |> exp10(),
      gamma = input$gamma |> exp10(),
      y0 = input$y0 |> exp10(),
      b0 = input$b0 |> exp10()
    ) |> reactive()

  output$plot1 =
    plot1() |>
    renderPlot()
    # plotly::ggplotly()  |>
    # plotly::layout(legend = list(orientation = 'h')) |>
    # plotly::renderPlotly()

  output$plot2 =
    plot2() |>
    renderPlot()
    # plotly::ggplotly()  |>
    # plotly::layout(legend = list(orientation = 'h')) |>
    # plotly::renderPlotly()
}
