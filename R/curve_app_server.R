curve_app_server <- function(input, output, session) {
  exp10 <- function(x) 10^x
  derived_params <- tibble(
    mu_y = input$mu_y |> exp10(),
    mu_b = input$mu_b |> exp10(),
    gamma = input$gamma |> exp10(),
    y0 = input$y0 |> exp10(),
    b0 = input$b0 |> exp10(),
    alpha = input$alpha |> exp10(),
    rho = input$rho,
    t1 = t1f( # nolint: object_usage_linter
      mu_y = .data$mu_y,
      mu_b = .data$mu_b,
      gamma = .data$gamma,
      y0 = .data$y0,
      b0 = .data$b0
    ),
    y1 = y1f( # nolint: object_usage_linter
      y0 = .data$y0,
      mu_y = .data$mu_y,
      t1 = .data$t1
    )
  ) |>
    shiny::reactive()


  output$tab1 <-
    derived_params() |> shiny::renderTable(digits = 4)

  plot1 <-
    shiny::eventReactive(
      eventExpr = derived_params() | input$ymax1 | input$alpha | input$rho,
      {
        plot_decay_curve( # nolint: object_usage_linter
          decay_function = antibody_decay_curve, # nolint: object_usage_linter
          mu_y = input$mu_y |> exp10(),
          mu_b = input$mu_b |> exp10(),
          gamma = input$gamma |> exp10(),
          y0 = input$y0 |> exp10(),
          b0 = input$b0 |> exp10(),
          alpha = input$alpha |> exp10(),
          rho = input$rho,
          ymax = input$ymax1 |> exp10()
        )
      }
    )

  plot2 <-
    shiny::eventReactive(
      eventExpr = derived_params() | input$ymax2,
      {
        plot_decay_curve( # nolint: object_usage_linter
          decay_function = pathogen_decay_curve, # nolint: object_usage_linter
          mu_y = input$mu_y |> exp10(),
          mu_b = input$mu_b |> exp10(),
          gamma = input$gamma |> exp10(),
          y0 = input$y0 |> exp10(),
          b0 = input$b0 |> exp10(),
          ymax = input$ymax2 |> exp10()
        )
      }
    )

  output$plot1 <-
    plot1() |>
    shiny::renderPlot()

  output$plot2 <-
    plot2() |>
    shiny::renderPlot()
}
