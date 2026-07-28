curve_app_server <- function(input, output, session) {
  exp10 <- function(x) 10^x
  derived_params <- shiny::reactive({
    tryCatch(
      tibble::tibble(
        mu_y = input$mu_y |> exp10(),
        mu_b = input$mu_b |> exp10(),
        gamma = input$gamma |> exp10(),
        y0 = input$y0 |> exp10(),
        b0 = input$b0 |> exp10(),
        alpha = input$alpha |> exp10(),
        rho = input$rho,
        t1 = t1f( # nolint: object_usage_linter
          mu_y = .data$mu_y, # nolint: object_usage_linter
          mu_b = .data$mu_b, # nolint: object_usage_linter
          gamma = .data$gamma, # nolint: object_usage_linter
          y0 = .data$y0, # nolint: object_usage_linter
          b0 = .data$b0 # nolint: object_usage_linter
        ),
        y1 = y1f( # nolint: object_usage_linter
          y0 = .data$y0, # nolint: object_usage_linter
          mu_y = .data$mu_y, # nolint: object_usage_linter
          t1 = .data$t1 # nolint: object_usage_linter
        )
      ),
      error = function(e) {
        shiny::validate(paste(
          "Invalid parameter combination - please adjust the sliders:",
          conditionMessage(e)
        ))
      }
    )
  })


  output$tab1 <-
    derived_params() |> shiny::renderTable(digits = 4)

  plot1 <-
    shiny::eventReactive(
      eventExpr = {
        derived_params()
        input$ymax1
      },
      {
        dp <- derived_params() # nolint: object_usage_linter
        plot_decay_curve( # nolint: object_usage_linter
          decay_function = antibody_decay_curve, # nolint: object_usage_linter
          mu_y = dp$mu_y,
          mu_b = dp$mu_b,
          gamma = dp$gamma,
          y0 = dp$y0,
          b0 = dp$b0,
          alpha = dp$alpha,
          rho = dp$rho,
          ymax = input$ymax1 |> exp10()
        )
      }
    )

  plot2 <-
    shiny::eventReactive(
      eventExpr = {
        derived_params()
        input$ymax2
      },
      {
        dp <- derived_params() # nolint: object_usage_linter
        plot_decay_curve( # nolint: object_usage_linter
          decay_function = pathogen_decay_curve, # nolint: object_usage_linter
          mu_y = dp$mu_y,
          mu_b = dp$mu_b,
          gamma = dp$gamma,
          y0 = dp$y0,
          b0 = dp$b0,
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
