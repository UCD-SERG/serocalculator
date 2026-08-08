curve_app_ui <- function(request) {

  sidebar <- shiny::sidebarPanel(
    width = 3,

    shiny::sliderInput(
      inputId = "y0",
      label = "log10(y0)",
      min = -2,
      max = 2,
      step = .1,
      value = 0
    ),

    shiny::sliderInput(
      inputId = "b0",
      label = "log10(b0)",
      min = -2,
      max = 2,
      step = .1,
      value = 0
    ),

    shiny::sliderInput(
      inputId = "mu_b",
      label = "log10(mu_b)",
      min = -2,
      max = 2,
      step = .1,
      value = log10(0.18432798)
    ),

    shiny::sliderInput(
      inputId = "mu_y",
      label = "log10(mu_y)",
      min = -2,
      max = 2,
      step = .1,
      value = log10(0.36853621)
    ),

    shiny::sliderInput(
      inputId = "gamma",
      label = "log10(gamma)",
      min = -10,
      max = 10,
      step = .1,
      value = log10(0.0013040664)
    ),

    shiny::sliderInput(
      inputId = "rho",
      label = "rho",
      min = 1,
      max = 3,
      step = .1,
      value = 2
    ),

    shiny::sliderInput(
      inputId = "alpha",
      label = "log10(alpha)",
      min = -8,
      max = -1,
      step = .1,
      value = log10(0.00002192627)
    )
  )


  shiny::sidebarLayout(
    sidebarPanel = sidebar,
    mainPanel = shiny::mainPanel(
      width = 9,

      shiny::fluidRow(
        shiny::tableOutput("tab1")
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h2("pathogens"),
          shiny::plotOutput(height = "300px", "plot2"),
          shiny::sliderInput(
            inputId = "ymax2",
            label = "log10(ymax)",
            min = 0,
            max = 5,
            step = .1,
            value = 1
          ),
          shiny::h2("antibodies"),
          shiny::plotOutput(height = "300px", "plot1"),
          shiny::sliderInput(
            inputId = "ymax1",
            label = "log10(ymax)",
            min = 2,
            max = 10,
            step = .1,
            value = 4.5
          )
        )
      )
    )
  ) |>
    shiny::fluidPage()
}
