curve_app_ui = function(request)
{

  sidebar = shiny::sidebarPanel(
    width = 3,

    sliderInput(
      inputId = "y0",
      label = "log10(y0)",
      min = -2,
      max = 2,
      step = .1,
      val =  0),


    sliderInput(
      inputId = "b0",
      label = "log10(b0)",
      min = -2,
      max = 2,
      step = .1,
      val =  0),

    sliderInput(
      inputId = "mu_b",
      label = "log10(mu_b)",
      min = -2,
      max = 2,
      step = .1,
      val =  log10(0.18432798)),
    sliderInput(
      inputId = "mu_y",
      label = "log10(mu_y)",
      min = -2,
      max = 2,
      step = .1,
      val =  log10(0.36853621)),
    sliderInput(
      inputId = "gamma",
      label = "log10(gamma)",
      min = -10,
      max = 10,
      step = .1,
      val =  log10(0.0013040664)),

    sliderInput(
      inputId = "rho",
      label = "rho",
      min = 1,
      max = 3,
      step = .1,
      val =  2),

    sliderInput(
      inputId = "alpha",
      label = "log10(alpha)",
      min = -8,
      max = -1,
      step = .1,
      val =  log10(0.00002192627)),

    shiny::sliderInput(
      inputId = "ymax",
      label = "log10(ymax)",
      min = 2,
      max = 10,
      step = .1,
      val = 4.5)

    # shiny::textInput(
    #   inputId = "inputtext1",
    #   label = "title 1",
    #   value = ""
    # )
  )


  shiny::sidebarLayout(
    sidebarPanel = sidebar,
    mainPanel = shiny::mainPanel(
      width = 9,

      # shiny::fluidRow(shiny::textOutput("text"))  ,
      shiny::fluidRow(
        shiny::tableOutput("tab1")
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          h2("pathogens"),
          # plotly::plotlyOutput("plot2")),
          shiny::plotOutput("plot2")),
        shiny::column(
          width = 6,
          h2("antibodies"),
          # plotly::plotlyOutput("plot1"))
          shiny::plotOutput('plot1'))


      )
    )
  ) |>
    shiny::fluidPage()
}
