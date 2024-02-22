
# load libraries
library(ggplot2)
library(magrittr)


# Define UI for app --
ui <- fluidPage(

  # App title ----
  titlePanel("Parameters"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar to demonstrate various slider options ----
    sidebarPanel(

      # Input: Simple integer interval ----
      sliderInput("y0", "Baseline antibody concentration (y0):",
                  min = 0.0, max = 4000,
                  value = 2.48, step = 0.01),

      # Input: Decimal interval with step value ----
      sliderInput("y1", "Peak antibody concentration (y1):",
                  min = 0.5, max = 1300000,
                  value = 63.48, step = 0.01),

      # Input: Specification of range within an interval ----
      sliderInput("t1", "Time to peak antibody concentration (t1):",
                  min = 0.0, max = 190,
                  value = 9.51, step = 0.01),

      # Input: Custom currency format for with basic animation ----
      sliderInput("alpha", "Antibody decay rate (alpha):",
                  min = 5.494682e-08, max = 0.35,
                  value =5.807501e-04, step = 0.00001),

      # Input: Animation
      sliderInput("r", "Antibody decay shape (r):",
                  min = 1.0, max = 19,
                  value = 1.7, step = 1)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Table summarizing the values entered ----
      tableOutput("values"),
      plotOutput("plot", click = "plot_click")
      #textOutput(outputId = "Slider1Out")

    )
  )
)
