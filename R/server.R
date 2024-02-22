
# load script
source('graph.decay.curves.R')

# Define server logic for slider examples ----
server <- function(input, output) {

  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({

    data.frame(

      matrix(data = c(input$y0,
                      input$y1,
                      input$t1,
                      input$alpha,
                      input$r),nrow = 1,ncol = 5) %>%
        set_colnames(c("y0",
                       "y1",
                       "t1",
                       "alpha",
                       "r"))
    )


  })

  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })

  output$plot <- renderPlot({
    # plot curve
    p = plot_curve_params_one_ab(object = sliderValues())
    print(p)
  }, res = 96)

  #output$Slider1Out <- renderText({
  #  paste("You've selected: ", input$t1)
  #})

}
