#Super simple Shiny example

library(shiny)

ui <- fluidPage(
  numericInput("n", "Sample size", value = 25),
  plotOutput("hist")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$n))
  })
}
shinyApp(ui = ui, server = server)