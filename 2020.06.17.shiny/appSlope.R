#######
## Chamber Plots

library(shiny)
library(ggplot2)


#### Kate Corner ####
# this to figure out
# the UI /
# the main page
# what is supposed to go on the server side





# Define User interface ----
ui <- fluidPage(
  titlePanel("It's Shiny!!!"),
  sidebarLayout(
    sidebarPanel( width = 2,
                  numericInput("slope", h4("What is your favorite slope?"), "1"),
                  
                  radioButtons("lineColor", h3("Line Color"),
                               choices = list("I like purple!" = "purple",
                                              
                                              #***the label***  #***the output***
                                              
                                              "Black is the new black" = "black",
                                              "Soemthing boring" = "grey"),
                                              selected = "purple"),
                  selectInput("lineType", h4("Line Type"),
                              list("Solid" = "solid", 
                                   "Dashed" = "dashed",
                                   "Dotted" = "dotted"),
                                  selected = "solid"
                              ),
                  textInput("name", "What is your name?", "Someone")
                  
    ),
    mainPanel(
      width = 10,
      h1(textOutput("favText")),
      plotOutput("plot", width = "100%", height = "740px")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$favText <-renderText({
    paste("This is ", input$name, "'s favorite slope.", sep = "")
  })
  output$plot <- renderPlot({
    #make a data frame that uses the input value
    d <-data.frame(x = 1:100)
    d$y <- d$x * input$slope
    ggplot(d, aes(x,y)) +
      geom_line(colour = input$lineColor, 
                linetype = input$lineType,
                size = 4) +
      ylim(0,100) +
      theme_bw(base_size = 24)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)