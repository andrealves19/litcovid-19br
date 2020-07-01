

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- htmlTemplate(
    filename = "www/index.html",
    plotOutput = plotOutput(outputId = "distPlot")

)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x))

        # draw the histogram with the specified number of bins
        ggplot(faithful, aes(x = eruptions, y = waiting)) + 
            geom_point() +
            theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
