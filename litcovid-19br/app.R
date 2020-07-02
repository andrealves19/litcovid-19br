# Packages 

require(shiny)
require(ggplot2)
require(highcharter)
require(forecast)
require(htmlwidgets)
require(shinyjs)
require(shinyWidgets)

# Define UI for application 
ui <- htmlTemplate(
    filename = "www/index.html",
    plotOutput = highchartOutput(outputId = "hcontainer"),
    mapOutput = highchartOutput(outputId = "mapcontainer")

)


# Define server logic required to draw a timeseries
server <- function(input, output) {
    
    
    output$hcontainer <- renderHighchart({

        airforecast <- forecast(auto.arima(AirPassengers), level = 95)
        
        hchart(airforecast)
    })
    
    
    output$mapcontainer <- renderHighchart({
        
        mapdata <- get_data_from_map(download_map_data("countries/us/us-all"))
        
        data_fake <- mapdata %>% 
            select(code = `hc-a2`) %>% 
            mutate(value = 1e5 * abs(rt(nrow(.), df = 10)))
        
        hcmap("countries/us/us-all", data = data_fake, value = "value",
              joinBy = c("hc-a2", "code"), name = "Fake data",
              dataLabels = list(enabled = TRUE, format = '{point.name}'),
              borderColor = "#FAFAFA", borderWidth = 0.1,
              tooltip = list(valueDecimals = 2, valuePrefix = "$", valueSuffix = " USD")) 
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
