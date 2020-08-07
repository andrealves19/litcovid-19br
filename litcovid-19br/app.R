# Packages 

require(shiny)
require(dplyr)
require(ggplot2)
require(highcharter)
require(forecast)
require(htmlwidgets)
require(shinyjs)
require(shinyWidgets)
setwd("/home/juracybertoldo/Shiny_projetos/litcovid-19br/litcovid-19br/")

# Define UI for application 
ui <- htmlTemplate(
    filename = "www/index.html",
    plotOutput = leafletOutput(outputId = "mapa",width = "100%",height = "600"),
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
    
    output$mapa <- renderLeaflet({
        quintiles =  quantile(sPDF$qtd_artigos, probs = (0:5)/5,na.rm = T)
        pal_ <- colorBin("Blues", domain = sPDF$qtd_artigos, bins = quintiles)
        labels <- sprintf(
            "<strong>%s</strong><br/>%g </sup>",
            sPDF$Country,sPDF$qtd_artigos ) %>% lapply(htmltools::HTML)
        
        m <- leaflet(sPDF, options = leafletOptions(minZoom = 2,zoomSnap = 0.3,zoomDelta = 0.3) ) %>%  addTiles() %>% addPolygons(
            fillColor = ~pal_(qtd_artigos),
            weight = 0.5,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            label = labels,
            layerId = ~Country,
            highlightOptions = highlightOptions(color = "black", weight = 2,
                                                bringToFront = TRUE)) %>%
            addResetMapButton() %>% addLegend(pal = pal_, values = ~qtd_artigos, opacity = 0.7, title = NULL,
                                              position = "bottomright")

         
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
