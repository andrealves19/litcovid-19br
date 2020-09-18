
setwd("/home/juracybertoldo/Shiny_projetos/litcovid-19br/litcovid-19br/")
source('global.R', local = T) 


# Define UI for application 
ui <- htmlTemplate(
    filename = "www/index.html",
    plotOutput = leafletOutput(outputId = "mapa",width = "100%",height = "600"),
    tableOutput = dataTableOutput(outputId = "table_sources",width = "100%",height = "600"),
    tableOutput_v2 =  uiOutput("table_bar"),
    mapOutput = highchartOutput(outputId = "mapcontainer"),
    worldcloud_output = wordcloud2Output(outputId =  "wordcloud2")
    
)


# Define server logic required to draw a timeseries
server <- function(input, output) {
    
    
    
    output$wordcloud2 <- renderWordcloud2({
        wordcloud2::wordcloud2(freq_words_dt, color = "random-dark", backgroundColor="rgba(255, 255, 255, 0)")
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
        bins <- c(0, 10, 100, 500, 1000, 2000, 4000, Inf)
        pal_ <- colorBin("Blues", domain = sPDF$qtd_artigos, bins = bins)
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
    

    
    table_pat_all <- reactive(
        DT::datatable(dados_source_artigos, options = list(pageLength = 25))
    )
    output$table_pat_all <- DT::renderDataTable({
        table_pat_all()
    })
    
    output$table_bar <- renderUI({
        div(
            style = "position: relative",
            shinydashboard::tabBox(
                id = "box_pat2",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Tabela",
                    htmlOutput("patients_total"),
                    withSpinner(
                        DT::dataTableOutput("table_pat_all"),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                ),
                tabPanel(
                    title = "Plot",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                    ),
                    withSpinner(
                        plotOutput("plot_age_select", height = 500,width = "100%"),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    plot_age_select <- reactive({
        
        
        dados_source_artigos = dados_source_artigos %>% mutate(nome = str_sub(source, start = 1,end = 30))
        g <- dados_source_artigos %>% 
            arrange(n) %>% 
            ggplot(., mapping = aes(x=reorder(nome, n), y =n,fill = n))+
            geom_bar(stat = "identity", width=.8, position = position_dodge(width = .25))+
            scale_colour_brewer(palette ="Blues")+
            labs(
                title = "",
                y= "Frequência"
            )+
            coord_flip()
        g
    })
    
    output$plot_age_select <- renderPlot({
        plot_age_select()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
