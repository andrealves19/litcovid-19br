
setwd("/srv/shiny-server/litcovid-19br/litcovid-19br")
source('global.R', local = T) 


# Define UI for application 
ui <- htmlTemplate(
    filename = "www/index.html",
    plotOutput = highchartOutput(outputId = "hcontainer"),
    mapOutput = highchartOutput(outputId = "mapcontainer"),
    tableOutput = dataTableOutput(outputId = "table_sources",width = "100%",height = "600"),
    tableOutput_v2 =  uiOutput("table_bar"),
    #mapOutput = highchartOutput(outputId = "mapcontainer"),
    worldcloud_output = wordcloud2Output(outputId =  "wordcloud2")
    
)


# Define server logic required to draw a timeseries
server <- function(input, output) {
    
    
    
    output$wordcloud2 <- renderWordcloud2({
        wordcloud2::wordcloud2(freq_words_dt, color = "random-dark", backgroundColor="rgba(255, 255, 255, 0)")
    })
    
    dfInput <- reactive({
        x <- df1 %>% mutate(variable = !! rlang::sym(input$select))
    })
    
    mapInput <- reactive({
        y <- publications %>% mutate(variable = !! rlang::sym(input$mapSelect))
    })
    
    output$hcontainer <- renderHighchart({
        
        df1 <- dfInput()
        
        df1 %>% 
            #filter(country1 %in% target) %>% 
            group_by(variable, country1) %>% 
            summarise(n = n()) %>% 
            hchart('line', hcaes(x = variable, y = n, group = country1)) %>% 
            hc_add_theme(hc_theme_google()) %>% 
            hc_title(text = "Number of Publications by Primary Country") %>% 
            hc_yAxis(title = list(text = "Count")) %>% 
            hc_xAxis(title = list(text = "Publication Month")) %>% 
            hc_subtitle(text = "Primary Country of Publication") %>% 
            hc_legend(align = "center")
        
    })
    
    
    output$mapcontainer <- renderHighchart({
        
        dfmap <- mapInput()
        
       
        dfmap$variable[dfmap$variable=="United States"] <- "United States of America"
        mapdata$name[mapdata$name=="United States of America"] <- "United States Of America"
        
        dfmap %>% 
            filter(!is.na(country1)) %>% 
            group_by(variable) %>% 
            summarise(n = n()) %>% 
            mutate(country = tools::toTitleCase(variable)) %>% 
            arrange(desc(n)) %>% 
            hcmap(data = .,
                  map = "custom/world",
                  showInLegend = FALSE,
                  download_map_data = TRUE,
                  value = "n",
                  joinBy = c("name", "country"),
                  dataLabels = list(enabled = TRUE)) %>% 
            hc_title(text = "Publications around the World") # %>% 
        #hc_colorAxis(dataClasses = color_classes(c(seq(100, 115, by = 5)))) %>% 
        #hc_legend(layout = "horizontal", align = "center", valueDecimals = 0)
        
        
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
