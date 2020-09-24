
#setwd("/srv/shiny-server/litcovid-19br/litcovid-19br")
source('global.R', local = T) 


# Define UI for application 
ui <- htmlTemplate(
    filename = "www/index.html",
    plotOutput = highchartOutput(outputId = "hcontainer"),
    mapOutput = highchartOutput(outputId = "mapcontainer",width = "100%",height = "600"),
    tableOutput = dataTableOutput(outputId = "table_sources",width = "100%",height = "600"),
    tableOutput_v2 =  uiOutput("table_bar"),
    worldcloud_output = wordcloud2Output(outputId =  "wordcloud2"),
    bar_output = highchartOutput(outputId = "barcontainer",width = "100%",height = "600"),
    gender_output = highchartOutput(outputId = "gendercontainer",width = "100%",height = "600")
    
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
    
    barInput <- reactive({
        
       z <-  name_base %>% 
            filter(!is.na(country1), 
                   country1 != 0,
                   !is.na(gender),
                   probability >= 0.9) %>% 
            group_by(gender, country1) %>% 
            summarise(n = n()) %>% 
            arrange(desc(n)) %>% 
            slice(1:input$slice_input) %>% 
            filter(n >= input$n_input) 
        
        
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
            hc_xAxis(title = list(text = paste("Publication ", tools::toTitleCase(as.character(input$select))))) %>% 
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
                  dataLabels = list(enabled = FALSE),
                  borderColor = '#999999', borderwidth = 0.1,
                  tooltip = list(valueDecimals = 0)) %>% 
            hc_title(text = "Publications around the World") %>% 
            hc_colorAxis(tickInterval = 10000) %>% 
            hc_legend(layout = "horizontal", align = "center", valueDecimals = 0)
        
        
    })

    

    
    # table_pat_all <- reactive(
    #     DT::datatable(dados_source_artigos, options = list(pageLength = 25))
    # )
    # output$table_pat_all <- DT::renderDataTable({
    #     table_pat_all()
    # })
    
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
    
    output$barcontainer <- renderHighchart({
        
        df2 <- barInput()
 
        df2 %>% 
            filter(country1 != 0, 
                   country1 != "0") %>% 
            hchart('bar', hcaes(x = country1, y = n, group = gender)) %>% 
            hc_add_theme(hc_theme_smpl())
        
        
    })
    
    output$gendercontainer <- renderHighchart({
        
        name_base %>% 
            filter(!is.na(country1), 
                   country1 != "0",
                   !is.na(gender),
                   probability >= 0.9) %>% 
            group_by(country1) %>% 
            summarise(n = n(),
                      fem = sum(gender=="F")) %>% 
            mutate(country = tools::toTitleCase(country1)) %>% 
            arrange(desc(n)) %>% 
            #slice(1:15) %>% 
            filter(n >= 1) %>%
            mutate(prop = (fem/n)*100,
                   prop = round(prop, digits = 1)) %>% 
            hcmap(data = .,
                  map = "custom/world",
                  name = "Percentage of Women as First Author",
                  showInLegend = FALSE,
                  download_map_data = TRUE,
                  value = "prop",
                  joinBy = c("name", "country"),
                  dataLabels = list(enabled = FALSE)) %>% 
            hc_title(text = "Proportion of Publications with Women as First Author Around the World") %>% 
            hc_subtitle(text = "Only names with probability of match greater than 90%") %>% 
            #hc_colorAxis(dataClasses = color_classes(c(seq(100, 115, by = 5)))) %>% 
            hc_legend(layout = "horizontal", align = "center", valueDecimals = 0) %>% 
            hc_colorAxis(
                stops = color_stops(colors = viridisLite::plasma(n = 10, begin = 0.45, end = 1, direction = -1)),
                type = "linear",
                tickInterval = 10
            ) 
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
