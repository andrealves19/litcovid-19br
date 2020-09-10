# Packages 

require(shiny)
require(dplyr)
require(ggplot2)
require(highcharter)
require(forecast)
require(htmlwidgets)
require(shinyjs)
require(shinyWidgets)
require(lubridate)
require(stringr)
require(rworldmap)
require(pubmedR)
require(bibliometrix)


#setwd(dir = "C:/Users/Visitante/Documents/GitHub/litcovid-19br/litcovid-19br/")

teste = convert2df(file = "C:/Users/Visitante/Documents/GitHub/litcovid-19br/pubmed-COVID-19Ti-set.nbib",dbsource = "pubmed",format = "csv")

# Define UI for application 
ui <- htmlTemplate(
    filename = "www/index.html",
    plotOutput = highchartOutput(outputId = "hcontainer"),
    mapOutput = plotOutput(outputId = "mapcontainer", 
                           width = 1200, height = 700)

)


# Define server logic required to draw a timeseries
server <- function(input, output) {
    
    
    output$hcontainer <- renderHighchart({

        teste %>% 
            mutate(DEP = ymd(DEP)) %>% 
            filter(DEP >= '2020-01-01',
                   LA %in% c('ENG', 'POR', 'FRE', 'SPA', 'GER', 'ITA', 'RUS', 'CHI', 'DUT')) %>% 
            mutate(year = year(DEP),
                   week = isoweek(DEP)) %>% 
            group_by(week, LA) %>% 
            summarise(n = n()) %>% 
            hchart('line', hcaes(x = week, y = n, group = LA)) %>% 
            hc_add_theme(hc_theme_google()) %>% 
            hc_title(text = "Number of Publications by Language") %>% 
            hc_yAxis(title = list(text = "Count")) %>% 
            hc_xAxis(title = list(text = "Publication Week")) %>% 
            hc_subtitle(text = "Primary Language of Publication")%>% 
            hc_legend(align = "center")
    })
    
    
    output$mapcontainer <- renderPlot({
        
        matched <- teste %>% 
            filter(DEP >= '2020-01-01',
                   !is.na(PL)) %>% 
            group_by(PL) %>% 
            summarise(n = n()) %>% 
            joinCountryData2Map(joinCode = "NAME", nameJoinColumn = "PL")
        
        mapParams <- mapCountryData(matched,
                                    nameColumnToPlot = "n",
                                    mapTitle = "Number of Publications by Country", addLegend = FALSE,
                                    mapRegion = 'world',
                                    #missingCountryCol = "grey",
                                    numCats = 9)
        
        #country_coord <-data.frame(coordinates(matched),stringsAsFactors=F)
        #country <- na.omit(matched@data$PL)
        #country <- str_to_title(country)
        #country[35] <- "United States of America"
        #country_coord <-  country_coord[country,]
        
        #text(x=country_coord$X1,y=country_coord$X2,labels=row.names(country_coord), cex = 0.7)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
