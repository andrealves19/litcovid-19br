# Packages 

packages <- c("shiny", "tidyverse", "highcharter", "htmlwidgets", "shinyjs", "shinyWidgets", "lubridate", "stringr", "rworldmap", "pubmedR", "DT", "readxl", "leaflet")
lapply(packages, require, character.only = TRUE)


#setwd(dir = "C:/Users/Visitante/Documents/GitHub/litcovid-19br/litcovid-19br/")
#teste = convert2df(file = "C:/Users/Visitante/Documents/GitHub/litcovid-19br/pubmed-COVID-19Ti-set.nbib",dbsource = "pubmed",format = "csv")

# Publications
publications <- read_excel("C:/Users/Visitante/Downloads/dimensions-covid19-export-2020-08-06-h06-08-44.xlsx")

# Covid Cases
cases <- read_csv("C:/Users/Visitante/Downloads/covid-cases-by-source.csv")


publications <- publications %>% 
    separate(`Country of Research organization`, into = c("country1", "country2", "country3"), sep = "; ", remove = FALSE)

publications <- publications %>% 
    mutate(date = ymd(`Publication Date`),
           week = epiweek(date),
           month = month(date, label = TRUE))

target <- publications %>% 
    filter(!is.na(country1)) %>% 
    group_by(country1) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% 
    slice(1:15) %>% 
    select(country1)

target <- as.vector(target$country1)

df1 <- publications %>% 
    filter(country1 %in% target)



# Define UI for application 
ui <- htmlTemplate(
    filename = "www/index.html",
    plotOutput = highchartOutput(outputId = "hcontainer"),
    mapOutput = highchartOutput(outputId = "mapcontainer")
)


# Define server logic required to draw a timeseries
server <- function(input, output) {
    
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
}

# Run the application 
shinyApp(ui = ui, server = server)
