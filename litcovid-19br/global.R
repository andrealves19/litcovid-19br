# Packages 

require(shiny)
#require(leaflet)
library(data.table)
library(shinydashboard)
#library(leaflet.extras)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(ggplot2)
library(highcharter)
#library(forecast)
library(htmlwidgets)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(wordcloud2)
library(tidyverse)
library(lubridate)
library(rworldmap)
library(mapdata)

print(getwd())
load("pubmed_data_R_ambiente.RData")
mapdata <- get_data_from_map(download_map_data("custom/world"))

S = readRDS(file = "pubmed_bibiometrix_analisys.rds")
S
#gerar dados : qtd de artigo por journal
dadossource = as.data.frame(S[["MostRelSources"]][["Sources       "]])
dadosartigos = as.data.frame(S[["MostRelSources"]]$Articles)
dadossource = dadossource %>% mutate(id = dplyr::row_number()) %>% rename(source = `S[["MostRelSources"]][["Sources       "]]`)                   
dadosartigos = dadosartigos %>% mutate(id = dplyr::row_number()) %>% rename(n = `S[["MostRelSources"]]$Articles`)                             
dados_source_artigos = dadossource %>% inner_join(dadosartigos,by = "id") %>% select(-id) %>% mutate(n = as.numeric(n))

#world cloud
freq_words_dt = fread("dados/dados_update_04092020/freq_words_11092020.csv")


#publications = fread("dados/dados_update_04092020/dimensions-covid19-export-2020-08-06-h06-08-44.csv",sep = ";")

#publications <- read_excel("dados/dados_update_04092020/dimensions-covid19-export-2020-09-16-h06-08-18.xlsx")
#saveRDS(publications, file = "publications")
publications = readRDS(file = "publications")


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
