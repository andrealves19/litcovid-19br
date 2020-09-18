# Packages 

require(shiny)
require(leaflet)
require(shinydashboard)
require(leaflet.extras)
require(shinycssloaders)
require(plotly)
require(dplyr)
require(ggplot2)
require(highcharter)
require(forecast)
require(htmlwidgets)
require(shinyjs)
require(shinyWidgets)
require(DT)
library(wordcloud2)

print(getwd())
load("pubmed_data_R_ambiente.RData")


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
