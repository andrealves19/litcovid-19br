library(pubmedR)
library(bibliometrix)
library(tidyverse)
library(xlsx)
library(data.table)
setwd("/home/juracybertoldo/Shiny_projetos/litcovid-19br/")


query = "“COVID-19”[Title/Abstract] OR “2019 novel coronavirus”[Title/Abstract] OR “COVID19”[Title/Abstract] OR “coronavirus disease 2019”[Title/Abstract] OR “coronavirus
disease-19”[Title/Abstract] OR “2019-nCoV”[Title/Abstract] OR “severe acute respiratory
syndrome coronavirus 2”[Title/Abstract] OR “SARS-CoV-2”[Title/Abstract] OR
“SARS2”[Title/Abstract]"

# query_count = pmQueryTotalCount(query)
# query_count
# 
# pubmequery_request = pmApiRequest(query,limit = 500)  # reduzir para ser mais rápido
# #glimpse(pubmequery_request)
# pubme_df = pmApi2df(pubmequery_request) 
# 
# 
# results <- biblioAnalysis(pubme_df, sep = ";")
# S <- summary(object = results, k = 10, pause = FALSE)
# 
# 
# fwrite(x = pubme_df,file = "testepubmed_v3_raw.csv",sep = "|")

teste = convert2df(file = "C:/Users/Visitante/Documents/GitHub/litcovid-19br/pubmed-COVID-19Ti-set.nbib",dbsource = "pubmed",format = "csv")

results <- biblioAnalysis(teste, sep = ";")

plot(x = results, k = 10, pause = FALSE)
