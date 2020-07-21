library(pubmedR)
library(bibliometrix)
library(tidyverse)
setwd("/home/juracybertoldo/Shiny_projetos/litcovid-19br/")

query = "“COVID-19”[Title/Abstract] OR “2019 novel coronavirus”[Title/Abstract] OR “COVID19”[Title/Abstract] OR “coronavirus disease 2019”[Title/Abstract] OR “coronavirus
disease-19”[Title/Abstract] OR “2019-nCoV”[Title/Abstract] OR “severe acute respiratory
syndrome coronavirus 2”[Title/Abstract] OR “SARS-CoV-2”[Title/Abstract] OR
“SARS2”[Title/Abstract]"

query_count = pmQueryTotalCount(query)
query_count

pubmequery_request = pmApiRequest(query,limit = 5000)  # reduzir para ser mais rápido

pubme_df = pmApi2df(pubmeddata,format = "bibliometrix")  # tentar salvar como raw para tentar ler depois direto no biblio

write_csv(x = pubme_df,path = "pubmed_teste2_raw.csv")
getwd()


#fwrite(x = pubme_df , file = "pubmed_teste2_raw.txt",sep = ",",quote = "auto")

M <- convert2df(file = "pubmed_teste2_raw.txt", dbsource = "pubmed")


results <- biblioAnalysis(pubme_df)
summary(results)
