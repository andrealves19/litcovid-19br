
library(bibliometrix)
library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)
library(stringi)


#script para integrar os dados de diversas fontes e disponibilizar para o aplicativo
setwd("/home/juracybertoldo/Shiny_projetos/litcovid-19br")
#pubmed = convert2df(file = "pubmed_data_04092020.txt",dbsource = "pubmed",format = "csv")
#fwrite(x = pubmed,file = "pubmed_data_04092020.csv",sep = ";")
pubmed = fread(input = "pubmed_data_04092020.csv")
medrxiv = fread("litcovid-19br/dados/dados_update_04092020/medRxiv_results_2020-09-04_.csv",sep = ";")
allen_data = fread("litcovid-19br/dados/dados_update_04092020/metadata.csv")
dimensions = fread("litcovid-19br/dados/dados_update_04092020/dimensions-covid19-export-2020-08-06-h06-08-44.csv",sep = ";")



allen_merge = allen_data %>% select(doi,title,abstract,source_x,publish_time,authors,journal) %>% mutate(date = ymd(publish_time),
                                                                                                         ano = year(date),base = "CORD-19")
names(allen_merge_filter_v2)
allen_merge = allen_merge %>% mutate(ano2 = if_else(is.na(ano),as.numeric(publish_time),ano  ))

allen_merge_filter = allen_merge %>% filter(ano >= 2019 ) %>% filter(ano <= 2020)
allen_merge_filter_v2 = allen_merge_filter %>% filter(date >= "2020-03-01") %>% select(-c(ano,ano2,source_x,publish_time))





#table(allen_merge_filter$ano2)


medrxiv_merge = medrxiv %>% mutate(base = "MedRxiv") %>% select(doi,title,abstract,date,authors,base)  %>% mutate(date = ymd(date))

dimensions_merge = dimensions %>% select(DOI,Title,Abstract,"Publication Date","Open Access",Authors,"Country of Research organization") %>% mutate(base = "dimensions")
dimensions_merge = dimensions_merge %>% rename(doi = DOI,title = Title,abstract = Abstract,authors = Authors,pais = "Country of Research organization",
                                               date = "Publication Date")



pubmed_merge = pubmed %>% select(TI,AB,AU,AID,DEP) %>% rename(title = TI,abstract = AB,authors = AU, DOI_PII_list= AID,date = DEP  ) %>% mutate(base = "pubmed")
names(pubmed)

pubmed_merge_v2 = pubmed_merge %>% separate(DOI_PII_list,c("DOI_1","DOI_2"),sep = ";",remove = F)
pubmed_merge_v2 = pubmed_merge_v2 %>% mutate(DOI_1_v2 = if_else( grepl("DOI",DOI_1,fixed = TRUE)  ,DOI_1 , ""  ) )
pubmed_merge_v2 = pubmed_merge_v2 %>% mutate(DOI_2_v2 = if_else( grepl("DOI",DOI_2,fixed = TRUE)  ,DOI_2 , ""  ) )


pubmed_merge_v2 = pubmed_merge_v2 %>% mutate(check = if_else(DOI_1_v2 != "" & DOI_2_v2 != "",1,0),doi = if_else(DOI_1_v2 != "",DOI_1_v2,DOI_2_v2 ),
                                             date = ymd(date)) 

names(pubmed_merge_v2)
pubmed_merge_v2 = pubmed_merge_v2 %>% separate(doi,c("DOI_ok","nome"),sep = " ",remove = F) %>% select(-c(nome,doi)) %>% rename(doi = DOI_ok)

pubmed_merge_v2 = pubmed_merge_v2 %>% select(-c(DOI_1,DOI_2,DOI_1_v2,DOI_2_v2,check,DOI_PII_list))




glimpse(allen_merge_filter_v2)
glimpse(medrxiv_merge)
glimpse(dimensions_merge)
dados_dash = bind_rows(allen_merge_filter_v2,medrxiv_merge)
dados_dash = bind_rows(dados_dash,dimensions_merge)
dados_dash = bind_rows(dados_dash,pubmed_merge_v2)

empty_as_na <- function(x){
    if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
    ifelse(as.character(x)!="", x, NA)
}

## transform all columns
dados_dash = dados_dash %>% mutate_each(funs(empty_as_na)) 


dados_dash_doiNA  = dados_dash %>% filter(is.na(doi))

dados_dash_doi = dados_dash %>% filter(!is.na(doi))
distinct_df = dados_dash_doi %>% distinct(doi,.keep_all = T)

check = dados_dash %>% group_by(doi) %>% summarise(bases_ = paste0(base, collapse = "|"), contador = n()) 

distinct_df_todos = distinct_df %>% bind_rows(dados_dash_doiNA)

fwrite(x =  distinct_df_todos,file = "base_dash_tipo1_v1.csv",sep = ";")


#codigo para gerar dados para plotar o mapa , incompleto
#results_D <- biblioAnalysis(pubmed, sep = ";")

#results_D

#res = plot(x = results_D, k = 3, pause = FALSE)
#res
#df_paises = res['MostProdCountries']$MostProdCountries
#df_paises = as.data.frame(res[["MostProdCountries"]])
#pubmed = fread("pubmed_data_04092020.txt")
#res[["MostProdCountries"]]$data


