require(lubridate)
require(highcharter)
require(stringr)

lubridate::ymd(teste$DEP)

teste <- teste %>% 
  mutate(DEP = ymd(DEP))

teste %>% 
  filter(DEP >= '2020-01-01') %>% 
  ggplot(aes(x = DEP)) + 
  geom_bar()

hc <- hchart(teste, 'line',
             hcaes(x = DEP, y = n()))

hc

teste %>% 
  filter(DEP >= '2020-01-01',
         LA %in% c('ENG', 'POR', 'FRE', 'SPA', 'GER', 'ITA', 'RUS', 'CHI', 'DUT')) %>% 
  group_by(DEP, LA) %>% 
  summarise(n = n()) %>% 
  hchart('line', hcaes(x = DEP, y = n, group = LA)) %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_title(text = "Number of Publications by Language") %>% 
  hc_yAxis(title = list(text = "Count")) %>% 
  hc_xAxis(title = list(text = "Publication Date")) %>% 
  hc_subtitle(text = "Primary Language of Publication") %>% 
  hc_legend(align = "center")
  

teste %>% 
  filter(DEP >= '2020-01-01',
         !is.na(PL)) %>% 
  group_by(DEP, PL) %>%
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(n > 1) %>% 
  hchart('bar', hcaes(x = PL, y = n)) %>% 
  hc_add_theme(hc_theme_economist())

install.packages("rworldmap")
require(rworldmap)

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

#do.call(addMapLegend, c(mapParams, legendWidth=1.0, legendIntervals="page", legendMar=3))
#labelCountries()

country_coord <-data.frame(coordinates(matched),stringsAsFactors=F)
country <- na.omit(matched@data$PL)
country <- str_to_title(country)
country[35] <- "United States of America"
country_coord <-  country_coord[country,]

text(x=country_coord$X1,y=country_coord$X2,labels=row.names(country_coord), cex = 0.7)

teste %>% 
  filter(DEP >= '2020-01-01',
         !is.na(PL)) %>% 
  group_by(PL) %>% 
  summarise(n = n()) %>% 
  mutate(country = str_to_title(PL)) %>% 
  hcmap(.,
      map = "custom/world",
      download_map_data = FALSE,
      value = "n",
      joinBy = c("country", "name"),
      name = "Count by Countries",
      dataLabels = list(enabled = TRUE, format = "{point.country}")
      )

get_data_from_map(download_map_data("custom/world"))


teste %>% 
  filter(DEP >= '2020-01-01') %>% 
  group_by(PL) %>% 
  mutate(year = year(DEP),
         week = isoweek(DEP)) %>% 
  select(DEP, year, week)

teste %>% 
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
  hc_xAxis(title = list(text = "Publication Date")) %>% 
  hc_subtitle(text = "Primary Language of Publication") %>% 
  hc_legend(align = "center")

install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")

library(tm)
library(SnowballC)
library(wordcloud)

auxCorpus <- VCorpus(VectorSource(teste$RN))
auxCorpus <- tm_map(auxCorpus, PlainTextDocument)
auxCorpus <- tm_map(auxCorpus, removePunctuation) 

auxCorpus <- tm_map(auxCorpus, removeWords, stopwords('en')) 
auxCorpus <- tm_map(auxCorpus, removeNumbers)
auxCorpus <- tm_map(auxCorpus, stemDocument)
wordcloud(auxCorpus,max.words=50,colors=c("blue","red", "green", "orange", "yellow", "black"))

auxCorpus <- VCorpus(VectorSource(teste$SO))


auxCorpus <- tm_map(auxCorpus, PlainTextDocument)
auxCorpus <- tm_map(auxCorpus, removePunctuation) 

auxCorpus <- tm_map(auxCorpus, removeWords, stopwords('en')) 
auxCorpus <- tm_map(auxCorpus, removeNumbers)
auxCorpus <- tm_map(auxCorpus, stemDocument)
wordcloud(auxCorpus,max.words=50,colors=c("blue","red", "green", "orange", "yellow", "black"))



