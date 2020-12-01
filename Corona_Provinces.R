---
  title: "Leaflet"
author: "Nhyu Joshi"
date: "18th July, 2020"
output: html_document
---

## Load libraries


library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(leaflet)
library(geojsonio)
library(tigris)

### Pull province data from Wikipedia


webpage <- read_html("https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Nepal")
covid19_nepal <- webpage %>% 
  html_node(xpath = '//*[@id="covid19-container"]/table') %>% 
  html_table(fill = TRUE)
head(covid19_nepal)


### Rename columns and delete first row


names(covid19_nepal) <- c("location", "date", "cases", "deaths")#"recovered"
## delete first row
covid19_nepal <- covid19_nepal[-1,]


### Remove date column and Filter Province

covid19_nepal <- covid19_nepal %>% select(location, cases, deaths) %>% #recovered
  filter(str_detect(location, "^Province")| 
           str_detect(location, "Bagmati")|
           str_detect(location, "Gandaki")|
           str_detect(location, "Karnali")|
           str_detect(location, "Sudurpashchim"))


### Add P_ID column

covid19_nepal <- covid19_nepal %>% mutate(P_ID = row_number())


### Pull province map data from github & joining with covid19_data

province_map <- topojson_read("https://raw.githubusercontent.com/rugnepal/learnRgroup/master/week_13/province.topojson") %>%
  arrange(D_ID)

province_map_joined <- geo_join(province_map, covid19_nepal, 'D_ID', 'P_ID')

province_map_joined <- province_map_joined %>% 
  mutate(cases = as.numeric(gsub(",","",province_map_joined$cases)),
         #recovered = as.numeric(gsub(",","",province_map_joined$recovered)), 
         deaths = as.numeric(gsub(",","",province_map_joined$deaths)), 
         cases_fac = as.factor(gsub(",","",province_map_joined$cases)), 
         #recovered_fac = as.factor(gsub(",","",province_map_joined$recovered)), 
         deaths_fac = as.factor(gsub(",","",province_map_joined$deaths)))

str(province_map_joined)

### Visualising Corona's effect on Nepal's Provinces 

palCases <- colorFactor("Paired", NULL)
#palRecovered <- colorFactor("Dark2", NULL)
palDeaths <- colorFactor("Accent", NULL)

labels <- sprintf(
  "<strong>%s</strong> <br/>%g cases <br/>%g deaths",#%g recovered <br/>
  province_map_joined$location, province_map_joined$cases, #province_map_joined$recovered, 
  province_map_joined$deaths) %>% lapply(htmltools::HTML)

province_map_joined %>% 
  leaflet() %>%
  setView(lat = 28.3949, lng = 84.1240, zoom = 7) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(label = labels , 
              color = '#444444', 
              weight = 1, smoothFactor = 0.5, 
              opacity = 1, fillOpacity = 0.50, 
              fillColor = ~palCases(cases_fac), 
              highlightOptions = highlightOptions(color = "white", 
                                                  weight = 2,bringToFront = TRUE), 
              group = "Cases") %>% 
  #addPolygons(label = labels , 
  #color = '#444444', 
  #weight = 1, smoothFactor = 0.5, 
  #opacity = 1, fillOpacity = 0.50, 
  #fillColor = ~palRecovered(recovered_fac), 
  #highlightOptions = highlightOptions(color = "white", 
  #weight = 2,bringToFront = TRUE),
  #group = "Recovered") %>%
  addPolygons(label = labels , 
              color = '#444444', 
              weight = 1, smoothFactor = 0.5, 
              opacity = 1, fillOpacity = 0.50, 
              fillColor = ~palDeaths(deaths_fac), 
              highlightOptions = highlightOptions(color = "white", 
                                                  weight = 2,bringToFront = TRUE),
              group = "Deaths") %>%
  addLegend(pal = palCases, 
            values = ~cases_fac, 
            opacity = 0.75, 
            title = "Cases", 
            position = "bottomright",
            group = "CasesLeg") %>%
  #addLegend(pal = palRecovered, 
  #values = ~recovered_fac, 
  #opacity = 0.75, 
  #title = "Recovered", 
  #position = "bottomright",
  #group = "RecoveredLeg") %>%
  addLegend(pal = palDeaths, 
            values = ~deaths_fac, 
            opacity = 0.75, 
            title = "Death", 
            position = "bottomright",
            group = "DeathLeg") %>%
  addLayersControl(baseGroups = c("Cases", "Deaths"), #"Recovered"
                   overlayGroups = c("CasesLeg", "DeathLeg"),#"RecoveredLeg",
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("DeathLeg"))#"RecoveredLeg",
