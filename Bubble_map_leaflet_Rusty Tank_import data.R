##Load packages
# Library
library(tidyverse)
library(readxl)
library(lubridate)
library(DBI)
library(odbc)
library(keyring)
library(dbplyr)
library(writexl)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(grDevices)
library(colorRamps)

#import data from RQ_MCR_SITE.xlsx

# I use a K site for the data (since repo for the time being is on a public github)
# RQ_MCR_SITE<- read_excel("RQ_MCR_SITE.xlsx")
RQ_MCR_SITE<- read_excel("K:/Avdeling/214-Oseanografi/DHJ/Prosjekter/RAdb/Input_data/RQ_MCR_SITE.xlsx")

## Create A leaflet interactive map

# Create a color palette with handmade bins.
mybins=seq(from = -4, to = 7, by = 1)
mypalette = colorBin( palette="RdYlGn", domain=RQ_MCR_SITE$SUM_Q_AVG, na.color="transparent", bins=mybins, reverse = TRUE)

# Prepare the text for the tooltip:

# mytext=paste(
#   "SITE: ", RQ_MCR_SITE$SITE_CODE, "<br/>",
#   "TAXON: ", RQ_MCR_SITE$SPECIES_GROUP, "<br/>",
#   "EFFECT: ", RQ_MCR_SITE$EFFECT_TYPE, "<br/>",
#   "ENDPOINT: ", RQ_MCR_SITE$ENDPOINT, "<br/>",
#   "RQ: ", formatC(RQ_MCR_SITE$SUM_Q_AVG, format = "e", digits = 1), "<br/>",
#   "MCR: ", round(RQ_MCR_SITE$MCR_Q_AVG, digits =1), "<br/>",
#   "CHEMICALS: ", RQ_MCR_SITE$TOTAL, sep="") %>%
#   lapply(htmltools::HTML)

#
# Adds tooltip text as variable. Then it also is included in each data set
# In addCircles(), refer to it as "~mytext" (~ iundicates it's a variable in the data set)
#
RQ_MCR_SITE$mytext <- paste(
  "SITE: ", RQ_MCR_SITE$SITE_CODE, "<br/>",
  "TAXON: ", RQ_MCR_SITE$SPECIES_GROUP, "<br/>",
  "EFFECT: ", RQ_MCR_SITE$EFFECT_TYPE, "<br/>",
  "ENDPOINT: ", RQ_MCR_SITE$ENDPOINT, "<br/>",
  "RQ: ", formatC(RQ_MCR_SITE$SUM_Q_AVG, format = "e", digits = 1), "<br/>",
  "MCR: ", round(RQ_MCR_SITE$MCR_Q_AVG, digits =1), "<br/>",
  "CHEMICALS: ", RQ_MCR_SITE$TOTAL, sep="")


# create subsets (groups) to populate the layers with data
Algae_Acute <- subset(RQ_MCR_SITE, SPECIES_GROUP == "Algae" 
                      & ENDPOINT == "EC50" 
                      & EFFECT_DESCRIPTION == "Mortality"
                      & EFFECT_TYPE == "ACUTE"
                      & TREND == "INC",
                      select = -CAMPAIGN
)
Crustaceans_Acute <- subset(RQ_MCR_SITE, SPECIES_GROUP == "Crustaceans"
                            & ENDPOINT == "EC50"
                            & EFFECT_DESCRIPTION == "Mortality"
                            & EFFECT_TYPE == "ACUTE"
                            & TREND == "INC",
                            select = -CAMPAIGN
)
Fish_Acute <- subset(RQ_MCR_SITE, SPECIES_GROUP == "Fish" 
                     & ENDPOINT == "EC50" 
                     & EFFECT_DESCRIPTION == "Mortality"
                     & EFFECT_TYPE == "ACUTE"
                     & TREND == "INC", 
                     select = -CAMPAIGN
)
Algae_Chronic <- subset(RQ_MCR_SITE, SPECIES_GROUP == "Algae" 
                        & ENDPOINT == "NOEC" 
                        & EFFECT_DESCRIPTION == "Growth"
                        & EFFECT_TYPE == "CHRONIC"
                        & TREND == "DEC", 
                        select = -CAMPAIGN
) 
Crustaceans_Chronic <- subset(RQ_MCR_SITE, SPECIES_GROUP == "Crustaceans" 
                              & ENDPOINT == "NOEC" 
                              & EFFECT_DESCRIPTION == "Growth"
                              & EFFECT_TYPE == "CHRONIC"
                              & TREND == "DEC", 
                              select = -CAMPAIGN
) 
Fish_Chronic <- subset(RQ_MCR_SITE, SPECIES_GROUP == "Fish" 
                       & ENDPOINT == "NOEC" 
                       & EFFECT_DESCRIPTION == "Growth"
                       & EFFECT_TYPE == "CHRONIC"
                       & TREND == "DEC", 
                       select = -CAMPAIGN
)

head(Fish_Chronic)    


# Final Map
map <- leaflet(RQ_MCR_SITE) %>% # add data from case
  addTiles(group = "OSM")  %>% # adds map group OSM 
  #setView( lat=-27, lng=170 , zoom=4) %>% #central point of map. test if not using if sentralized to points. Test: fitBounds(), setMaxBounds, 
  addProviderTiles("CartoDB", group = "Carto") %>% # add map view type 1
  addProviderTiles("Esri", group = "Esri") %>% # add map view type 2
  addProviderTiles("Esri.WorldImagery", group = "Esri_WI") %>% # add map view type 3
  
  addCircles(data = Algae_Acute, ~LONGITUDE, ~LATITUDE, fillColor = ~mypalette(log10(SUM_Q_AVG)), fillOpacity = 0.7, color="white", radius=~sqrt(TOTAL)*50,
             stroke=FALSE, weight = 1, popup = ~mytext, labelOptions = labelOptions( style = list("font-weight" = "normal",
                                                                                                  padding = "3px 8px"),textsize = "13px", direction = "auto"), group = "Algae_Acute") %>%
  addCircles(data = Crustaceans_Acute, ~LONGITUDE, ~LATITUDE, fillColor = ~mypalette(log10(SUM_Q_AVG)), fillOpacity = 0.7, color="white", radius=~sqrt(TOTAL)*50,
             stroke=FALSE, weight = 1, popup = ~mytext, labelOptions = labelOptions( style = list("font-weight" = "normal",
                                                                                                  padding = "3px 8px"),textsize = "13px", direction = "auto"), group = "Crustaceans_Acute") %>%
  addCircles(data = Fish_Acute, ~LONGITUDE, ~LATITUDE, fillColor = ~mypalette(log10(SUM_Q_AVG)), fillOpacity = 0.7, color="white", radius=~sqrt(TOTAL)*50,
             stroke=FALSE, weight = 1, popup = ~mytext, labelOptions = labelOptions( style = list("font-weight" = "normal",
                                                                                                  padding = "3px 8px"),textsize = "13px", direction = "auto"), group = "Fish_Acute") %>%
  addCircles(data = Algae_Chronic, ~LONGITUDE, ~LATITUDE, fillColor = ~mypalette(log10(SUM_Q_AVG)), fillOpacity = 0.7, color="white", radius=~sqrt(TOTAL)*50,
             stroke=FALSE, weight = 1, popup = ~mytext, labelOptions = labelOptions( style = list("font-weight" = "normal",
                                                                                                  padding = "3px 8px"),textsize = "13px", direction = "auto"), group = "Algae_Chronic") %>%
  addCircles(data = Crustaceans_Chronic, ~LONGITUDE, ~LATITUDE, fillColor = ~mypalette(log10(SUM_Q_AVG)), fillOpacity = 0.7, color="white", radius=~sqrt(TOTAL)*50,
             stroke=FALSE, weight = 1, popup = ~mytext, labelOptions = labelOptions( style = list("font-weight" = "normal",
                                                                                                  padding = "3px 8px"),textsize = "13px", direction = "auto"), group = "Crustaceans_Chronic") %>%
  addCircles(data = Fish_Chronic, ~LONGITUDE, ~LATITUDE, fillColor = ~mypalette(log10(SUM_Q_AVG)), fillOpacity = 0.7, color="white", radius=~sqrt(TOTAL)*50,
             stroke=FALSE, weight = 1, popup = ~mytext, labelOptions = labelOptions( style = list("font-weight" = "normal",
                                                                                                  padding = "3px 8px"),textsize = "13px", direction = "auto"), group = "Fish_Chronic") %>%
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri", "Esri_WI"), 
                   overlayGroups = c("Algae_Acute", "Crustaceans_Acute","Fish_Acute",
                                     "Algae_Chronic", "Crustaceans_Chronic","Fish_Chronic"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend( pal=mypalette, values=~log10(SUM_Q_AVG), opacity=0.9, title = "Risk Quotient (log)", 
             position = "bottomright", labFormat =labelFormat(between = " to ")) %>% 
  hideGroup(c("Algae_Acute", "Crustaceans_Acute", "Algae_Chronic", "Crustaceans_Chronic","Fish_Chronic"))  # showGroup doesn't work for some reason


map 



