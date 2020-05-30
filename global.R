library(data.table)
library(plyr)
library(tidyr)
library(dplyr)
library(magrittr)
library(shiny)
library(leaflet)
library(sf)
library(httr)
library(googledrive)
library(googlesheets4)

  ############################## CONNECT TO GOOGLESHEET ##############################

ss <- "https://docs.google.com/spreadsheets/d/1EUyrdDC3_KAwlsa_jUTq9YWeRIJoCcA2dNHR3S3dbHw/edit?usp=sharing"
authModal <- modalDialog(
  size = "s",
  title = "Si vis pacem, para bellum",
  textInput("nickname","Enter your name :"),
  # textInput("email","Enter your email :"),
  footer = tagList(actionButton("run", "Run"), actionButton("quit", "Quit"))
)
quitModal <- modalDialog(
  size = "s",
  title = "Sorry, too many players!",
  img(src = "https://raw.githubusercontent.com/letsang/risk/master/graphics/busy.jpg", width = "100%"),
  footer = actionButton("close","Close")
)

  ############################## GENERATE MAP ##############################

countries <- read.csv(file = "dataset/countries.csv", stringsAsFactors = FALSE, na.strings = "N/A")
regions <- merge(spData::world, countries, by = "iso_a2", full = TRUE) %>%
           select(id = iso_a2, continent, region = region_un, subregion, name = name_long, latitude, longitude, geometry) %>%
           group_by(subregion)

map <- leaflet() %>%
  addProviderTiles("Stamen.Watercolor", options = providerTileOptions(minZoom = 2, maxZoom = 10)) %>%
  setView(lng = 0, lat = 40, zoom = 3) %>%
  setMaxBounds(lng1 = -180, lat1 = 90, lng2 = 180, lat2 = -90) %>% 
  addPolygons(data = summarize(regions), weight = 0, color = "white", smoothFactor = 0.000001,
              label = ~subregion, labelOptions = labelOptions(style = list("color" = "black",
                                                                           "font-family" = "serif",
                                                                           "font-style" = "italic",
                                                                           "font-size" = "15px")),
              fillColor = "black", fillOpacity = 0.1, layerId = ~subregion,
              highlightOptions = highlightOptions(fillColor = "white", bringToFront = TRUE)) %>%
  addControl(html = img(src = "https://raw.githubusercontent.com/letsang/risk/master/graphics/logorisk.png", height = "16px"),
             position = "bottomleft")

  ############################## LOAD GAME DATA ##############################
# Already preset in the googlesheet
# game <- regions %>%
#   summarize(latitude = median(as.numeric(latitude)), longitude = median(as.numeric(longitude))) %>%
#   select(subregion, latitude, longitude) %>%
#   mutate(player = "p3", regiment = sample(1:5,1)) #random value for test, to be replaced by reactive input

  ############################## MARKER ICON ##############################

flagIcon <- iconList(null = makeIcon("https://raw.githubusercontent.com/letsang/risk/master/graphics/null.png", iconWidth = 24, iconHeight =32),
                     p1 = makeIcon("https://raw.githubusercontent.com/letsang/risk/master/graphics/flag1.png", iconWidth = 24, iconHeight =32),
                     p2 = makeIcon("https://raw.githubusercontent.com/letsang/risk/master/graphics/flag2.png", iconWidth = 24, iconHeight =32),
                     p3 = makeIcon("https://raw.githubusercontent.com/letsang/risk/master/graphics/flag3.png", iconWidth = 24, iconHeight =32))

  ############################## LANDSCAPE ILLUSTRATIONS ##############################

landImg <- list("Antarctica" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/antarctica.jpg",
                "Australia and New Zealand" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/australia.jpg",
                "Caribbean" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/carribean.jpeg",
                "Central America" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/centralamerica.jpg",
                "Central Asia" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/centralasia.jpg",
                "Eastern Africa" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/eastafrica.jpg",
                "Eastern Asia" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/eastasia.jpg",
                "Eastern Europe" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/esteurope.jpg",
                "Melanesia" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/melanasia.jpeg",
                "Middle Africa" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/midafrica.jpg",
                "Northern Africa" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/northafrica.jpg",
                "Northern America" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/northamerica.jpg",
                "Northern Europe" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/northeurope.jpg",
                "Seven seas (open ocean)" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/sevenseas.jpg",
                "South America" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/southamerica.jpg",
                "South-Eastern Asia" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/southeastasia.jpg",
                "Southern Africa" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/southafrica.jpg",
                "Southern Asia" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/southasia.jpg",
                "Southern Europe" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/southeurope.jpeg",
                "Western Africa" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/westafrica.jpg",
                "Western Asia" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/westasia.jpg",
                "Western Europe" = "https://raw.githubusercontent.com/letsang/risk/master/graphics/landscape/westeurope.jpg")
