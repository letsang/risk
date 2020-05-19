library(data.table)
library(plyr)
library(tidyr)
library(dplyr)
library(shiny)
library(leaflet)
library(sf)
library(httr)
library(googlesheets4)

  ############################## CONNECT TO GOOGLESHEET ##############################

authModal <- modalDialog(
  title = "Si vis pacem, para bellum",
  textInput("nickname","What is your name ?"),
  footer = tagList(actionButton("run", "Run"), actionButton("quit", "Quit"))
)
quitModal <- modalDialog(
  title = "Sorry, too many players!",
  img(src = "https://raw.githubusercontent.com/letsang/risk/master/graphics/busyModal.jpg", width = "100%"),
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
              fillColor = "black", fillOpacity = 0.1,
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

flagIcon <- iconList(p1 = makeIcon("https://raw.githubusercontent.com/letsang/risk/master/graphics/flag1.png", iconWidth = 24, iconHeight =32),
                     p2 = makeIcon("https://raw.githubusercontent.com/letsang/risk/master/graphics/flag2.png", iconWidth = 24, iconHeight =32),
                     p3 = makeIcon("https://raw.githubusercontent.com/letsang/risk/master/graphics/flag3.png", iconWidth = 24, iconHeight =32))