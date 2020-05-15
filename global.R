library(data.table)
library(tidyr)
library(dplyr)
library(shiny)
library(leaflet)
library(sf)
library(firebase)
library(fireData)
library(httr)

  ############################## MAP DATA ##############################

countries <- read.csv(file = "dataset/countries.csv", stringsAsFactors = FALSE, na.strings = "N/A")
regions <- merge(spData::world, countries, by = "iso_a2", full = TRUE) %>%
           select(id = iso_a2, continent, region = region_un, subregion, name = name_long, latitude, longitude, geometry) %>%
           group_by(subregion)
game <- regions %>%
        summarize(latitude = median(as.numeric(latitude)), longitude = median(as.numeric(longitude))) %>%
        select(subregion, latitude, longitude) %>%
        mutate(player = "p3", regiment = sample(1:5,1)) #random value for test, to be replaced by reactive input

  ############################## MARKER ICON ##############################

flagIcon <- iconList(p1 = makeIcon("https://raw.githubusercontent.com/letsang/risk/master/graphics/flag1.png", iconWidth = 24, iconHeight =32),
                     p2 = makeIcon("https://raw.githubusercontent.com/letsang/risk/master/graphics/flag2.png", iconWidth = 24, iconHeight =32),
                     p3 = makeIcon("https://raw.githubusercontent.com/letsang/risk/master/graphics/flag3.png", iconWidth = 24, iconHeight =32))