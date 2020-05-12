library(dplyr)
library(shiny)
library(leaflet)
library(firebase)
library(sf)

regionMap <- spData::world %>% group_by(subregion) %>% summarize()

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%}
                    #controls {background-color:white; opacity:0.5; padding:20px;}
                    #controls:hover {opacity:0.8;}"),
  useFirebase(), # import dependencies
  useFirebaseUI(),
  leafletOutput('map', height = '100%', width = '100%'),
  # reqSignin will hide the content until the user signs in, but not safe.
  reqSignin(absolutePanel(tags$img(src="https://raw.githubusercontent.com/letsang/risk/master/logorisk2.png", height ="25px"),
                          top = 10, right = 10, id = "controls",
                          checkboxInput("legend", "Show legend", TRUE)
                          )
  )
)

server <- function(input, output){
  f <- FirebaseUI$
    new()$ # instantiate
    set_providers(email = TRUE)$
    launch() # launch
  
  output$map <- renderLeaflet({
    f$req_sign_in() # require sign in
    leaflet() %>%
      addProviderTiles("Stamen.Watercolor", options = providerTileOptions(minZoom = 2, maxZoom = 10)) %>%
      setView(lng = 0, lat = 40, zoom = 3) %>%
      setMaxBounds(lng1 = -180, lat1 = 90, lng2 = 180, lat2 = -90) %>% 
      addPolygons(data = regionMap, weight = 0, color = "white", smoothFactor = 0.000001,
                  fillColor = "black", fillOpacity = 0.1,
                  highlightOptions = highlightOptions(fillColor = "white", bringToFront = TRUE))
  })
}

shinyApp(ui, server)