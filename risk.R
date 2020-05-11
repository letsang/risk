library(dplyr)
library(shiny)
library(leaflet)
library(firebase)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}
             #controls{background-color:white;padding:20px;}"),
  
  useFirebase(), # import dependencies
  useFirebaseUI(),
  leafletOutput('map', height = '100%', width = '100%'),
# reqSignin will hide the content until the user signs in, but not safe.
  reqSignin(absolutePanel(top = 10, right = 10, id = "controls",
                checkboxInput("legend", "Show legend", TRUE)))
)

server <- function(input, output){
  f <- FirebaseUI$
    new()$ # instantiate
    set_providers(email = TRUE)$
    launch() # launch
  
  output$screen <- renderUI({
    f$req_sign_in() # require sign in
    mainUI()
  })
  
  output$map <- renderLeaflet({
    f$req_sign_in() # require sign in
    leaflet() %>%
      addProviderTiles("Stamen.Watercolor", options = providerTileOptions(minZoom = 2, maxZoom = 10)) %>%
      setView(lng = 0, lat = 40, zoom = 3) %>%
      setMaxBounds(lng1 = -180, lat1 = 90, lng2 = 180, lat2 = -90)
  })
}

shinyApp(ui, server)