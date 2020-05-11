library(dplyr)
library(shiny)
library(leaflet)
library(firebase)

mainUI <- function() {
  bootstrapPage(
    theme = shinythemes::shinytheme('simplex'),
    leaflet::leafletOutput('map', height = '1000px', width = '100%'),
    absolutePanel(top = 10, left = 10, id = 'controls'),
    tags$style(type = "text/css", "html, body {width:100%;height:100%}
                   #controls{background-color:white;padding:20px;}")
  )
}

ui <- fluidPage(
  useFirebase(), # import dependencies
  useFirebaseUI(),
  reqSignin(h4("RISK")), # hide from UI
  htmlOutput("screen")
)

server <- function(input, output){
  f <- FirebaseUI$
    new()$ # instantiate
    set_providers(email = TRUE)$
    launch() # launch
  
  output$screen <- renderUI({
    f$req_sign_in() # require sign in
    div(do.call(bootstrapPage, c("", mainUI())))
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