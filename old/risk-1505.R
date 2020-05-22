source(file = "global.R")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%}
                    #controls {background-color:white;border-style:double;opacity:0.5;padding:0px 20px;width:25%;font-family:serif;font-style:italic;}
                    #controls:hover {opacity:1.0;}"),
  # useFirebase(), # import dependencies
  # useFirebaseUI(),
  leafletOutput('map', height = '100%', width = '100%'),
  # reqSignin will hide the content until the user signs in, but not safe.
  #reqSignin(
    absolutePanel(top = 10, right = 10, id = "controls",
                  h3("Player 1"),
                  h5("What is your next step ?"),
                  br(),
                  actionButton("attack", label = "Attack"),
                  actionButton("move", label = "Move"),
                  actionButton("pass", label = "Next"),
                  br(),
                  br(),
                  )
  #)
)

server <- function(input, output){
  # f <- FirebaseUI$
  #   new()$ # instantiate
  #   set_providers(email = TRUE)$
  #   launch() # launch
  
  output$map <- renderLeaflet({
  # f$req_sign_in() # require sign in
    leaflet() %>%
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
      addMarkers(data = as.data.table(game),
                 icon = flagIcon["p2"],
                 label = ~as.character(regiment),
                 labelOptions = labelOptions(noHide = TRUE, direction = "bottom",
                                             style = list("color" = "black",
                                                          "font-family" = "serif",
                                                          "font-style" = "italic",
                                                          "font-size" = "15px",
                                                          "height" = "25px",
                                                          "width" = "25px",
                                                          "border-color" = "black")
                 )
      ) %>%
      addControl(html = img(src = "https://raw.githubusercontent.com/letsang/risk/master/graphics/logorisk.png", height = "16px"),
                 position = "bottomleft")
  })
}

shinyApp(ui, server)