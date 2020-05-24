source(file = "global.R")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%}
                    #controls {background-color:white;border-style:double;opacity:0.5;padding:0px 20px;width:25%;font-family:serif;font-style:italic;}
                    #controls:hover {opacity:1.0;}"),
  leafletOutput('map', height = '100%', width = '100%'),
  absolutePanel(top = 10, right = 10, id = "controls",
                h3(textOutput("nickname")),
                h5("What is your next step ?"),
                br(),
                # actionButton("init", label = "Initiate"),
                # actionButton("attack", label = "Attack"),
                # actionButton("pass", label = "Next"),
                actionButton("exit", label = "Quit Game"),
                br(),
                br(),
  ),
  uiOutput("markers")
)

server <- function(input, output, session){
  output$map <- renderLeaflet(map)
  
  rv <- reactiveValues( #reactive value to be used as global variable
    playerID = character()
  )

  ############################## CONNECT TO GOOGLESHEET ##############################
  showModal(authModal)
  observeEvent(input$run,{
    gs4_auth(cache = ".secrets", email = TRUE)
    player <- read_sheet(ss, "player")
    if (length(player$player) > 2) # limited to 3 players
    {
      showModal(quitModal)
    }
    else
    {
      removeModal()
      # initialize the player in googlesheets
      rv$playerID <- paste("p", length(player$player)+1, sep = "") #assigning player id to the reactive value
      sheet_append(ss, data.frame(input$nickname, 20,
                                  0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0,
                                  0, 0, rv$playerID
                                  ),
                   "player")
    }
  })
  output$nickname <- renderText({
    input$nickname
  })
  observeEvent(input$close,{ #close authModal
    stopApp()
  })
  observeEvent(input$quit,{ #close quitModal
    stopApp()
  })
  
  ############################## UPDATE MARKERS ##############################
  # * Quota exceeded for quota group 'ReadGroup' and limit 'Read requests per user per 100 seconds' of service 'sheets.googleapis.com' for consumer 'project_number:603366585132'.
  dat <- reactive({
    invalidateLater(3000)
    tmp <- read_sheet(ss)
    as.data.table(tmp)
  })
  markers <- reactive({
  leafletProxy("map") %>% clearMarkers() %>%
    addMarkers(data = dat(),
               icon = ~flagIcon[player],
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
    )
  })
  output$markers <- renderUI(markers())
  
  ############################## MOOVING TROOPS ##############################
  observeEvent(input$map_shape_click,{
    dat <- dat() %>% filter(subregion == input$map_shape_click$id)
    player <- read_sheet(ss, "player")
    moveModal <- modalDialog(size = "s",
                             title = input$map_shape_click$id,
                             numericInput("retreat", "Retreat - :", width = "100px", value = 0, min = 0, max = ifelse(dat$regiment > 0, max(dat$regiment) - 1, max(dat$regiment))),
                             numericInput("charge", "Charge + :", width = "100px", value = 0, min = 0, max = max(player$regiment)),
                             footer = actionButton("move","Move"),
                             easyClose = TRUE)
    if (dat$occupied == FALSE | dat$player == rv$playerID) # OR dat$player == ACTUAL PLAYER
    {
      showModal(moveModal)
    }
  })
  
  observeEvent(input$move,{
    player <- read_sheet(ss, "player")
    player[player$id == rv$playerID, input$map_shape_click$id] <- player[player$id == rv$playerID, input$map_shape_click$id] + (input$charge - input$retreat)
    player[player$id == rv$playerID, "regiment"] <- player[player$id == rv$playerID, "regiment"] - (input$charge - input$retreat)
    write_sheet(player, ss, "player")
  })
  
  ############################## QUIT GAME ##############################
  observeEvent(input$exit,{ #exit the game properly
    player <- player[0, ]
    write_sheet(player, ss, "player")
    stopApp()
  })
}

shinyApp(ui, server)