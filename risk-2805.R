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
  
  ############################## MOOVING TROOPS AND/OR ATTACK ##############################
  observeEvent(input$map_shape_click,{
    dat <- dat()
    # for moving troops
    mov <- dat %>% filter(subregion == input$map_shape_click$id)
    player <- read_sheet(ss, "player")
    # for attack move
    attackList <- read_sheet(ss, "attack") %>% select(input$map_shape_click$id)
    attack <- dat %>% filter(player == rv$playerID)
    
    if (mov$occupied == FALSE | mov$player == rv$playerID) # OR mov$player == ACTUAL PLAYER
    {
      moveModal <- modalDialog(size = "s",
                               title = input$map_shape_click$id,
                               numericInput("retreat", "Retreat - :", width = "100px", value = 0, min = 0, max = ifelse(mov$regiment > 0, max(mov$regiment) - 1, max(mov$regiment))),
                               numericInput("charge", "Charge + :", width = "100px", value = 0, min = 0, max = max(player$regiment)),
                               footer = actionButton("move","Move"),
                               easyClose = TRUE)
      showModal(moveModal)
    }
    else if (mov$occupied == TRUE & mov$player != rv$playerID & any(attack$subregion %in% unlist(attackList)))
    {
      attackModal <- modalDialog(size = "s",
                                 title = paste("Enemy : ", input$map_shape_click$id),
                                 selectInput("attackFrom", "Attack from : ", choices = attack$subregion[which(attack$subregion %in% unlist(attackList) & attack$regiment > 2)]),
                                 tags$i("Rule : You need at least 3 regiments on your territory to attack."),
                                 selectInput("attackNbRegiment", "Send regiments : ", choices = c(1:3), selectize = FALSE, size = 3),
                                 tags$img(src = "https://raw.githubusercontent.com/letsang/risk/master/graphics/beforeCharge.jpg", width = "100%"),
                                 tags$i("Rule : You cannot attack with more than 3 regiments."),
                                 footer = actionButton("attack","Attack"),
                                 easyClose = TRUE)
      showModal(attackModal)
    }
  })
  
  observeEvent(input$move,{
    player <- read_sheet(ss, "player")
    player[player$id == rv$playerID, input$map_shape_click$id] <- player[player$id == rv$playerID, input$map_shape_click$id] + (input$charge - input$retreat)
    player[player$id == rv$playerID, "regiment"] <- player[player$id == rv$playerID, "regiment"] - (input$charge - input$retreat)
    write_sheet(player, ss, "player")
    removeModal()
  })
  
  ############################## ATTACK PHASE ##############################
  observeEvent(input$attack,{
    removeModal()
  })
  
  ############################## QUIT GAME ##############################
  observeEvent(input$exit,{ #exit the game properly
    player <- read_sheet(ss, "player")
    player <- player[0, ]
    write_sheet(player, ss, "player")
    stopApp()
  })
}

shinyApp(ui, server)