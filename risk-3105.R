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
    data <- dat()
      leafletProxy("map") %>% clearMarkers() %>%
        addMarkers(data = data,
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
                                              ),
                  group = "playerFlag"
                  ) %>%
        addMarkers(data = data[which(data$attacked == TRUE)],
                  icon = makeIcon(iconUrl = "https://raw.githubusercontent.com/letsang/risk/master/graphics/dicesred.gif", iconAnchorX = 20, iconAnchorY = 60),
                  options = markerOptions(clickable = TRUE),
                  group = "attackFlag"
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
    
    if ((mov$occupied == FALSE | mov$player == rv$playerID) & mov$attacked == FALSE) # OR mov$player == ACTUAL PLAYER
    {
      moveModal <- modalDialog(size = "s",
                               title = input$map_shape_click$id,
                               tags$img(src = landImg[input$map_shape_click$id], width = "100%"),
                               tags$i(paste("Rule : This territory is free, place 1 or more regiments to make it yours.")),
                               numericInput("retreat", "Retreat - :", width = "100px", value = 0, min = 0, max = ifelse(mov$regiment > 0, max(mov$regiment) - 1, max(mov$regiment))),
                               numericInput("charge", "Charge + :", width = "100px", value = 0, min = 0, max = max(player$regiment)),
                               tags$i(paste("Remaining regiments : ", player$regiment[player$id == rv$playerID])),
                               footer = actionButton("move","Move"),
                               easyClose = TRUE)
      showModal(moveModal)
    }
    else if (mov$occupied == TRUE & mov$attacked == FALSE & mov$player != rv$playerID & any(attack$subregion %in% unlist(attackList)))
    {
      attackModal <- modalDialog(size = "s",
                                 title = paste("Enemy : ", input$map_shape_click$id),
                                 selectInput("attackFrom", "Attack from : ", choices = attack$subregion[which(attack$subregion %in% unlist(attackList) & attack$regiment > 2)]),
                                 tags$i("Rule : You need at least 3 regiments to attack the enemies territory."),
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
    data <- read_sheet(ss)
    data[data$subregion == input$map_shape_click$id, "attacked"] <- TRUE
    data[data$subregion == input$map_shape_click$id, "oponent"] <- input$attackNbRegiment
    data[data$subregion == input$map_shape_click$id, "from"] <- input$attackFrom
    range_write(ss, data[, 7:11], "game", range = "G1:K23")
    
    player <- read_sheet(ss, "player")
    player[player$id == rv$playerID, input$attackFrom] <- player[player$id == rv$playerID, input$attackFrom] - as.numeric(input$attackNbRegiment)
    write_sheet(player, ss, "player")
    removeModal()
    attackModal2 <- modalDialog(size = "s",
                               title = "Alea jacta est",
                               tags$i("Rule : You are attacking enemies territory, the die has been cast. The enemie will have the choice to defend his territory with 1 or 2 regiments."),
                               tags$img(src = "https://raw.githubusercontent.com/letsang/risk/master/graphics/attack2.jpg", width = "100%"),
                               tags$i("Hint : The more dice you roll, the higher your odds of winning. Yet the more dice you roll, the more armies you may lose, or be required to move into a captured territory."),
                               footer = modalButton("Close"),
                               easyClose = TRUE)
    showModal(attackModal2)
  })

  #   sort(sample(c(1:6), input$attackNbRegiment, replace = TRUE), decreasing = TRUE)
  observeEvent(input$map_marker_click,{
    data <- dat()
    if (input$map_marker_click$group == "attackFlag" & (rv$playerID %in% data$player) & data[data$latitude == input$map_marker_click$lat, "player"] == rv$playerID)
    {
      alertAttack <- modalDialog(size = "s",
                                 title = data[data$latitude == input$map_marker_click$lat, "subregion"],
                                 tags$i("Rule : Your territory has been attacked ! Fight the enemies with 1 or 2 regiment(s)."),
                                 tags$img(src = "https://raw.githubusercontent.com/letsang/risk/master/graphics/defend.jpg", width = "100%"),
                                 selectInput("defendNbRegiment", "Send regiments : ", choices = c(1:ifelse(data[data$latitude == input$map_marker_click$lat, "regiment"] > 1, 2, 1)), selectize = FALSE, size = 2),
                                 tags$i("Hint : The more dice the defender rolls, the higher his or her odds of winning."),
                                 footer = actionButton("defend","Defend"),
                                 easyClose = TRUE)
      showModal(alertAttack)
    }
  })
  
  ############################## QUIT GAME ##############################
  observeEvent(input$exit,{ #exit the game
    player <- read_sheet(ss, "player")
    player <- player[0, ]
    write_sheet(player, ss, "player")
    
    data <- read_sheet(ss)
    data[, "attacked"] <- FALSE
    data[, "oponent"] <- 0
    data[, "from"] <- ""
    data[, "defended"] <- FALSE
    data[, "defender"] <- 0
    range_write(ss, data[, 7:11], "game", range = "G1:K23")
    stopApp()
  })
}

shinyApp(ui, server)