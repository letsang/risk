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
                actionButton("init", label = "Initiate"),
                # actionButton("attack", label = "Attack"),
                # actionButton("move", label = "Move"),
                # actionButton("pass", label = "Next"),
                actionButton("exit", label = "Quit Game"),
                br(),
                br(),
  ),
  uiOutput("markers")
)

server <- function(input, output, session){
  output$map <- renderLeaflet(map)
  observeEvent(input$exit,{ #exit the game properly
    stopApp()
  })
  
  ############################## CONNECT TO GOOGLESHEET ##############################
  showModal(authModal)
  observeEvent(input$run,{
    gs4_token()
    player <- read_sheet(ss, "player")
    if (length(player$player) > 2) # limited to 3 players
    {
      showModal(quitModal)
    }
    else
    {
      removeModal()
      # initialize the player in googlesheets
      sheet_append(ss, data.frame(input$nickname, 20,
                                  0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0,
                                  0, 0, paste("p", length(player$player)+1, sep = "")
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
    initModal <- modalDialog(
      size = "s",
      title = input$map_shape_click$id,
      actionButton("minus","-"),
      dat$regiment,
      actionButton("plus","+"),
      easyClose = TRUE
    )
    if (dat$occupied == FALSE) # OR dat$player == ACTUAL PLAYER
    {
      showModal(initModal)
    }
  })
  
  
  # observeEvent(input$init, {
  #   dat <- read_sheet(ss, 1) %>% filter(occupied == FALSE)
  #   player <- read_sheet(ss, 2)
  #   showModal(modalDialog(
  #     selectInput("init_region", "Where would you send your regiment ?", choices = dat$subregion),
  #     numericInput("init_regiment", "How many regiment ?", value = min(player$regiment, min = min(player$regiment), max = max(player$regiment))),
  #     actionButton("exe_init", "Execute")
  #   ))
  # })
  # 
  # observeEvent(input$exe_init, {
  #   removeModal()
  #   range_write(ss, data = data.frame(input$init_regiment), range = "F9", col_names = FALSE)
  # })
  
  ############################## MOVE TROOPS ##############################
  # observeEvent(input$move, {
  #   dat <- read_sheet(ss)
  #   showModal(modalDialog(
  #     selectInput("move_from", "From", choices = dat$subregion),
  #     selectInput("move_to", "to", choices = dat$subregion),
  #     numericInput("move_regiment", "move", value = 1, min = 1, max = 2),
  #     actionButton("exe_move", "Execute")
  #   ))
  # })
  # 
  # observeEvent(input$exe_move, {
  #   removeModal()
  #   range_write(ss, data = data.frame(input$move_regiment), range = "F9", col_names = FALSE)
  # })

}

shinyApp(ui, server)