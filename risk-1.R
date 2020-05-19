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
  )
)

server <- function(input, output){
  output$map <- renderLeaflet(map)
  observeEvent(input$exit,{
    stopApp()
  })
  ############################## CONNECT TO GOOGLESHEET ##############################
  showModal(authModal)
  observeEvent(input$run,{
    gs4_token()
    ss <- "https://docs.google.com/spreadsheets/d/1EUyrdDC3_KAwlsa_jUTq9YWeRIJoCcA2dNHR3S3dbHw/edit?usp=sharing"
    dat <- read_sheet(ss,1)
    player <- read_sheet(ss,2)
    if (length(player$player) > 2){
      showModal(quitModal)
    }else{
      removeModal()
      sheet_append(ss, data.frame(input$nickname, 20), "player")
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
  
  ############################## INIT TROOPS ##############################
  observeEvent(input$init, {
    dat <- read_sheet(ss, 1) %>% filter(occupied == FALSE)
    player <- read_sheet(ss, 2)
    showModal(modalDialog(
      selectInput("init_region", "Where would you send your regiment ?", choices = dat$subregion),
      numericInput("init_regiment", "How many regiment ?", value = min(player$regiment, min = min(player$regiment), max = max(player$regiment))),
      actionButton("exe_init", "Execute")
    ))
  })
  
  observeEvent(input$exe_init, {
    removeModal()
    range_write(ss, data = data.frame(input$init_regiment), range = "F9", col_names = FALSE)
  })
  
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

  ############################## UPDATE MARKERS ##############################
  observeEvent(input$map_shape_click, {
    dat <- read_sheet(ss)
    leafletProxy("map") %>% clearMarkers() %>%
      addMarkers(data = as.data.table(dat),
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
      )
  })
}

shinyApp(ui, server)