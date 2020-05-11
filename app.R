library(shiny)
library(httr)

authUI <- function() {
    fluidPage(
        # Application title
        tags$head(
            tags$link(rel = "stylesheet",
                      type = "text/css",
                      href = "styles.css"),
            tags$title("Login")
        ),
        div(
            id = "login",
            wellPanel(
                textInput("email", "Email"),
                passwordInput("password", "Password"),
                tags$br(),
                actionButton("loginButton", "Log in"),
                actionButton("signupButton", "Sign up")
            )
        ),
        div(id = "authStatus",
            h3(textOutput("status")),
            br(),
            h3(textOutput("body")))
    )
}
mainUI <- function() {
    fluidPage("hello, world!")
}

ui <- (htmlOutput("screen"))


server <- function(input, output, session) {
    isAuth <- reactiveVal(value = FALSE)
    
    observe({
      if (isAuth() == FALSE)
      {
          output$screen <- renderUI({div(do.call(bootstrapPage, c("", authUI())))
          })
      }
      else if (isAuth() == TRUE)
      {
          output$screen <- renderUI({div(do.call(bootstrapPage, c("", mainUI())))
          })
      }
    })
    
    
    
    # LOGIN
    observeEvent(input$loginButton, {
        authEmail <- input$email
        authPassword <- input$password
        response <- POST("https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=AIzaSyDd_bqdAJnkRPoU2eLLMCIA0CUvvoPsmOE",
                         body = list(email = toString(authEmail),
                                     password = toString(authPassword),
                                     returnSecureToken = TRUE),
                         encode = "json")
        status <- http_status(response)$category
        body <- content(response, "text")
        output$status <- renderText(status)
        output$body <- renderText(body)
        if (status == 'Success' &&
                      gregexpr(pattern = '"idToken"', text = body) > 0)
        {
            isAuth(TRUE)
        }
        else
        {
            print(status)
            print(body)
            isAuth(FALSE)
        }
    })
    
    # SIGNUP
    observeEvent(input$signupButton, {
        authEmail <- input$email
        authPassword <- input$password
        response <-POST("https://identitytoolkit.googleapis.com/v1/accounts:signUp?key=AIzaSyDd_bqdAJnkRPoU2eLLMCIA0CUvvoPsmOE",
                        body = list(email = toString(authEmail),
                                    password = toString(authPassword),
                                    returnSecureToken = TRUE),
                        encode = "json")
        status <- http_status(response)$category
        body <- content(response, "text")
        output$status <- renderText(status)
        output$body <- renderText(body)
        if (status == 'Success' &&
                      gregexpr(pattern = '"idToken"', text = body) > 0)
        {
            print(status)
            print(body)
            isAuth(TRUE)
        }
        else
        {
            print(status)
            print(body)
            start <- gregexpr(pattern = '"message"', text = body)[[1]][1] + 11
            end <- start + 30
            errorMessage <- substr(body, start, end)
            message <- paste('Not successfull Authentication with error',
                             errorMessage,
                             sep = " -> ")
            isAuth(FALSE)
            output$message <- renderText(message)
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
