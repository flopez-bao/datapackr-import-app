library(shiny)
library(futile.logger)
library(shinyWidgets)
library(datimutils)
library(shinyjs)

# js ----
# allows for using the enter button
jscode <- '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#login_button").click();
}});'

# allowed users ----
USER = "Global"

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # data ----
  user <- reactiveValues(type = NULL)
  route <- reactiveValues(route = "triage")
  
  # user information
  user_input  <-  reactiveValues(
    authenticated = FALSE,
    status = "",
    d2_session = NULL,
    memo_authorized = FALSE
  )
  
  # ui ----
  
  # login page with username and password
  output$uiLogin  <-  renderUI({
    fluidPage(
      tags$head(tags$script(HTML(jscode))),
      wellPanel(
      fluidRow(
        h4(
          "Use this app to test DATIM imports in triage and conduct DATIM imports in production:"
        ),
        br()
      ),
      fluidRow(
        selectInput("server", "Choose Server", choices = c("triage", "prod")),
        textInput("user_name", "Username: ", width = "500px"),
        passwordInput("password", "Password:", width = "500px"),
        actionButton("login_button", "Log in!")
      )
    ))
  })
  
  # is the user authenticated
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      uiOutput("uiLogin")
    } else {
      uiOutput("authenticated")
    }
  })
  
  # main screen
  output$authenticated <- renderUI({
    fluidPage(
      sidebarPanel(
        id = "side-panel",
        fileInput(
          "file1",
          "Choose DataPack (Must be XLSX!):",
          accept = c("application/xlsx",
                     ".xlsx"),
          width = "240px"
        ),
        # unpack and return all the warnings and messages
        actionButton("validate", "Validate"),
        # import into the respective server
        actionButton("import", "Import")
      ),
      fluidRow(h1(
      paste0(
        "You are currently logged into the ",
        input$server,
        " DATIM server."
      )
    )),
    fluidRow(column(
      actionButton("logout_button", "Log out of Session", style = "color: #fff; background-color: #FF0000; border-color: #2e6da4"),
      width = 6
    )))
  })
  
  # actions ----
  
  ## choose server ----
  observeEvent(input$server, {
    route$route <- input$server
    if (input$server == "prod") {
      showModal(modalDialog(
        title = "IMPORTANT",
        "You are attempting to log into production, please make sure you have tested this datapack in triage!!!",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  ## login process ----
  observeEvent(input$login_button, {
    
    print(paste0("login into...", input$server))
    
    tryCatch({
      if (route$route == "triage") {
        
        datimutils::loginToDATIM(
          base_url = Sys.getenv("TRIAGE_BASE_URL"),
          username = input$user_name,
          password = input$password,
          d2_session_envir = parent.env(environment())
        )
        
        ### check user rights and kick them out if not allowed ----
        # store data so call is made only once
        user$type <- datimutils::getMyUserType()
        
        # if a user is not to be allowed deny them entry
        if (user$type != USER) {
          
          # alert the user they cannot access the app
          sendSweetAlert(
            session,
            title = "YOU CANNOT LOG IN",
            text = "You are not authorized to use this application",
            type = "error"
          )
          
          # log them out
          Sys.sleep(3)
          flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
          user_input$authenticated  <-  FALSE
          user_input$user_name <- ""
          user_input$authorized  <-  FALSE
          user_input$d2_session  <-  NULL
          d2_default_session <- NULL
          gc()
          session$reload()
          
        }
        
      } else if (route$route == "prod") {
        
        datimutils::loginToDATIM(
          base_url = Sys.getenv("PROD_BASE_URL"),
          username = input$user_name,
          password = input$password,
          d2_session_envir = parent.env(environment())
        )
        
        ### check user rights and kick them out if not allowed ----
        # store data so call is made only once
        user$type <- datimutils::getMyUserType()
        
        # if a user is not to be allowed deny them entry
        if (user$type != USER) {
          
          # alert the user they cannot access the app
          sendSweetAlert(
            session,
            title = "YOU CANNOT LOG IN",
            text = "You are not authorized to use this application",
            type = "error"
          )
          
          # log them out
          Sys.sleep(3)
          flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
          user_input$authenticated  <-  FALSE
          user_input$user_name <- ""
          user_input$authorized  <-  FALSE
          user_input$d2_session  <-  NULL
          d2_default_session <- NULL
          gc()
          session$reload()
          
        }
        
      }
    },
    # This function throws an error if the login is not successful
    error = function(e) {
      flog.info(paste0("User ", input$username, " login failed."), name = "datapack")
    })
    
    if (exists("d2_default_session")) {
      if (any(class(d2_default_session) == "d2Session")) {
        user_input$authenticated  <-  TRUE
        user_input$d2_session  <-  d2_default_session$clone()
        d2_default_session <- NULL
        
        
        # Need to check the user is a member of the PRIME Data Systems Group, COP Memo group, or a super user
        user_input$memo_authorized  <-
          grepl("VDEqY8YeCEk|ezh8nmc4JbX",
                user_input$d2_session$me$userGroups) |
          grepl("jtzbVV4ZmdP",
                user_input$d2_session$me$userCredentials$userRoles)
        flog.info(
          paste0(
            "User ",
            user_input$d2_session$me$userCredentials$username,
            " logged in."
          ),
          name = "datapack"
        )
      }
    } else {
      sendSweetAlert(session,
                     title = "Login failed",
                     text = "Please check your username/password!",
                     type = "error")
    }
  })
  
  ## logout process ----
  observeEvent(input$logout_button, {
    flog.info(
      paste0(
        "User ",
        user_input$d2_session$me$userCredentials$username,
        " logged out."
      )
    )
    user_input$authenticated  <-  FALSE
    user_input$user_name <- ""
    user_input$authorized  <-  FALSE
    user_input$d2_session  <-  NULL
    d2_default_session <- NULL
    gc()
    session$reload()
  })
}
