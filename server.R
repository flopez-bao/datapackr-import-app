library(shiny)
library(futile.logger)
library(shinyWidgets)
library(datimutils)
library(shinyjs)
library(datapackr)
library(httr)
library(data.table)
library(shinydashboard)

options(
  "scipen" = 999,
  shiny.maxRequestSize=30*1024^2
  )

################ OAuth Client information #####################################
if (interactive()) {
  # testing url
  options(shiny.port = 3123)
  APP_URL <- "http://127.0.0.1:3123/"# This will be your local host path
} else {
  # deployed URL
  APP_URL <- Sys.getenv("APP_URL") #This will be your shiny server path
}

oauth_app <- httr::oauth_app(Sys.getenv("OAUTH_APPNAME"),
                             key = Sys.getenv("OAUTH_KEYNAME"),   # dhis2 = Client ID
                             secret = Sys.getenv("OAUTH_SECRET"), # dhis2 = Client Secret
                             redirect_uri = APP_URL)


oauth_api <- httr::oauth_endpoint(base_url = paste0(Sys.getenv("PROD_BASE_URL"), "uaa/oauth"),
                                  request = NULL, # Documentation says to leave this NULL for OAuth2
                                  authorize = "authorize",
                                  access = "token")

oauth_scope <- "ALL"


has_auth_code <- function(params) {
  
  return(!is.null(params$code))
}

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
  
  # data and values ----
  
  # results and msgs
  user <- reactiveValues(type = NULL)
  route <- reactiveValues(route = "triage")
  ready <- reactiveValues(ok = FALSE)
  validation_results <- reactiveValues(datapack = NULL, import = NULL)
  
  # import files json
  import_files <- reactiveValues()
  import_files_json <- reactiveValues()
  
  # user information
  user_input  <-  reactiveValues(
    authenticated = FALSE,
    status = "",
    d2_session = NULL,
    memo_authorized = FALSE
  )
  
  # ui ----
  
  ## redirect ----
  output$ui_redirect <- renderUI({
    #print(input$login_button_oauth) #useful for debugging
    if (!is.null(input$login_button_oauth)) {
      if (input$login_button_oauth > 0) {
        url <-
          httr::oauth2.0_authorize_url(oauth_api, oauth_app, scope = oauth_scope)
        print(url)
        redirect <- sprintf("location.replace(\"%s\");", url)
        tags$script(HTML(redirect))
      } else  {
        NULL
      }
    } else  {
      NULL
    }
  })
  
  ## auth switch ----
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      uiOutput("uiLogin")
      #uiOutput("authenticated")
      
    } else {
      #uiOutput("authenticated")
      uiOutput("authenticated")
    }
  })
  
  ## login screen ----
  output$uiLogin  <-  renderUI({
    # tabItems(
    #   tabItem(tabName = "login",
              fluidPage(
                titlePanel(title = "Datapack Import App"),
                tags$head(tags$script(HTML(jscode))),
                wellPanel(
                  fluidRow(
                    h4(
                      "Use this app to test DATIM imports in triage and conduct FULL DATIM imports in production;
                      currently only runs for datapacks and prod pointed to cop-test. Note that triage will allow
                      login via username and password but prod login will require oauth:"
                    ),
                    br()
                  ),
                  fluidRow(
                    selectInput("server", "Choose Server", choices = c("triage", "prod")),
                    #actionButton("login_button_oauth", "Log in with DATIM"),
                    uiOutput("login_button"),
                    uiOutput("ui_hasauth"),
                    uiOutput("ui_redirect")
                  )
                )
              )
      #         )
      # )
  })
  
  ## main screen ----
  output$authenticated <- renderUI({
    fluidPage(sidebarLayout(
      sidebarPanel(
        shinyjs::useShinyjs(),
        id = "side-panel",
        fileInput(
          "file1",
          "Choose DataPack (Must be XLSX!):",
          accept = c("application/xlsx",
                     ".xlsx"),
          width = "240px"
        ),
        h5("Import Process:"),
        fluidRow(
          # unpack and return all the warnings and messages
          shinyjs::disabled(
            actionButton("validate", "Validate")
          ),
          # import into the respective server
          shinyjs::disabled(
            actionButton("import", "Import")
          )
        ),
        h5("Download Results:"),
        fluidRow(
          # download import json files
          shinyjs::disabled(
            downloadButton("download", "Download Json Files") 
          ),
          # download console output
          shinyjs::disabled(
            downloadButton("download_i", "Download Console Output") 
          )
        ),
        br(),
        fluidRow(
          actionButton("logout_button", "Log out of Session", style = "color: #fff; background-color: #FF0000; border-color: #2e6da4")
        )
      ),
      mainPanel(
        fluidRow(h1(
        paste0(
          "You are currently logged into the ",
          input$server,
          " DATIM server ",
          "at ",
          user_input$d2_session$base_url
        )
      )
      ),
      uiOutput("info"),
      br(),
      fluidRow(
        uiOutput("messages")
        )
      )
    ))
  })
  
  ## datapack info ----
  output$info <- renderUI({
    if (!is.null(validation_results$datapack)) {
      fluidRow(
        h4("Tool loaded: ", validation_results$datapack$info$tool),
        h4("Name: ", validation_results$datapack$info$datapack_name),
        h4("COP Year: ", validation_results$datapack$info$cop_year)
      )
    }
    
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
  
  ## login triage ----
  # this is separated since triage login uses uname and pwd
  observeEvent(input$login_button, {
    tryCatch({
      print(paste0("login into...", input$server))
      
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
        sendSweetAlert(session,
                       title = "YOU CANNOT LOG IN",
                       text = "You are not authorized to use this application",
                       type = "error")
        
        # log them out
        Sys.sleep(3)
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
  
  ## login process ----
  observeEvent(input$login_button_oauth > 0, {
    
    print(paste0("login button status:", input$login_button_oauth))
    
    tryCatch({
      
      print(paste0(
        "login into...",
        input$server,
        " at ",
        Sys.getenv("PROD_BASE_URL")
      ))

      #Grabs the code from the url
      params <- parseQueryString(session$clientData$url_search)
      #Wait until the auth code actually exists
      req(has_auth_code(params))
      
      #Manually create a token
      token <- httr::oauth2.0_token(
        app = oauth_app,
        endpoint = oauth_api,
        scope = oauth_scope,
        use_basic_auth = TRUE,
        oob_value = APP_URL,
        cache = FALSE,
        credentials = httr::oauth2.0_access_token(
          endpoint = oauth_api,
          app = oauth_app,
          code = params$code,
          use_basic_auth = TRUE
        )
      )
      
      loginAttempt <- tryCatch({
        datimutils::loginToDATIMOAuth(
          base_url =  Sys.getenv("PROD_BASE_URL"),
          token = token,
          app = oauth_app,
          api = oauth_api,
          redirect_uri = APP_URL,
          scope = oauth_scope,
          d2_session_envir = parent.env(environment())
        )
        
      },
      # This function throws an error if the login is not successful
      error = function(e) {
        flog.info(paste0("User login failed. ", e$message), name = "datapack")
      })
      
      #print(loginAttempt)
      is_authorized <-
        if (!is.null(loginAttempt$token)) {
          TRUE
        } else {
          FALSE
        }
      #print(is_authorized)
      
      if (exists("d2_default_session") && is_authorized) {
        user_input$authenticated  <-  TRUE
        user_input$d2_session  <-  d2_default_session$clone()
        d2_default_session <- NULL
        
      } else {
        sendSweetAlert(session,
                       title = "Login Failed",
                       text = "You are not authorized to use this application",
                       type = "error")
      }
      
    },
    # This function throws an error if the login is not successful
    error = function(e) {
      flog.info(paste0("User ", input$username, " login failed."), name = "datapack")
    })
    
    
  })
  
  ## logout process ----
  observeEvent(input$logout_button, {
    req(input$logout_button)
    # Gets you back to the login without the authorization code at top
    updateQueryString("?", mode = "replace", session = session)
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
  

  
  # button management ----
  
  ## login button ----
  output$login_button <- renderUI({
    
    if(input$server != "prod") {
      fluidPage(
        textInput("user_name", "Username: ", width = "500px"),
        passwordInput("password", "Password:", width = "500px"),
        actionButton("login_button", "Log in!")
      )
    } else {
      actionButton("login_button_oauth", "Log in with DATIM")
    }
  })
  
  ## turn on validate ----
  observeEvent(input$file1, {
    shinyjs::enable("validate")
  })
  
  ## download button ----
  output$download_i <- downloadHandler(
    filename = function() {
      paste0(validation_results$datapack$info$datapack_name,'_console_output_', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      fwrite(validation_results$import, con)
    }
  )
  
  ## validation ----
  observeEvent(input$validate, {
    
    # disable validation
    shinyjs::disable("validate")
    
    print("beginning validation...")
    
    # store validation results
    validation_results$datapack <- validate(file = input$file1, d2_session = user_input$d2_session)
    
    # store raw files
    import_files$deletes <- validation_results$datapack$import_files$deletes
    import_files$main_import <- validation_results$datapack$import_files$main_import
    import_files$dedupes_00000 <- validation_results$datapack$import_files$dedupes_00000
    import_files$dedupes_00001 <- validation_results$datapack$import_files$dedupes_00001
    
    # generate json versions of the import files
    import_files_json$deletes_json <- prepJson(validation_results$datapack$import_files$deletes)
    import_files_json$main_import_json <- prepJson(validation_results$datapack$import_files$main_import)
    import_files_json$dedupes_00000_json <- prepJson(validation_results$datapack$import_files$dedupes_00000)
    import_files_json$dedupes_00001_json <- prepJson(validation_results$datapack$import_files$dedupes_00001)
    
    # enable import
    shinyjs::enable("import")
    
    if(!is.null(validation_results$datapack)) {
      showModal(modalDialog(
        title = "VALIDATION COMPLETE",
        "Your datapack has been unpacked and validated! Check the warnings and information below.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
  })
  
  ## import ----
  observeEvent(input$import, {
    #output$import_output <- renderPrint({
      shinyjs::disable("import")
      print("attempting import...")
      validation_results$import <- importToDatim(
        d = validation_results$datapack,
        server = input$server,
        import_data_json = import_files_json,
        import_data = import_files,
        d2session = user_input$d2_session
      )
      
      if(!is.null(validation_results$import)) {
        shinyjs::enable("download")
        shinyjs::enable("download_i")
        showModal(modalDialog(
          title = "IMPORT COMPLETE",
          "Your datapack has been imported. You can now download a console output report csv as well as json import files.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    #})
    
    
    
  })
  
  ## download data ----
  output$download <- downloadHandler(
    filename = function(){
      paste0(validation_results$datapack$info$datapack_name,"_import_files_", Sys.Date(), ".zip", sep = "")
      
    },
    content = function(file) {
      # go to a temp dir to avoid permission issues
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      dp_name <- validation_results$datapack$info$datapack_name
      
      # write deletes
      deletes_name <- 
        write(import_files_json$deletes_json[["pl"]], file.path(temp_directory, paste0("deletes_",dp_name,".json")))
      
      # write main import
      write(import_files_json$main_import_json[["pl"]], file.path(temp_directory, paste0("main_import_",dp_name,".json")))
      
      # write dedupes 00000
      write(import_files_json$dedupes_00000_json[["pl"]], file.path(temp_directory, paste0("dedupes_00000_",dp_name,".json")))
      
      # write dedupes 00001
      write(import_files_json$dedupes_00001_json[["pl"]], file.path(temp_directory, paste0("dedupes_00001_",dp_name,".json")))
      
      # create the zip file
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    }, contentType = "application/zip"
  )
  
  
  # results ----
  output$results <- renderUI({
    ir <- validation_results$import
    
    messages <- NULL
    
    if (is.null(ir)) {
      return(NULL)
    } else {
      unlist(as.character(ir))
    }
    
  })
  
  ## messages ----
  output$messages <- renderUI({
      
      vr <- validation_results$datapack
      
      messages <- NULL
      
      if (is.null(vr)) {
        return(NULL)
      }
      
      if (inherits(vr, "error")) {
        return(paste0("ERROR! ", vr$message))
        
      } else {
        
        messages <- vr %>%
          purrr::pluck(., "info") %>%
          purrr::pluck(., "messages")
        
        
        if (length(messages$message) > 0) {
          
          class(messages) <- "data.frame"
          
          messages %<>%
            dplyr::mutate(level = factor(level, levels = c("ERROR", "WARNING", "INFO"))) %>%
            dplyr::arrange(level) %>%
            dplyr::mutate(msg_html =
                            dplyr::case_when(
                              level == "ERROR" ~ paste('<li><p style = "color:red"><b>', message, "</b></p></li>"),
                              TRUE ~ paste("<li><p>", message, "</p></li>")
                            ))
          
          messages_sorted <-
            paste0("<ul>", paste(messages$msg_html, sep = "", collapse = ""), "</ul>")
          
          shiny::HTML(messages_sorted)
        } else {
          tags$li("No Issues with Integrity Checks: Congratulations!")
        }
      }
   
    
  })
}


# TESTING 
#observeEvent(input$test, {
# test reactive value is captured
#print(user_input$d2_session$base_url)
#print(deletes_json())
#print(deletes())
#})
# 
# 
# login sidebar
# output$ui_sidebar <- renderUI({
#   if (user_input$authenticated == FALSE) {
#     uiOutput("uiLoginSidebar")
#   } else {
#     uiOutput("authenticatedSidebar")
#   }
# })
# 
# # authenticated  sidebar
# output$authenticated_sidebar <- renderUI({
#   sidebarMenu(
#     menuItem("OPU Datapack Imports", tabName = "dashboard", icon = icon("dashboard"))
#   )
# })
# 
# # login sidebar
# output$uiLoginSidebar <- renderUI({
#   sidebarMenu(
#     menuItem("Login Screen", tabName = "login", icon = icon("dashboard"))
#   )
# })
