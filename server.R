library(shiny)
library(futile.logger)
library(shinyWidgets)
library(datimutils)
library(shinyjs)
library(datapackr)

options("scipen" = 999)

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
  validation_results <- reactiveValues(datapack = NULL)
  ready <- reactiveValues(ok = FALSE)
  # import files
  main_import_json <- reactiveVal
  deletes_json <- reactiveVal()
  dedupes_00000_json <- reactiveVal()
  dedupes_00001_json <- reactiveVal()
  
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
        shinyjs::useShinyjs(),
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
        actionButton("import", "Import"),
        # FOR TESTING - eliminate when prod
        actionButton("test", "TEST SOMETHING")
      ),
      fluidRow(
        h1(
          paste0(
            "You are currently logged into the ",
            input$server,
            " DATIM server."
          )
        ),
        uiOutput("info")
      ),
      fluidRow(
        uiOutput("messages")
      ),
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
  
  ## datapack info ----
  output$info <- renderUI({
    
    if(!is.null(validation_results$datapack)) {
      
      paste0(
        "Tool loaded: ", validation_results$datapack$info$tool,
        " Name: ", validation_results$datapack$info$datapack_name,
        " COP Year: ", validation_results$datapack$info$cop_year
      )
      
    } 
    
  })
  
  # button management ----
  observeEvent(input$file1, {
    shinyjs::show("validate")
    shinyjs::enable("validate")
    ready$ok <- FALSE
  })
  
  # validation ----
  observeEvent(input$validate, {
    
    # disable buttons
    shinyjs::disable("file1")
    shinyjs::disable("validate")
    ready$ok <- TRUE
    
    inFile <- input$file1
    messages <- ""
    
    print("unpacking...")
    print(input$file1)
    
    withProgress(message = "Validating file", value = 0, {
      
      incProgress(0.1, detail = ("Unpacking your DataPack"))
      
      
      d <- tryCatch({
        datapackr::unPackTool(inFile$datapath,
                              d2_session = user_input$d2_session)},
        error = function(e) {
          return(e)
        })
      
      if (inherits(d, "error")) {
        return("An error occurred. Please contact DATIM support.")
      }
    })
    
    #Create some additional metadadta for S3 tagging
    d$info$sane_name <-
      paste0(stringr::str_extract_all(d$info$datapack_name,
                                      "[A-Za-z0-9_]",
                                      simplify = TRUE),
             sep = "", collapse = "")
    if (is.null(d$info$sane_name)) {
      stop("sane_name cannot be NULL!")
    }
    d$info$source_user <- user_input$d2_session$username
    if (is.null(d$info$source_user)) {
      stop("source_user cannot be NULL!")
    }
    
    #All self-service datapacks should be marked as unapproved for PAW
    d$info$approval_status <- "APPROVED"
    #Generate a unique identifier
    d$info$uuid <- uuid::UUIDgenerate()
    
    if (d$info$tool ==  "Data Pack") {
      
      print("Datapack")
      
      # differences between datapack and datim at start of import
      # baseline_diff <- compareData_DatapackVsDatim(d)
      # 
      
      # extract data for import
      data <- d$datim$MER %>% 
        dplyr::bind_rows(d$datim$subnat_impatt)
      
      #Remap mech codes to UIDs
      
      data$attributeOptionCombo <-
        datimvalidation::remapCategoryOptionCombos(data$attributeOptionCombo,
                                                   "code", "id", d2session = user_input$d2_session)
      
      # double check all category option combos are uids, no "default" or mech codes
      assertthat::assert_that(
        all(datapackr::is_uidish(data$attributeOptionCombo)))
      assertthat::assert_that(
        all(datapackr::is_uidish(data$categoryOptionCombo)))
      
      # drop dedupe from main import
      main_import <-
        dplyr::filter(data,
                      !(attributeOptionCombo %in% c("X8hrDf6bLDC",
                                                    "YGT1o7UxfFu"))) %>%
        dplyr::mutate(value = as.character(value))
      
      dedupes_00000 <- dplyr::filter(data,
                                                   attributeOptionCombo == "X8hrDf6bLDC")
      
      dedupes_00001 <- dplyr::filter(data,
                                                   attributeOptionCombo == "YGT1o7UxfFu")
      
      # delete prior cop subnat data
      # delete any pre existing COP22 data, generally only has impact
      # if we are reloading the DP
      #TODO: d2_session needs to be changed to be consistent with d2sessions, we need to choose one
      deletes <- datapackr::getCOPDataFromDATIM(d$info$country_uids,
                                                        cop_year = 2021,
                                                        datastreams = c("subnat_targets"),
                                                        d2_session = user_input$d2_session
      ) %>% 
        dplyr::bind_rows(datapackr::getCOPDataFromDATIM(d$info$country_uids,
                                                        cop_year = 2022,
                                                        d2_session = user_input$d2_session)
        ) %>%
        dplyr::mutate(attributeOptionCombo =
                        datimvalidation::remapCategoryOptionCombos(
                          attributeOptionCombo,
                          "code",
                          "id",
                          d2session = user_input$d2_session),
                      categoryOptionCombo =
                        datimvalidation::remapCategoryOptionCombos(
                          categoryOptionCombo,
                          "code",
                          "id",
                          d2session = user_input$d2_session)) %>%
        dplyr::select(dataElement,
                      period,
                      orgUnit,
                      categoryOptionCombo,
                      attributeOptionCombo,
                      value)
    }
    
    
    # generate json versions of the import files
    print("generating json import files stored as reactive val...")
    main_import_json(prep_json(main_import))
    dedupes_00000_json(prep_json(dedupes_00000))
    dedupes_00001_json(prep_json(dedupes_00001))
    deletes_json(prep_json(deletes))
    
    
    # pass entire d object to validation results
    validation_results$datapack <- d
    
  })
  
  # messages ----
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
  
  ## TESTING
  observeEvent(input$test, {
    # test reactive value is captured
    print(deletes_json())
  })
}

