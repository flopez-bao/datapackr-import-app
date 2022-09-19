# import libraries -----
library(shiny)
library(futile.logger)
library(shinyWidgets)
library(shinydashboard)

# ui -----
# ui <- shinyUI(
#   uiOutput("ui")
# )

ui <- dashboardPage(
  dashboardHeader(title = "Datapack Import App",
                  tags$li(
                    a(
                      img(
                        src = 'datim.png',
                        title = "Company Home",
                        height = "50px"
                      ),
                      style = "padding-top:10px; padding-bottom:10px;"
                    ),
                    class = "dropdown"
                  )),
  ## Sidebar content
  dashboardSidebar(
    collapsed = T#,
    #uiOutput("ui_sidebar")
    ),
  dashboardBody(
    # First tab content
    uiOutput("ui")
    )
)