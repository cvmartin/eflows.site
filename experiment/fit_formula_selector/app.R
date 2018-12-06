
# the prep ----------------------------------------------------------------

fitSelectorInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    singleton(tags$style('
.fitSelectorDiv {  padding:0;
                         padding-top:5px;
                         background-color: #f5f5f5;
                         border: 1px solid #e3e3e3;
                         border-radius: 2px;
}
')),
    box(width = 12, class = "fitSelectorDiv", 
        column(width = 9, style = "margin-bottom: -5px;",
               searchInput(ns("formula"), tagList("Fit formula:", tags$code("fit = ~")),
                           value = "1*.demand", btnSearch = icon("level-down"), width = "100%")
        ), 
        column(width = 3, style = "margin-bottom: -5px;",
               selectInput(ns("predefined_formulas"), "Predefined formulas", choices = list_formulas))
        )
  )
}



fitSelector <- function(input, output, session) {
  
  observeEvent(input$predefined_formulas, {
    updateSearchInput(session = session, "formula", 
                      value = input$predefined_formulas,
                      trigger = TRUE)
  })
  
  # Does it have to be reactive?
  current_formula <- reactive({ input$formula })
  
  return(current_formula)
}


# the app -----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(header = dashboardHeader(), 
                    sidebar = dashboardSidebar(), 
                    body = dashboardBody(
                      fluidPage(
                        fluidRow(
                          fitSelectorInput("fit1")
                        )
                        )
                    )
                    )


server <- function(input, output, session) {
  result <- callModule(fitSelector, "fit1")
}

shinyApp(ui, server)