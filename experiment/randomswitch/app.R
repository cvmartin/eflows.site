library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)

ui <- dashboardPage(dashboardHeader(), dashboardSidebar(
  tags$head(
    tags$style('#dorand{
transition: background-color 0.5s ease;
color:white; 
background-color: #337ab7; 
border-color: #cccccc} 
#dorand:disabled {
transition: background-color 0.5s ease;
    background-color: white ;
border-color: darkgray;
color: darkgray;
}')
  )
), dashboardBody(
  
  useShinyjs(),
  div(
    div(
      switchInput("randswitch", 
                  label = "Randomize profile",
                  labelWidth = 140, 
                  inline = TRUE
      ), 
      div(style = "display:inline;", 
          actionButton("dorand", NULL , icon = icon("random")))
    )
  )
  
), skin = "black")
  
server <- function(input, output, session) {
  
  observeEvent(input$randswitch, {
    toggleState("dorand")
  })
  
}

shinyApp(ui, server)