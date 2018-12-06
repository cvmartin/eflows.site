

# The module --------------------------------------------------------------
moduleSearchInput <- function(id, title) {
  ns <- NS(id)
  
  div(style = "padding:10px; margin:10px; border-style:solid; border-width:2px;",
    tags$h2(title),
    searchInput(
      inputId = ns("search"), label = "Enter your text",
      placeholder = "A placeholder",
      btnSearch = icon("search"),
      btnReset = icon("remove"),
      width = "450px"
    ),
    br(),
    verbatimTextOutput(outputId = ns("res")),
    br(),
    textInput(
      inputId = ns("update_search"),
      label = "Update search"
    ),
    checkboxInput(
      inputId = ns("trigger_search"),
      label = "Trigger update search",
      value = TRUE
    )
    )
  }

moduleSearch <- function(input, output, session) {
  output$res <- renderPrint({
    input$search
  })
  
  observeEvent(input$update_search, {
    updateSearchInput(
      session = session,
      inputId = "search",
      value = input$update_search,
      trigger = input$trigger_search
    )
  }, ignoreInit = TRUE)
}



# The app -----------------------------------------------------------------
  library(shiny)
  library(shinyWidgets)
  
  ui <- fluidPage(
    moduleSearchInput("module1", "First module"),
    moduleSearchInput("module2", "Second module")
  )
  
  server <- function(input, output, session) {
    callModule(moduleSearch, "module1")
    callModule(moduleSearch, "module2")
  }
  
  shinyApp(ui, server)
  
