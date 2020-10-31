#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
run_app <- function() {
  
  Sys.setenv(
    DYGRAPH_HEIGHT = 210
  )
  
  shiny::addResourcePath(
    'www', 
    system.file('app', 'www', package='eflows.site')
  )
  
  
  shinyApp(
    ui = app_ui,
    server = app_server
  )
}