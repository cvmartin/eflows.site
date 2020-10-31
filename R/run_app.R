#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
run_app <- function() {
  
  const <- list(dy_height = 210)
  
  shinyApp(
    ui = app_ui,
    server = app_server
  )
}