Sys.setenv(
  DYGRAPH_HEIGHT = 210
)

shiny::addResourcePath(
  'www',
  "./inst/app/www"
)

shinyApp(
  ui = eflows.site::app_ui,
  server = eflows.site::app_server
)