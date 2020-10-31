Sys.setenv(
  DYGRAPH_HEIGHT = 210
)

shiny::addResourcePath(
  'www', 
  system.file('app', 'www', package='eflows.site')
)


shinyApp(
  ui = eflows.site::app_ui,
  server = eflows.site::app_server
)