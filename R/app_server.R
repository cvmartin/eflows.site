#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny dplyr tidyr eflows eflows.viz
#' @noRd
app_server <- function(input, output, session) {
  # server tab content ------------------------------------------------------
  mod_tab_fitting_server("x")
  mod_tab_foreshift_server("x")
  mod_tab_backshift_server("x")
  mod_tab_ev_server("x")
  
  # observers ---------------------------------------------------------------
  # Add URL navigation
  observeEvent(input$sidebarmenudefault, {
    # No work to be done if input$tabs and the hash are already the same
    if (getUrlHash() == paste0("#", input$sidebarmenudefault)) return()
    # update the URL when the tab selection changes
    updateQueryString(
      paste0(getQueryString(), paste0("#", input$sidebarmenudefault)),
      "push"
    )
    # scroll to the top
    shinyjs::runjs("window.scrollTo(0, 10)")
    
    # Don't run the first time 
  }, ignoreInit = TRUE)
  
  # When the URL hash changes (typically because of pressing 
  # the forward/back button in browser)
  observeEvent(getUrlHash(), {
    hash <- getUrlHash()
    # No work to be done if input$tabs and the hash are already the same
    if (hash == paste0("#", input$sidebarmenudefault)) return()
    # chang the current tab
    updateTabsetPanel(session, "sidebarmenudefault", gsub("#", "", hash))
    # scroll to the top
    shinyjs::runjs("window.scrollTo(0, 10)")
  })  
  
  # When pressing home button
  observeEvent(input$title_button, {
    updateTabsetPanel(session, "sidebarmenudefault", "home")
  })
}
