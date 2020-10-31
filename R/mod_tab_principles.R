#' mod_tab_principles UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_tab_principles_ui <- function(id){
  ns <- NS(id)
  
  sidebar <- menuSubItem("Design principles", tabName = ns("principles"))
  
  content <- 
    tabItem(ns("principles"), 
            narrowDiv(
              include_md_text("principles/principles.md")
            ), 
            wideDiv(
            ))
  
  
  list(
    sidebar = sidebar, 
    content = content
  )
}
    
#'  mod_tab_principles Server Function
#'
#' @noRd 
mod_tab_principles_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session
    
    })
}
    