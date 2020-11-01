#' mod_tab_author UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_tab_author_ui <- function(id){
  ns <- NS(id)
  
  sidebar <- menuItem("Author", tabName = "author")
  
  content <-
    tabItem("author", 
            narrowDiv(
              include_md_text("author/author.md")
            )
    )
  
  list(
    sidebar = sidebar, 
    content = content
  )
}
    
#'  mod_tab_author Server Function
#'
#' @noRd 
mod_tab_author_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session
    
    })
}
    