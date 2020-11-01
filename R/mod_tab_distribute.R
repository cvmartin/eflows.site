#' mod_tab_distribute UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_tab_distribute_ui <- function(id){
  ns <- NS(id)
  
  sidebar <- menuSubItem("distribute", tabName = "distribute")
  
  content <-
    tabItem("distribute", 
            narrowDiv(
              include_md_text("distribute/distribute-intro.md")
            ), 
            mreDiv(
              "distribute", "Distribute", "mre_distribute.R"
            ),
            narrowDiv()
    )
  
  list(
    sidebar = sidebar, 
    content = content
  )
}
    
#'  mod_tab_distribute Server Function
#'
#' @noRd 
mod_tab_distribute_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session
    
    })
}
    