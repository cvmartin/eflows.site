#' mod_tab_home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_tab_home_ui <- function(id){
  ns <- NS(id)
  
  sidebar <- menuItem("Home", tabName = "home")
  
  content <- 
    tabItem("home", 
            narrowDiv(
              include_md_text("home/header.md")
            ),
            broadDiv(fluidRow(style = "font-family: Georgia, Times, serif; font-size: 120%; margin-top:-60px; margin-bottom:50px;",
                              column(width = 4, style = "text-align: center;",
                                     include_md_text("home/intro_column_1.md")),
                              column(width = 4, style = "text-align: center;",
                                     include_md_text("home/intro_column_2.md")),
                              column(width = 4, style = "text-align: center;",
                                     include_md_text("home/intro_column_3.md"))
            ),
            fluidRow(
              column(width = 6, style = "text-align: right;",
                     tags$a(href = "https://github.com/cvmartin/eflows", target = "_blank",
                            actionBttn(
                              inputId = "git_eflows",
                              label = "eflows",
                              color = "primary",
                              style = "bordered",
                              icon = icon("github")
                            ))),
              column(width = 6, style = "text-align: left;",
                     tags$a(href = "https://github.com/cvmartin/eflows.viz", target = "_blank",
                            actionBttn(
                              inputId = "git_eflows_viz",
                              label = "eflows.viz",
                              color = "primary",
                              style = "bordered",
                              icon = icon("github")
                            )))
            )
            ),
            narrowDiv(
              include_md_text("home/home-1.md")
            ),
            mreDiv(
              "getting_started", "Getting started", "mre_getting_started.R", height = "180px"
            ),
            narrowDiv(
              include_md_text("home/home-2.md")
            )
    )
  
  list(
    sidebar = sidebar, 
    content = content
    )
}
    
#'  mod_tab_home Server Function
#'
#' @noRd 
mod_tab_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session
    
    })
}
    