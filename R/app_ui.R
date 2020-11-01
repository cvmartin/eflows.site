#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny shinydashboard shinyWidgets
#' @noRd
app_ui <- function(request) {
  # load UI content from modules --------------------------------------------
  tab_home <- mod_tab_home_ui("x")
  tab_foreshift <- mod_tab_foreshift_ui("x")
  tab_backshift <- mod_tab_backshift_ui("x")
  tab_distribute <- mod_tab_distribute_ui("x")
  tab_fitting <- mod_tab_fitting_ui("x")
  tab_principles <- mod_tab_principles_ui("x")
  tab_ev <- mod_tab_ev_ui("x")
  tab_author <- mod_tab_author_ui("x")
  
  # sidebar -----------------------------------------------------------------
  sidebar <- dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
      tags$link(rel = "icon", href = "www/images/favicon/favicon.ico", type = "image/x-icon"),
      # Google analytics
      tags$script(async = TRUE, 
                  src = "https://www.googletagmanager.com/gtag/js?id=UA-70471257-2"),
      tags$script("window.dataLayer = window.dataLayer || [];
                function gtag(){dataLayer.push(arguments);}
                gtag('js', new Date());
                gtag('config', 'UA-70471257-2');")
    ),
    sidebarMenu(id = "sidebarmenudefault",
                div(style = "text-align:center; font-size:300%", 
                    actionBttn(
                      inputId = "title_button",
                      label = "eflows",
                      style = "minimal",
                      size = "lg"
                    )),
                div(style = "text-align:center", 
                    br(),
                    "Data driven energy transition", 
                    br(),br()),
                tab_home$sidebar,
                menuItem("Functions",
                         tab_foreshift$sidebar,
                         tab_backshift$sidebar,
                         tab_distribute$sidebar
                ),
                menuItem("Articles", 
                         tab_fitting$sidebar,
                         tab_principles$sidebar
                ),
                menuItem("Applications",
                         tab_ev$sidebar),
                tags$hr(style = "width: 53%;"),
                tab_author$sidebar
                
    ),
    actionButton("hide_bar", label = NULL, icon = icon("caret-square-left"), 
                 class = "sidebar-toggle togbar",
                 `data-toggle` = "offcanvas")
  ) 
  
  # body ----------------
  body <- dashboardBody(
    shinyjs::useShinyjs(),
    actionButton("show_bar", label = NULL, icon = icon("caret-square-right"), 
                 class = "sidebar-toggle togbar",
                 `data-toggle` = "offcanvas"),
    tabItems(
      tab_home$content,
      tab_foreshift$content,
      tab_backshift$content,
      tab_distribute$content,
      tab_fitting$content,
      tab_principles$content,
      tab_ev$content,
      tab_author$content
    )
    )
  
  # build ----------------
  dashboardPage(
    header = dashboardHeader(title = "eflows", disable = FALSE), 
    sidebar = sidebar, 
    body = body,
    skin = "black"
    )
}
