#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny shinydashboard shinyWidgets dygraphs
#' @noRd
app_ui <- function(request) {
  # load UI content from modules --------------------------------------------
  tab_home <- mod_tab_home_ui("x")
  tab_fitting <- mod_tab_fitting_ui("x")
  tab_foreshift <- mod_tab_foreshift_ui("x")
  tab_backshift <- mod_tab_backshift_ui("x")
  tab_ev <- mod_tab_ev_ui("x")
  
  tab_principles <- mod_tab_principles_ui("x")
  
  # sidebar -----------------------------------------------------------------
  sidebar <- dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
      tags$link(rel = "icon", href = "images/favicon/favicon-96x96.png", type = "image/x-icon"),
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
                         menuSubItem("distribute", tabName = "distribute")
                ),
                menuItem("Articles", 
                         tab_fitting$sidebar,
                         tab_principles$sidebar
                ),
                menuItem("Applications",
                         tab_ev$sidebar),
                tags$hr(style = "width: 53%;"),
                menuItem("Author", tabName = "author")
    ),
    actionButton("hide_bar", label = NULL, icon = icon("caret-square-left"), 
                 class = "sidebar-toggle togbar",
                 `data-toggle` = "offcanvas")
  ) 
  
  
  # distribute (dis) ------------------------------------------------------------
  tab_distribute <- 
    tabItem("distribute", 
            narrowDiv(
              include_md_text("distribute/distribute-intro.md")
            ), 
            mreDiv(
              "distribute", "Distribute", "mre_distribute.R"
            ),
            narrowDiv()
    )
  
  
  # author ------------------------------------------------------------------
  tab_author <- 
    tabItem("author", 
            narrowDiv(
              include_md_text("author/author.md")
            )
    )
  
  # body ----------------
  body <- dashboardBody(
    shinyjs::useShinyjs(),
    actionButton("show_bar", label = NULL, icon = icon("caret-square-right"), 
                 class = "sidebar-toggle togbar",
                 `data-toggle` = "offcanvas"),
    tabItems(
      tab_home$content,
      tab_fitting$content,
      tab_ev$content,
      tab_principles$content,
      tab_backshift$content,
      tab_distribute,
      tab_foreshift$content,
      tab_author
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
