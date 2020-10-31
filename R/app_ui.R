#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny shinydashboard shinyWidgets dygraphs
#' @noRd
app_ui <- function(request) {
  # Header elements for the visualization
  header <- dashboardHeader(title = "eflows", disable = FALSE)
  
  # SIDEBAR -----------------------------------------------------------------
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
                menuItem("Home", tabName = "home"),
                menuItem("Functions",
                         menuSubItem("foreshift", tabName = "foreshift"),
                         menuSubItem("backshift (BETA)", tabName = "backshift"),
                         # menuSubItem("simulate", tabName = "simulate"),
                         menuSubItem("distribute", tabName = "distribute")
                ),
                menuItem("Articles", 
                         menuSubItem("Fitting formula and curve", tabName = "fitting"),
                         menuSubItem("Design principles", tabName = "principles")
                ),
                menuItem("Applications",
                         menuSubItem("Electric Vehicles charging", tabName = "ev")),
                tags$hr(style = "width: 53%;"),
                menuItem("Author", tabName = "author")
    ),
    actionButton("hide_bar", label = NULL, icon = icon("caret-square-left"), 
                 class = "sidebar-toggle togbar",
                 `data-toggle` = "offcanvas")
  ) 
  
  # BODY --------------------------------------------------------------------
  # home --------------------------------------------------------------------
  tab_home <- 
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
  
  # principles (princ) ----------------------------------------------------------
  tab_principles <- 
    tabItem("principles", 
            narrowDiv(
              include_md_text("principles/principles.md")
            ), 
            wideDiv(
            ))
  
  # fitting (fit) ---------------------------------------------------------------
  tab_fitting <- 
    tabItem("fitting", 
            narrowDiv(
              include_md_text("fitting/fit-1.md")
            ),
            wideDiv(title = "Fitting curve: combining factors", 
                    column(12, include_md_text("fitting/fit-basic-pre.md")),
                    box(width = 12, title = "Factors that influence the fitting curve", 
                        collapsible = TRUE, collapsed = TRUE,
                        dyRadioSelectorUI("factors_fit_basic", c("demand", "production", "price", "grid capacity"))
                    ),
                    inputDiv(
                      column(3,
                             sliderInput("fit_basic_demand", 
                                         "Demand: relative importance",
                                         min = 0, max = 1, value = 0.5, step = 0.1, ticks = FALSE)
                      ), 
                      column(3,
                             sliderInput("fit_basic_solar", 
                                         "Production: relative importance",
                                         min = 0, max = 1, value = 0.5, step = 0.1, ticks = FALSE)
                      ), 
                      column(3,
                             sliderInput("fit_basic_price", 
                                         "Price: relative importance",
                                         min = 0, max = 1, value = 0.5, step = 0.1, ticks = FALSE)
                      ), 
                      column(3, 
                             div(style = "text-align:center; padding-top:22px;",
                                 switchInput("switch_fit_basic_cap", 
                                             label = "Use grid capacity",
                                             labelWidth = 140, 
                                             inline = TRUE)
                             )
                      )
                    ),
                    box(width = 12,
                        dyRadioSelectorUI("graph_fit_basic", c("original", "foreshifted", "comparison"))
                    ),
                    column(12, include_md_text("fitting/fit-basic-post.md"))
            ),
            narrowDiv(
              include_md_text("fitting/fit-2.md")
            ),
            wideDiv(title = "Fitting curve: applying fitting formulas",
                    column(12, include_md_text("fitting/fit-plus-pre.md")),
                    fitSelectorInput("formula_fit"),
                    
                    box(width = 12, title = "Factors that influence the fitting curve", collapsible = TRUE,
                        dyRadioSelectorUI("factors_fit_plus", c("demand", "production", "price", "grid capacity"))
                    ),
                    box(width = 12, title = "Fitting curves", collapsible = TRUE,
                        dygraphs::dygraphOutput("fit_fitcurve", height = Sys.getenv("DYGRAPH_HEIGHT"))
                    ), 
                    box(width = 12,
                        dyRadioSelectorUI("graph_fit_plus", c("original", "foreshifted", "comparison")),
                        dyCornerDiv(randomizeInput("fit_random_in", label = "Random profile"))
                    )
            ),
            mreDiv(
              "fit", "Fitting formula and curve", "mre_fit.R"
            ),
            narrowDiv(
              include_md_text("fitting/fit-3.md")
            )
    )
  
  # foreshift (fsh) -------------------------------------------------------------
  tab_foreshift <- 
    tabItem("foreshift", 
            narrowDiv(
              include_md_text("fsh/fsh-1.md")
            ),
            wideDiv(title = "Flexibility layer",
                    column(12, include_md_text("fsh/fsh-basic-pre.md")),
                    inputDiv(
                      column(width = 6,
                             sliderInput("vol", label = "Volume of flexible demand", min = 0,
                                         max = 30, value = 10, step = 1, ticks = FALSE, post = " kWh")
                      ),
                      column(width = 6, 
                             sliderInput("hflex", label = "Flexibility", min = 1,
                                         max = 12, value = 4, step = 1, ticks = FALSE, post = " hours")
                      )
                    ),
                    box(width = 12,
                        dyRadioSelectorUI("graph_fsh_basic", c("original", "foreshifted", "comparison"))), 
                    column(12, include_md_text("fsh/fsh-basic-post.md"))
            ),
            mreDiv(
              "fsh_oneflex", "Foreshift: one flexibility", "mre_fsh_oneflex.R"
            ),
            narrowDiv(
              include_md_text("fsh/fsh-2.md")
            ),
            mreDiv(
              "fsh_multiflex", "Foreshift: one object, multiple flexibility", "mre_fsh_multiflex.R"
            ),
            wideDiv(title = "Layers of flexibility",
                    box(width = 12,
                        column(12, include_md_text("fsh/fsh-plus-pre.md")),
                        dyRadioSelectorUI("graph_fsh_plus", c("original", "foreshifted", "comparison", "unstacked by flex")),
                        dyCornerDiv(randomizeInput("fsh_random_in", label = "Random profile")),
                        column(12, include_md_text("fsh/fsh-plus-post.md"))
                    )
            ),
            mreDiv(
              "fsh_multiobject", "Foreshift: multiple objects, multiple flexibility", "mre_fsh_multiobject.R"
            ),
            narrowDiv(
              include_md_text("fsh/fsh-3.md")
            )
    ) 
  
  # backshift (bsh) ------------------------------------------------------------
  tab_backshift <- 
    tabItem("backshift", 
            narrowDiv(
              include_md_text("backshift/backshift-intro.md")
            ), 
            wideDiv( title = "The cost of backshifing", #####
                     inputDiv(
                       column(3,
                              sliderInput("bsh_cost_self_discharge",
                                          "Self discharge",
                                          min = 0, max = 10, value = 1, step = 0.1,
                                          ticks = FALSE, post = " % per hour")
                       ),
                       column(3,
                              sliderInput("bsh_cost_eff_to",
                                          "Efficiency to battery:",
                                          min = 85, max = 100, value = 100,
                                          ticks = FALSE, post = " %")
                       ),
                       column(3,
                              sliderInput("bsh_cost_eff_from",
                                          "Efficiency from battery:",
                                          min = 85, max = 100, value = 100,
                                          ticks = FALSE, post = " %")
                       ),
                       column(3,
                              sliderInput("bsh_cost_back_time",
                                          "Time horizon",
                                          min = 1, max = 24, value = 12,
                                          ticks = FALSE, post = " hours")
                       ),
                       column(12,
                              div(
                                # style = "display:inline;width = 500px",
                                style = "padding-left: 60px",
                                tags$strong("Point in time"),
                                sliderInput("bsh_cost_back_point", label = NULL,
                                            min = 1, max = 168, value = 94,
                                            ticks = FALSE, post = " hours from the start")
                              )
                              
                       )
                     ),
                     box(width = 12,
                         dygraphs::dygraphOutput("graph_bsh_cost", height = Sys.getenv("DYGRAPH_HEIGHT")),
                         dyCornerDiv(randomizeInput("bsh_cost_random_in", label = "Random profile"))
                     )
            ),
            wideDiv(title = "Fitting to backshift", 
                    inputDiv(
                      column(3,
                             sliderInput("bsh_fit_self_discharge",
                                         "Self discharge",
                                         min = 0, max = 10, value = 1, step = 0.1,
                                         ticks = FALSE, post = " % per hour")
                      ), 
                      column(3,
                             sliderInput("bsh_fit_eff_to",
                                         "Efficiency to battery:",
                                         min = 85, max = 100, value = 100,
                                         ticks = FALSE, post = " %")
                      ), 
                      column(3,
                             sliderInput("bsh_fit_eff_from",
                                         "Efficiency from battery:",
                                         min = 85, max = 100, value = 100,
                                         ticks = FALSE, post = " %")
                      ), 
                      column(3, 
                             sliderInput("bsh_fit_back_time",
                                         "Time horizon",
                                         min = 1, max = 24, value = 12,
                                         ticks = FALSE, post = " hours")
                      ),
                      ##
                      
                      column(2, 
                             sliderInput("bsh_fit_pwr_to",
                                         "Max. charge rate",
                                         min = 0, max = 5, value = 3,
                                         ticks = FALSE, post = " kW")
                      ),
                      column(2, 
                             sliderInput("bsh_fit_pwr_from",
                                         "Max. discharge rate",
                                         min = 0, max = 5, value = 3,
                                         ticks = FALSE, post = " kW")
                      ),
                      column(3, 
                             sliderInput("bsh_fit_soc_init",
                                         "Initial State Of Charge",
                                         min = 0, max = 100, value = 5,
                                         ticks = FALSE, post = " %")
                      ),
                      column(3,
                             sliderInput("bsh_fit_vol",
                                         "Storage capacity",
                                         min = 0, max = 100, value = 5,
                                         ticks = FALSE, post = " kWh")
                      ),
                      column(2,
                             div(style = "text-align:center; padding-top:22px;",
                                 switchInput("switch_bsh_fit_vol_unlimit",
                                             label = "Unlimited",
                                             labelWidth = 100,
                                             inline = TRUE)
                             )
                      )
                    ),
                    fitSelectorInput("formula_bsh_fit"),
                    
                    box(width = 12, title = "Factors that influence the fitting curve", 
                        collapsible = TRUE, collapsed = TRUE,
                        dyRadioSelectorUI("factors_bsh_fit", c("demand", "production", "price", "grid capacity"))
                    ),
                    box(width = 12, title = "Fitting curves", collapsible = TRUE, collapsed = TRUE,
                        dygraphs::dygraphOutput("bsh_fit_fitcurve", height = Sys.getenv("DYGRAPH_HEIGHT"))
                    ), 
                    box(width = 12, 
                        dyRadioSelectorUI("graph_bsh_fit", c("potential", "backshifted", "comparison")),
                        dyRadioSelectorUI("graph_bsh_str", c("storage: exchange", "storage: soc"), height = 120)
                    )
            )
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
  
  # ev (ev) ------------------------------------------------------------------
  tab_ev <- 
    tabItem("ev", 
            narrowDiv(
              include_md_text("ev/ev-1.md")
            ),
            wideDiv(title = "One Electric Vehicle", 
                    column(12, include_md_text("ev/ev-basic-pre.md")),
                    inputDiv(div(style = "padding: 10px", tabPanelEV("0"))),
                    box(width = 12, 
                        dyRadioSelectorUI("graph_ev_one",
                                          c("original", "foreshifted","comparison"))
                    ),
                    column(12, include_md_text("ev/ev-basic-post.md"))
                    
            ), 
            narrowDiv(
              include_md_text("ev/ev-2.md")
            ),
            wideDiv(title = "Multiple Electric Vehicles", 
                    column(12, include_md_text("ev/ev-plus-pre.md")),
                    tabBox(title = "Electric Vehicles", width = 12,
                           id = "tab_evs",
                           tabPanelEV("1"), 
                           tabPanelEV("2"), 
                           tabPanelEV("3"),
                           tabPanelEV("4") 
                    ),
                    
                    fitSelectorInput("formula_ev"),
                    
                    box(width = 12, title = "Factors that influence the fitting curve", 
                        collapsible = TRUE, collapsed = TRUE,
                        dyRadioSelectorUI("factors_ev", c("demand", "production", "price", "grid capacity"))
                    ),
                    box(width = 12, title = "Fitting curves", collapsible = TRUE, collapsed = TRUE,
                        dygraphs::dygraphOutput("ev_multi_fitcurve", height = Sys.getenv("DYGRAPH_HEIGHT"))
                    ), 
                    box(width = 12, 
                        dyRadioSelectorUI("graph_evs",
                                          c("original", "foreshifted","aggregated by ev", 
                                            "aggregated by flex", "comparison", "unstacked", "stacked"))
                    ),
                    column(12, include_md_text("ev/ev-plus-post.md"))
            ), 
            narrowDiv(
              include_md_text("ev/ev-3.md")
            ),
            mreDiv(
              "ev_timeframe", "EV charging: Timeframe", "mre_ev_timeframe.R"
            ),
            wideDiv(title = "Electric Vehicles flexibility: large numbers",
                    column(12, include_md_text("ev/ev-dist-pre.md")),
                    inputDiv(
                      column(4, sliderInput("ev_dist_evs", "Number of EVs",
                                            min = 0, max = 200,
                                            value = 100, step = 5, ticks = FALSE)
                      ), 
                      column(4, sliderInput("ev_dist_households", "Number of households",
                                            min = 0, max = 200,
                                            value = 100, step = 5, ticks = FALSE)
                      ),
                      column(4, selectInput("ev_dist_match", 
                                            label = "Distict type",
                                            choices = list(Downtown = "Centrum", 
                                                           Residential = "Zuid", 
                                                           `Late night charging` = "Nieuw-West"))
                      )
                    ),
                    box(width = 12,
                        dyRadioSelectorUI("graph_ev_dist",
                                          c("original", "foreshifted","comparison"))
                    ),
                    column(12, include_md_text("ev/ev-dist-post.md"))
            ),
            narrowDiv(
              include_md_text("ev/ev-4.md")
            ),
            wideDiv(title = "Power distribution among multiple Electric Vehicles",
                    column(12, include_md_text("ev/ev-pwr-pre.md")),
                    tabBox(title = "Electric Vehicles", width = 12,
                           id = "tab_pwr_evs",
                           tabPanelPwrEV("1", 40, 75, 20, 2), 
                           tabPanelPwrEV("2", 40, 50, 12, 4), 
                           tabPanelPwrEV("3", 15, 30, 10, 1),
                           tabPanelPwrEV("4", 5, 50, 12, 3), 
                           tabPanelPwrEV("5", 30, 40, 20, 3)
                    ),
                    inputDiv(
                      column(4, sliderInput("cap_evs_pwr", "Grid capacity", min = 10, max = 100,
                                            value = 40, step = 5, ticks = FALSE, post = " kW")
                      ), 
                      column(4, 
                             div(style = "text-align:center; padding-top:22px;",
                                 randomizeInput("cap_random_in", label = "Random capacity"))
                      ),
                      column(4, sliderInput("eff_evs_pwr", "Battery charging efficiency", min = 0.75, max = 1, 
                                            value = 0.9,ticks = FALSE)
                      )
                    ),
                    box(width = 12, title = "EVs State Of Charge", 
                        dygraphs::dygraphOutput("ev_pwr_soc", height = Sys.getenv("DYGRAPH_HEIGHT"))
                    ),
                    box(width = 12, title = "Power flow into EVs",
                        dygraphs::dygraphOutput("ev_pwr_flow", height = Sys.getenv("DYGRAPH_HEIGHT"))
                    ),
                    column(12, include_md_text("ev/ev-pwr-post.md"))
            ),
            mreDiv(
              "ev_gridcapacity", "EV charging: Grid Capacity", "mre_ev_gridcapacity.R"
            ),
            narrowDiv(
              include_md_text("ev/ev-5.md")
            )
    )
  
  # author ------------------------------------------------------------------
  tab_author <- 
    tabItem("author", 
            narrowDiv(
              include_md_text("author/author.md")
            )
    )
  
  # BUILD ----------------
  body <- dashboardBody(
    shinyjs::useShinyjs(),
    actionButton("show_bar", label = NULL, icon = icon("caret-square-right"), 
                 class = "sidebar-toggle togbar",
                 `data-toggle` = "offcanvas"),
    tabItems(
      tab_home,
      tab_fitting,
      tab_ev,
      tab_principles,
      tab_backshift,
      tab_distribute,
      tab_foreshift,
      tab_author
    ))
  
  dashboardPage(header, sidebar, body, skin = "black")
}
