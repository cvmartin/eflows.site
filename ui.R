
# Header elements for the visualization
header <- dashboardHeader(title = "eflows", disable = FALSE)

# SIDEBAR -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "icon", href = "images/favicon/favicon-96x96.png", type = "image/x-icon")
  ),
  sidebarMenu(
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
    menuItem("Articles", 
             menuSubItem("Principles", tabName = "principles"),
             menuSubItem("Fitting curve", tabName = "fitting")), 
    menuItem("Functions", 
             menuSubItem("foreshift()", tabName = "foreshift"),
             menuSubItem("backshift()", tabName = "backshift"),
             menuSubItem("simulate()", tabName = "simulate"),
             menuSubItem("distribute()", tabName = "distribute")
             ),
    menuItem("Cases",
             menuSubItem("Electric Vehicle charing", tabName = "ev"))
    
  ) 
) 

# BODY --------------------------------------------------------------------


# home --------------------------------------------------------------------
tab_home <- 
  tabItem("home", 
          div(br(),br(), br()),
          narrowDiv(
            includeMarkdown("./rmarkdown/home/header.Rmd")
          ),
          broadDiv(div(style = "text-align:center;", 
                       img(src = "images/main-banner.png", width = "80%")
                       ), 
                   fluidRow(style = "font-family: Georgia, Times, serif; font-size: 120%;", 
                            column(width = 4, 
                                   includeMarkdown("./rmarkdown/home/intro_column_1.Rmd")),
                            column(width = 4, 
                                   includeMarkdown("./rmarkdown/home/intro_column_2.Rmd")),
                            column(width = 4, 
                                   includeMarkdown("./rmarkdown/home/intro_column_3.Rmd"))
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
            includeMarkdown("./rmarkdown/home/description.Rmd")
          )
  )

# principles (princ) ----------------------------------------------------------

tab_principles <- 
  tabItem("principles", 
          narrowDiv(
            includeMarkdown("./rmarkdown/principles/principles.Rmd")
          ), 
          wideDiv(
          ))

# fitting (fit) ---------------------------------------------------------------

tab_fitting <- 
  tabItem("fitting", 
          narrowDiv(
            includeMarkdown("./rmarkdown/fitting/fit-1.Rmd")
          ),
          wideDiv(title = "Fitting curve: combining factors", 
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
                  )),
          narrowDiv(
            includeMarkdown("./rmarkdown/fitting/fit-2.Rmd")
          ),
          wideDiv(title = "Fitting curve: applying fitting formulas",
                  column(12, includeMarkdown("./rmarkdown/fitting/fit-plus-pre.Rmd")),
                  fitSelectorInput("formula_fit"),
                
                  box(width = 12, title = "Factors that influence the fitting curve", collapsible = TRUE,
                      dyRadioSelectorUI("factors_fit_plus", c("demand", "production", "price", "grid capacity"))
                      ),
                  box(width = 12, title = "Fitting curves", collapsible = TRUE,
                      dygraphOutput("fit_fitcurve", height = dy_height)
                  ), 
                  box(width = 12,
                      dyRadioSelectorUI("graph_fit_plus", c("original", "foreshifted", "comparison")),
                      dyCornerDiv(randomizeInput("fit_random_in", label = "Random profile"))
                  )
          ),
          narrowDiv(
            includeMarkdown("./rmarkdown/fitting/fit-3.Rmd")
          )
  )

# foreshift (fsh) -------------------------------------------------------------
tab_foreshift <- 
  tabItem("foreshift", 
          narrowDiv(
            includeMarkdown("./rmarkdown/foreshift/fsh-1.Rmd")
          ),
          wideDiv(title = "Basic example",
                  inputDiv(
                    column(width = 6, 
                           sliderInput("hflex", label = "Hours of flexibility", min = 1,
                                       max = 12, value = 4, step = 1, ticks = FALSE)
                           ),
                    column(width = 6,
                           sliderInput("vol", label = "Flexible demand volume", min = 0,
                                       max = 30, value = 10, step = 0.1, ticks = FALSE)
                           )
                    ),
                  box(width = 12,
                      dyRadioSelectorUI("graph_fsh_basic", c("original", "foreshifted", "comparison"))
          )),
          narrowDiv(
            includeMarkdown("./rmarkdown/foreshift/fsh-2.Rmd")
          ),
          wideDiv(title = "Layers of flexibility",
                  box(width = 12,
                      dyRadioSelectorUI("graph_fsh_plus", c("original", "foreshifted", "comparison")),
                      dyCornerDiv(randomizeInput("fsh_random_in", label = "Random profile"))
                  )
          ),
          narrowDiv(
            includeMarkdown("./rmarkdown/foreshift/fsh-3.Rmd")
          )
  ) 

# backshift (bsh) ------------------------------------------------------------

tab_backshift <- 
  tabItem("backshift", 
          narrowDiv(
            includeMarkdown("./rmarkdown/backshift/backshift-intro.Rmd")
          ), 
          wideDiv(
          ))


# distribute (dis) ------------------------------------------------------------

tab_distribute <- 
  tabItem("distribute", 
          narrowDiv(
            includeMarkdown("./rmarkdown/distribute/distribute-intro.Rmd")
          ), 
          wideDiv(
          ))

# ev (ev) ------------------------------------------------------------------

tab_ev <- 
  tabItem("ev", 
          narrowDiv(
            includeMarkdown("./rmarkdown/ev/ev-1.Rmd")
          ),
          wideDiv(title = "One Electric Vehicle", 
                  inputDiv(div(style = "padding: 10px", tabPanelEV("0"))),
                  box(width = 12, 
                      dyRadioSelectorUI("graph_ev_one",
                                        c("original", "foreshifted","comparison"))
                      )
          ), 
          narrowDiv(
            includeMarkdown("./rmarkdown/ev/ev-2.Rmd")
          ),
          wideDiv(title = "Multiple Electric Vehicles", 
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
                      dygraphOutput("ev_multi_fitcurve", height = dy_height)
                  ), 
                  box(width = 12, 
                      dyRadioSelectorUI("graph_evs",
                                           c("original", "foreshifted","aggregated by ev", "aggregated by flex", "comparison", "unstacked"))
          )), 
          narrowDiv(
            includeMarkdown("./rmarkdown/ev/ev-3.Rmd")
          ),
          wideDiv(title = "Estimating flexibility with a large number of Electric Vehicles"),
          narrowDiv(
            includeMarkdown("./rmarkdown/ev/ev-4.Rmd")
          ),
          wideDiv(title = "Power distribution among multiple Electric Vehicles",
                  column(12, includeMarkdown("./rmarkdown/ev/ev-pwr-pre.Rmd")),
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
                      dygraphOutput("ev_pwr_soc", height = dy_height)
                     ),
                  box(width = 12, title = "Power flow into EVs",
                      dygraphOutput("ev_pwr_flow", height = dy_height)
                  )
          ),
          narrowDiv(
            includeMarkdown("./rmarkdown/ev/ev-5.Rmd")
          )
          )


# BUILD ----------------

body <- dashboardBody(
  useShinyjs(),
  tabItems(
    tab_home,
    tab_fitting,
    tab_ev,
    tab_principles,
    tab_backshift,
    tab_distribute,
    tab_foreshift
))

dashboardPage(header, sidebar, body, skin = "black")

