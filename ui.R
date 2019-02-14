
# Header elements for the visualization
header <- dashboardHeader(title = "eflows", disable = FALSE)

# SIDEBAR -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
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
  ) 
) 

# BODY --------------------------------------------------------------------


# home --------------------------------------------------------------------
tab_home <- 
  tabItem("home", 
          narrowDiv(
            includeMarkdown("./rmarkdown/home/header.Rmd")
          ),
          broadDiv(fluidRow(style = "font-family: Georgia, Times, serif; font-size: 120%; margin-top:-60px; margin-bottom:50px;",
                            column(width = 4, style = "text-align: center;",
                                   includeMarkdown("./rmarkdown/home/intro_column_1.Rmd")),
                            column(width = 4, style = "text-align: center;",
                                   includeMarkdown("./rmarkdown/home/intro_column_2.Rmd")),
                            column(width = 4, style = "text-align: center;",
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
            includeMarkdown("./rmarkdown/home/home-1.Rmd")
          ),
          mreDiv(
            "getting_started", "Getting started", "mre/mre_getting_started.R", height = "180px"
          ),
          narrowDiv(
            includeMarkdown("./rmarkdown/home/home-2.Rmd")
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
                  column(12, includeMarkdown("./rmarkdown/fitting/fit-basic-pre.Rmd")),
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
                  column(12, includeMarkdown("./rmarkdown/fitting/fit-basic-post.Rmd"))
                  ),
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
          mreDiv(
            "fit", "Fitting formula and curve", "mre/mre_fit.R"
          ),
          narrowDiv(
            includeMarkdown("./rmarkdown/fitting/fit-3.Rmd")
          )
  )

# foreshift (fsh) -------------------------------------------------------------
tab_foreshift <- 
  tabItem("foreshift", 
          narrowDiv(
            includeMarkdown("./rmarkdown/fsh/fsh-1.Rmd")
          ),
          wideDiv(title = "Flexibility layer",
                  column(12, includeMarkdown("./rmarkdown/fsh/fsh-basic-pre.Rmd")),
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
                  column(12, includeMarkdown("./rmarkdown/fsh/fsh-basic-post.Rmd"))
                  ),
          mreDiv(
            "fsh_oneflex", "Foreshift: one flexibility", "mre/mre_fsh_oneflex.R"
          ),
          narrowDiv(
            includeMarkdown("./rmarkdown/fsh/fsh-2.Rmd")
          ),
          mreDiv(
            "fsh_multiflex", "Foreshift: one object, multiple flexibility", "mre/mre_fsh_multiflex.R"
          ),
          wideDiv(title = "Layers of flexibility",
                  box(width = 12,
                      column(12, includeMarkdown("./rmarkdown/fsh/fsh-plus-pre.Rmd")),
                      dyRadioSelectorUI("graph_fsh_plus", c("original", "foreshifted", "comparison", "unstacked by flex")),
                      dyCornerDiv(randomizeInput("fsh_random_in", label = "Random profile")),
                      column(12, includeMarkdown("./rmarkdown/fsh/fsh-plus-post.Rmd"))
                  )
          ),
          mreDiv(
            "fsh_multiobject", "Foreshift: multiple objects, multiple flexibility", "mre/mre_fsh_multiobject.R"
          ),
          narrowDiv(
            includeMarkdown("./rmarkdown/fsh/fsh-3.Rmd")
          )
  ) 

# backshift (bsh) ------------------------------------------------------------

tab_backshift <- 
  tabItem("backshift", 
          narrowDiv(
            includeMarkdown("./rmarkdown/backshift/backshift-intro.Rmd")
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
              dygraphOutput("graph_bsh_cost", height = dy_height),
              dyCornerDiv(randomizeInput("bsh_cost_random_in", label = "Random profile"))
              )
          # ,
          # box(width = 12,
          #     dyRadioSelectorUI("graph_bsh_basic", c("potential", "backshifted", "comparison")))
          ),
          ###
          ######
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
                      dygraphOutput("bsh_fit_fitcurve", height = dy_height)
                  ), 
                  box(width = 12, 
                      dyRadioSelectorUI("graph_bsh_fit", c("potential", "backshifted", "comparison"))
                  )
          )
          )


# distribute (dis) ------------------------------------------------------------

tab_distribute <- 
  tabItem("distribute", 
          narrowDiv(
            includeMarkdown("./rmarkdown/distribute/distribute-intro.Rmd")
          ), 
          mreDiv(
            "distribute", "Distribute", "mre/mre_distribute.R"
          ),
          narrowDiv()
          )

# ev (ev) ------------------------------------------------------------------

tab_ev <- 
  tabItem("ev", 
          narrowDiv(
            includeMarkdown("./rmarkdown/ev/ev-1.Rmd")
          ),
          wideDiv(title = "One Electric Vehicle", 
                  column(12, includeMarkdown("./rmarkdown/ev/ev-basic-pre.Rmd")),
                  inputDiv(div(style = "padding: 10px", tabPanelEV("0"))),
                  box(width = 12, 
                      dyRadioSelectorUI("graph_ev_one",
                                        c("original", "foreshifted","comparison"))
                      ),
                  column(12, includeMarkdown("./rmarkdown/ev/ev-basic-post.Rmd"))
                  
          ), 
          narrowDiv(
            includeMarkdown("./rmarkdown/ev/ev-2.Rmd")
          ),
          wideDiv(title = "Multiple Electric Vehicles", 
                  column(12, includeMarkdown("./rmarkdown/ev/ev-plus-pre.Rmd")),
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
                                           c("original", "foreshifted","aggregated by ev", 
                                             "aggregated by flex", "comparison", "unstacked"))
                      ),
                  column(12, includeMarkdown("./rmarkdown/ev/ev-plus-post.Rmd"))
          ), 
          narrowDiv(
            includeMarkdown("./rmarkdown/ev/ev-3.Rmd")
          ),
          mreDiv(
            "ev_timeframe", "EV charging: Timeframe", "mre/mre_ev_timeframe.R"
          ),
          wideDiv(title = "Electric Vehicles flexibility: large numbers",
                  column(12, includeMarkdown("./rmarkdown/ev/ev-dist-pre.Rmd")),
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
                  column(12, includeMarkdown("./rmarkdown/ev/ev-dist-post.Rmd"))
                  ),
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
                  ),
                  column(12, includeMarkdown("./rmarkdown/ev/ev-pwr-post.Rmd"))
          ),
          mreDiv(
            "ev_gridcapacity", "EV charging: Grid Capacity", "mre/mre_ev_gridcapacity.R"
          ),
          narrowDiv(
            includeMarkdown("./rmarkdown/ev/ev-5.Rmd")
          )
          )


# author ------------------------------------------------------------------

tab_author <- 
  tabItem("author", 
          narrowDiv(
            includeMarkdown("./rmarkdown/author/author.Rmd")
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
    tab_foreshift,
    tab_author
))

dashboardPage(header, sidebar, body, skin = "black")

