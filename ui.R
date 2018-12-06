
# Header elements for the visualization
header <- dashboardHeader(title = "eflows", disable = FALSE)

dy_height <- 210


# SIDEBAR -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
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
        
          
          # div(style = "text-align: center; font-size: 600%",
          #     p("eflows")), 
          # div(style = "text-align: center;font-size: 200%;",
          #     p("Data-driven energy transition")), 
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


# fitting -----------------------------------------------------------------

tab_fitting <- 
  tabItem("fitting", 
          narrowDiv(
            includeMarkdown("./rmarkdown/fitting/fitting_curve.Rmd")
          ),
          wideDiv(title = "Custom fit curve",
                  
                  fitSelectorInput("formula_fit"),
                  
                  # inputDiv(
                  #   
                  #   column(width = 9, 
                  #          searchInput("fit_formula", tagList("Fit formula:", tags$code("fit = ~")),
                  #                      value = "1*.demand", btnSearch = icon("level-down"), width = "100%")
                  #   ), 
                  #   column(width = 3, 
                  #          selectInput("fit_types", "Predefined formulas", choices = list_formulas))
                  #   
                  # ),
                  box(width = 12, title = "Factors that influence the fitting curve", collapsible = TRUE,
                      radioGroupButtons("fit.rbutton_vars", NULL, c("demand", "production", "price"), justified = TRUE),
                      dygraphOutput("fit_graphvars", height = dy_height
                      )),
                  box(width = 12, title = "Fitting curves", collapsible = TRUE,
                      dygraphOutput("fit_fitcurve", height = dy_height)
                  ), 
                  box(width = 12,
                      radioGroupButtons("fit.rbutton", NULL, c("original", "foreshifted", "comparison"), justified = TRUE),
                      dygraphOutput("fit_graph", height = dy_height),
                      dyCornerDiv(randomizeInput("fit_random_in", label = "Random profile"))
                          
                      
                  )
                  
          )
  )

# ev ----------------------------------------------------------------------

tab_ev <- 
  tabItem("ev", 
          narrowDiv(
            includeMarkdown("./rmarkdown/ev/ev-intro.Rmd")
          ),
          wideDiv(title = "Electric Vehicle", 
                  inputDiv(div(style = "padding: 10px", tabPanelEV("0"))),
                  box(width = 12, 
                      radioGroupButtons("p_1ev_rbutton", NULL, 
                                        c("original", "foreshifted", "comparison"), justified = TRUE), 
                      dygraphOutput("p_1ev_graph", height = dy_height))
                  
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
                  
                  box(width = 12, 
                      radioGroupButtons("evs.rbutton", NULL, 
                                        c("original", "foreshifted","aggregated by ev", "aggregated by flex", "comparison", "unstacked"), justified = TRUE), 
                      dygraphOutput("evs_graph", height = dy_height))
                  
          ), 
          wideDiv(title = "Power distribution", 
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
                                          value = 40, step = 5, ticks = FALSE, post = " kW")), 
                    column(4, 
                           div(style = "text-align:center; padding-top:22px;",
                               randomizeInput("cap_random_in", label = "Random capacity")
                           )
                    ),
                    column(4, sliderInput("eff_evs_pwr", "Battery charging efficiency", min = 0.75, max = 1, 
                                          value = 0.9,ticks = FALSE, post = " %"))
                  ),
                  box(width = 12, title = "EVs State Of Charge", 
                      dygraphOutput("evs_soc", height = dy_height)
                     ),
                  box(width = 12, title = "Power flow into EVs",
                      dygraphOutput("evs_flow", height = dy_height)
                  )
                  
          ))


# principles --------------------------------------------------------------

tab_principles <- 
  tabItem("principles", 
          narrowDiv(
            includeMarkdown("./rmarkdown/principles/principles.Rmd")
          ), 
          wideDiv(
          ))


# distribute --------------------------------------------------------------

tab_distribute <- 
  tabItem("distribute", 
          narrowDiv(
            includeMarkdown("./rmarkdown/distribute/distribute-intro.Rmd")
          ), 
          wideDiv(
          ))

# backshift --------------------------------------------------------------

tab_backshift <- 
  tabItem("backshift", 
          narrowDiv(
            includeMarkdown("./rmarkdown/backshift/backshift-intro.Rmd")
          ), 
          wideDiv(
          ))


# foreshift ---------------------------------------------------------------
tab_foreshift <- 
tabItem("foreshift", 
        narrowDiv(
          includeMarkdown("./rmarkdown/foreshift/foreshift.Rmd")
        ),
        wideDiv(title = "Basic example",
                inputDiv(
                  column(width = 6, 
                         sliderInput("hflex", label = "Hours of flexibility", min = 1,
                                     max = 12, value = 4, step = 1, ticks = FALSE)),
                  column(width = 6,
                         sliderInput("vol", label = "Flexible demand volume", min = 0,
                                     max = 30, value = 10, step = 0.1, ticks = FALSE))
                ),
                box(width = 12,
                    radioGroupButtons("test_rbutton", NULL, c("original", "foreshifted", "comparison"), justified = TRUE),
                    dygraphOutput("test_graph", height = dy_height))
                
        ),
        wideDiv(title = "Layers of flexibility",
                box(width = 12,
                    radioGroupButtons("random_rbutton", NULL, c("original", "foreshifted", "comparison"), justified = TRUE),
                    dygraphOutput("random_graph", height = dy_height), 
                    dyCornerDiv(randomizeInput("fore_random_in", label = "Random profile"))
                    )
                ) 
) 

# BUILD IT ALL

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

