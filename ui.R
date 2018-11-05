library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dygraphs)




# generator labels --------------------------------------------------------
tabPanelEV <- function(int){
  tabPanel(sprintf("EV %s", int), 
           flowLayout(sliderInput(sprintf("ev%sflex2", int),
                                  label = p("Charge in the next 2 hours", style = "color: white; padding:0px 3px;background-color: #338333;border-radius: 2px;margin-bottom:-1px;"), 
                                  min = 0,max = 40, value = 10, step = 1, ticks = FALSE, post = " kWh flex2"),
                      sliderInput(sprintf("ev%sflex6", int), 
                                  label = p("Charge in the next 6 hours", style = "color: white; padding:0px 3px;background-color: #89a54f;border-radius: 2px;margin-bottom:-1px;"), 
                                  min = 0, max = 40, value = 10, step = 1, ticks = FALSE, post = " kWh flex6"),
                      sliderInput(sprintf("ev%sflex12", int),
                                  label = p("Charge in the next 12 hours", style = "color: white; padding:0px 3px;background-color: #ffb733;border-radius: 2px;margin-bottom:-1px;"), 
                                  min = 0, max = 40, value = 10, step = 1, ticks = FALSE, post = " kWh flex12"),
                      sliderInput(sprintf("ev%scap", int), label = "Max charge power", min = 4,
                                  max = 40, value = 20, step = 1, ticks = FALSE, post = " kW")), 
           setSliderColor("#FF4500", 2),
           tags$strong("Charging start time"),
               sliderInput(sprintf("ev%spos", int), label = NULL, min = 1, max = 168, value = sample(seq(1,168),1), 
                           ticks = FALSE, post = " hours from start"
                           
                           
           )
  )
}

list_formulas <- list(`Peak shaving` = "1* .demand", 
                      `To the lowest demand` = "1*.demand_fixed",
                      `To the minimum price` = "1* .price",
                      `To the renewable energy` = "- 1*.production_fixed",
                      `Renewable within a limit` = "ifelse(.demand < 60, (- 1*.production_fixed), NA)", # Change for real cap!
                      `Net balance` = ".demand - .production_fixed",
                      `Market price` = "(0.5 * .price) + (0.5 * .demand)",
                      `The middle point` = "(0.3 * .price) + (0.4 * .demand) + (-0.3 * .production_fixed)",
                      `Conditional day and night` = "ifelse(.production_fixed > 0, .demand - .production_fixed, (0.5 * .price) + (0.5 * .demand))",
                      `Indifferent to other factors` = ".demand - .demand_fixed"
                      )


inputDiv <- function(...){
  box(width = 12, class = "inputDiv", ...)
}

narrowDiv <- function(...){
  fluidRow(div(style = "max-width: 700px;margin: 0 auto; padding: 20px; font-family: Georgia, Times, serif; font-size: 120%;", 
               ...
  ))
}

broadDiv <- function(..., title = NULL){
  fluidRow(div(style = "max-width: 1150px;margin: 0 auto; padding: 20px", 
               ...
  ))
}

mreDiv <- function(..., title = NULL){
  fluidRow(div(style = "max-width: 700px;margin: 0 auto; padding: 20px;", 
               box(title = title, width = 12, collapsible = TRUE, collapsed = TRUE, ...)
               )
           )
}

wideDiv <- function(..., title = NULL){
  fluidRow(div(style = "max-width: 1150px;margin: 0 auto; padding: 20px", 
               box(width = 12, title = title, class = "wideDivBox", ...)
  ))
}

# Header elements for the visualization
header <- dashboardHeader(title = "eflows", disable = TRUE)


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
          broadDiv(img(src = "images/main-banner.png", width = "100%"), 
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
                            tags$a(href="https://github.com/cvmartin/eflows", target="_blank",
                                   actionBttn(
                                     inputId = "git_eflows",
                                     label = "eflows",
                                     color = "primary",
                                     style = "bordered",
                                     icon = icon("github")
                                   ))),
                     column(width = 6, style = "text-align: left;",
                            tags$a(href="https://github.com/cvmartin/eflows.viz", target="_blank",
                                   actionBttn(
                                     inputId = "git_eflows_viz",
                                     label = "eflows.viz",
                                     color = "primary",
                                     style = "bordered",
                                     icon = icon("github")
                                   )))
                     
                   )
          )
  )


# fitting -----------------------------------------------------------------

tab_fitting <- 
  tabItem("fitting", 
          narrowDiv(
            includeMarkdown("./rmarkdown/fitting/fitting_curve.Rmd")
          ),
          wideDiv(title = "Custom fit curve",
                  
                  inputDiv(
                    
                    column(width = 9, 
                           searchInput("fit_formula", tagList("Fit formula:", tags$code("fit = ~")),
                                       value = "1*.demand", btnSearch = icon("level-down"), width = "100%")
                    ), 
                    column(width = 3, 
                           selectInput("fit_types", "Predefined formulas", choices = list_formulas))
                    
                  ),
                  box(width = 12, title = "Factors that influence the fitting curve", collapsible = TRUE,
                      radioGroupButtons("fit.rbutton_vars", NULL, c("demand", "production", "price"), justified = TRUE),
                      dygraphOutput("fit_graphvars", height = 230
                      )),
                  box(width = 12, title = "Fitting curves", collapsible = TRUE,
                      dygraphOutput("fit_fitcurve", height = 230)
                  ), 
                  box(width = 12,
                      radioGroupButtons("fit.rbutton", NULL, c("original", "foreshifted", "comparison"), justified = TRUE),
                      dygraphOutput("fit_graph", height = 230)
                  )
                  
          )
  )


# ev ----------------------------------------------------------------------

tab_ev <- 
  tabItem("ev", 
          wideDiv(title = "Electric Vehicle", 
                  inputDiv(tabPanelEV("0")),
                  box(width = 12, 
                      radioGroupButtons("p_1ev_rbutton", NULL, 
                                        c("original", "foreshifted", "comparison"), justified = TRUE), 
                      dygraphOutput("p_1ev_graph", height = 230))
                  
          ), 
          wideDiv(title = "Multiple Electric Vehicles", 
                  tabBox(title = "Electric Vehicles", width = 12,
                         id = "tab_evs",
                         tabPanelEV("1"), 
                         tabPanelEV("2"), 
                         tabPanelEV("3"),
                         tabPanelEV("4") 
                  ),
                  
                  box(width = 12, 
                      radioGroupButtons("evs.rbutton", NULL, 
                                        c("original", "foreshifted","aggregated by ev", "aggregated by flex", "comparison", "unstacked"), justified = TRUE), 
                      dygraphOutput("evs_graph", height = 230))
                  
          ))


# principles --------------------------------------------------------------

tab_principles <- 
  tabItem("principles", 
          narrowDiv(
            includeMarkdown("./rmarkdown/principles/principles.Rmd")
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
                    dygraphOutput("test_graph", height = 230))
                
        ) 
        
) 

###

body <- dashboardBody(
  tabItems(
    tab_home,
    tab_fitting,
    tab_ev,
    tab_principles,
    tab_foreshift
))

dashboardPage(header, sidebar, body, skin = "black")

