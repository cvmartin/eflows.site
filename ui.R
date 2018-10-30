library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dygraphs)




# generator labels --------------------------------------------------------
tabPanelEV <- function(int){
  tabPanel(sprintf("EV %s", int), 
           tags$strong("Battery allocation"),

           flowLayout(sliderInput(sprintf("ev%sflex2", int), label = NULL, min = 0,
                                  max = 40, value = 5, step = 1, ticks = FALSE, post = " kWh flex2"),
                      sliderInput(sprintf("ev%sflex6", int), label = NULL, min = 0,
                                  max = 40, value = 5, step = 1, ticks = FALSE, post = " kWh flex6"),
                      sliderInput(sprintf("ev%sflex12", int), label = NULL, min = 0,
                                  max = 40, value = 5, step = 1, ticks = FALSE, post = " kWh flex12")), 
           tags$strong("Placement in time"),
           
               sliderInput(sprintf("ev%spos", int), label = NULL, min = 1, max = 168, value = sample(seq(1,168),1), 
                           ticks = FALSE
                           
                           
           )
  )
}



inputDiv <- function(...){
  box(width = 12, class = "inputDiv", ...)
}

narrowDiv <- function(...){
  fluidRow(div(style = "max-width: 700px;margin: 0 auto; padding: 20px; font-family: Georgia, Times, serif; font-size: 120%;", 
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

# Sidebar elements for the search visualizations
sidebar <- dashboardSidebar(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  sidebarMenu(
    div(style = "text-align: center; font-size: 300%",
        p("eflows")),
    menuItem("Home", tabName = "index"),
    menuItem("Principles", tabName = "principles"),
    menuItem("Flexibility", tabName = "flex")
  ) 
) 

#Body elements for the search visualizations.
body <- dashboardBody(
  tabItems(
    tabItem("index", 
            div(style = "text-align: center; font-size: 600%",
                p("eflows")), 
            div(style = "text-align: center;font-size: 200%;",
                p("Data-driven energy transition"))
            ),
    tabItem("principles", 
            narrowDiv(
              includeMarkdown("./assets/principles.Rmd")
            ), 
            wideDiv(
            )),
    tabItem("flex", 
            narrowDiv(
              includeMarkdown("./assets/foreshift_1.Rmd")
            ),
            wideDiv(title = "Basic example",
                  inputDiv(
                    column(width = 6, 
                    sliderInput("hflex", label = "Hours of flexibility", min = 1,
                                max = 12, value = 4, step = 1, ticks = FALSE)),
                    column(width = 6,
                    sliderInput("vol", label = "Flexible demand volume", min = 0,
                                max = 3, value = 1, step = 0.1, ticks = FALSE))
                  ),
                  box(width = 12,
                  radioGroupButtons("test_rbutton", NULL, c("original", "foreshifted", "comparison"), justified = TRUE),
                  dygraphOutput("test_graph", height = 230))
              
            ), 
            narrowDiv(
              includeMarkdown("./assets/foreshift_2.Rmd")
              ),
            wideDiv(title = "Amsterdam districts",
                  inputPanel(
                    uiOutput("select_district")
                  ),
                  radioGroupButtons("ams.rbutton", NULL, c("original", "foreshifted", "comparison"), justified = TRUE),
                  dygraphOutput("ams_graph", height = 250)
            
            ), 
            narrowDiv(
              includeMarkdown("./assets/foreshift_2.Rmd")
            ),
            wideDiv(title = "Electric Vehicles example", 
                    inputDiv(tabBox(title = "Electric Vehicles", width = 12,
                      id = "tab_evs",
                      tabPanelEV("1"), 
                      tabPanelEV("2"), 
                      tabPanelEV("3"),
                      tabPanelEV("4") 
                     
                    )),
                 
                  box(width = 12, 
                  radioGroupButtons("evs.rbutton", NULL, c("original", "foreshifted","aggregated by ev", "aggregated by flex", "comparison"), justified = TRUE), 
                  dygraphOutput("evs_graph", height = 230))
                  
            ), 
            narrowDiv(
              includeMarkdown("./assets/foreshift_4.Rmd")
            ),
            wideDiv(title = "Custom fit curve",
              
                  inputDiv(
                    
                    column(width = 8, 
                           searchInput("fit_formula", tagList("Fit formula:", tags$code("fit = ~")),
                                       value = "1*.demand", btnSearch = icon("level-down"), width = "100%")
                    ), 
                    column(width = 4, 
                           selectInput("fit_types", "Predefined formulas", c(seq(1:10))))
                    
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
                  
                  
                  
            ), 
            mreDiv(title = "Example of a MRE", includeMarkdown("./assets/mre_example.Rmd"))
    ) 
    
          
))

dashboardPage(header, sidebar, body, skin = "black")

