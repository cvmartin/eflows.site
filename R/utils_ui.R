# generator labels --------------------------------------------------------
tabPanelEV <- function(int){
  tabPanel(sprintf("EV %s", int), 
           flowLayout(sliderInput(sprintf("ev%sflex2", int),
                                  label = p("Flexibility: 2 hours", style = "color: white; padding:0px 3px;background-color: #338333;border-radius: 2px;margin-bottom:-1px;"), 
                                  min = 0,max = 40, value = 10, step = 1, ticks = FALSE, post = " kWh flex2"),
                      sliderInput(sprintf("ev%sflex6", int), 
                                  label = p("Flexibility 6 hours", style = "color: white; padding:0px 3px;background-color: #89a54f;border-radius: 2px;margin-bottom:-1px;"), 
                                  min = 0, max = 40, value = 10, step = 1, ticks = FALSE, post = " kWh flex6"),
                      sliderInput(sprintf("ev%sflex12", int),
                                  label = p("Flexibility: 12 hours", style = "color: white; padding:0px 3px;background-color: #ffb733;border-radius: 2px;margin-bottom:-1px;"), 
                                  min = 0, max = 40, value = 10, step = 1, ticks = FALSE, post = " kWh flex12"),
                      sliderInput(sprintf("ev%scap", int), label = "Maximum charge rate", min = 4,
                                  max = 40, value = 20, step = 1, ticks = FALSE, post = " kW")), 
           div(
             style = "padding-left: 60px",
             tags$strong("Charging start time"),
             
               sliderInput(sprintf("ev%spos", int), label = NULL, min = 1, max = 168, value = sample(seq(1,168),1), 
                           ticks = FALSE, post = " hours from start") 
           )
  )
}

tabPanelPwrEV <- function(int, isoc, ivol, icap, ilevel){
  tabPanel(sprintf("EV %s", int), 
           fluidRow(
             column(6, 
                    sliderInput(sprintf("ev%ssocvol", int), 
                                label = "Initial State Of Charge - Final State of Charge (battery size)", 
                                min = 0, max = 100, value = c(isoc, ivol), step = 1, ticks = FALSE, post = " kWh")
             ),
             column(3, 
                    sliderInput(sprintf("ev%scap2", int),
                                label = "Maximum charge rate", 
                                min = 0, max = 40, value = icap, step = 1, ticks = FALSE, post = " kW")
             ),
             column(3, 
                    sliderInput(sprintf("ev%slevel", int), 
                                label = "Level (higher charges first)",
                                min = 1, max = 5, value = ilevel, step = 1, ticks = FALSE, post = " level")
             )
           )
  )
}


inputDiv <- function(...){
  box(width = 12, class = "inputDiv", ...)
}

narrowDiv <- function(...){
  fluidRow(tags$div(class = "narrowDiv", 
               ...
  ))
}

broadDiv <- function(..., title = NULL){
  fluidRow(div(style = "max-width: 1150px;margin: 0 auto; padding: 20px", 
               ...
  ))
}

mreDiv <- function(id, title = NULL, path, ...){
    div(class = "mreDiv",
               box(title = tagList(icon("code"), p(title, style = "display:inline;padding-left:10px")), 
                   width = 12, collapsible = TRUE, 
                   collapsed = TRUE,
                   aceEditor(outputId = id, 
                             value = readr::read_file(path), 
                             mode = "r", 
                             theme = "idle_fingers",
                             wordWrap = TRUE,
                             readOnly = TRUE, 
                             showLineNumbers = TRUE,
                             ...
                             )
                   )
  )
}

wideDiv <- function(..., title = NULL){
  fluidRow(div(style = "max-width: 1150px;margin: 0 auto; padding: 20px", 
               box(width = 12, title = title, class = "wideDivBox", ...)
  ))
}

dyCornerDiv <- function(...){
  tags$div(style = "float:right;margin-top:5px;margin-bottom:-5px;margin-right:-10px", 
           ...
           )
}   
