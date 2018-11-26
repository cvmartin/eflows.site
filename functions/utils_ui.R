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
           tags$strong("Charging start time"),
           sliderInput(sprintf("ev%spos", int), label = NULL, min = 1, max = 168, value = sample(seq(1,168),1), 
                       ticks = FALSE, post = " hours from start"
                       
                       
           )
  )
}

tabPanelPwrEV <- function(int, isoc, ivol, icap, ilevel){
  tabPanel(sprintf("EV %s", int), 
           flowLayout(sliderInput(sprintf("ev%ssoc", int),
                                  label = "Initial SOC", 
                                  min = 0,max = 100, value = isoc, step = 1, ticks = FALSE, post = " kWh"),
                      sliderInput(sprintf("ev%svol", int), 
                                  label = "Battery volume", 
                                  min = 0, max = 100, value = ivol, step = 1, ticks = FALSE, post = " kWh"),
                      sliderInput(sprintf("ev%scap2", int),
                                  label = "Maximum charge rate", 
                                  min = 0, max = 40, value = icap, step = 1, ticks = FALSE, post = " kW"),
                      sliderInput(sprintf("ev%slevel", int), 
                                  label = "Level (charge preference)",
                                  min = 1, max = 5, value = ilevel, step = 1, ticks = FALSE, post = " level")
                      )
  )
}




list_formulas <- list(`Peak shaving` = "1* .demand", 
                      `To the lowest demand` = "1*.demand_fixed",
                      `To the minimum price` = "1* .price",
                      `To the renewable energy` = "- 1*.production_fixed",
                      `Renewable within a limit` = "ifelse(.demand < 60, (-.production_fixed), .demand)", 
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
  fluidRow(tags$div(class = "narrowDiv", style = "max-width: 700px;margin: 0 auto; padding: 20px; font-family: Georgia, Times, serif; font-size: 120%;", 
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
