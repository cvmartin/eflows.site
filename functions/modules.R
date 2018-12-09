

# Randomize (slider to randomize + randomize button) ----------------------

randomizeInput <- function(id, label = "Randomize profile") {
  
  ns <- NS(id)
  
  # div(
  tagList(
    singleton(tags$style('
.randomize_button .btn {
transition: background-color 0.5s ease;
color:white; 
background-color: #337ab7; 
border-color: #cccccc}

.randomize_button .btn:disabled {
transition: background-color 0.5s ease;
background-color: white ;
border-color: darkgray;
color: darkgray;
}

.randomize_button .btn:enabled:active {
background-color: #193f5f;
color: white;
}

')),
    div(
      style = "min-width: 270px",
      switchInput(ns("switch_rand"), 
                  label = label,
                  labelWidth = 140, 
                  inline = TRUE
      ), 
      div(style = "display:inline;", class = "randomize_button",
          actionButton(ns("do_rand"), NULL , icon = icon("random")))
    )
  )
}


randomize <- function(input, output, session) {
  
  observeEvent(input$switch_rand, toggleState("do_rand"))
  
  return(reactive({
    list(switch = input$switch_rand, 
         button = input$do_rand)
  })
  )
  
}


# fitSelector (associate input bar with predefined formulas) --------------

list_formulas <- list(`Peak shaving` = "1* .demand", 
                      `To the lowest demand` = "1*.demand_fixed",
                      `To the minimum price` = "1* .price",
                      `To the renewable energy` = "- 1*.production_fixed",
                      `Profit within a limit` = "ifelse(.demand < .cap, .price, NA)", 
                      `Net balance` = ".demand - .production_fixed",
                      `Market price` = "(0.5 * .price) + (0.5 * .demand)",
                      `The middle point` = "(0.3 * .price) + (0.4 * .demand) + (-0.3 * .production_fixed)",
                      `Conditional day and night` = "ifelse(.production_fixed > 0, .demand - .production_fixed, (0.5 * .price) + (0.5 * .demand))",
                      `Indifferent to other factors` = ".demand - .demand_fixed"
)

fitSelectorInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    singleton(tags$style('
                         .fitSelectorDiv {  padding:0;
                         padding-top:5px;
                         background-color: #f5f5f5;
                         border: 1px solid #e3e3e3;
                         border-radius: 2px;
                         }
                         .formulaInput .search-text {
                         font-family: Menlo,Monaco,Consolas,"Courier New",monospace;
                         } 
                         ')),
    box(width = 12, class = "fitSelectorDiv", 
        column(width = 9, style = "margin: 0px -5px -8px -5px;",
               div(class = "formulaInput",
               searchInput(ns("formula"), tagList("Fit formula:", tags$code("fit = ~")),
                           value = "1*.demand", btnSearch = icon("level-down"), width = "100%")
               )
        ), 
        column(width = 3, style = "margin: 0px -5px -8px -5px; padding-right:0;",
               selectInput(ns("predefined_formulas"), "Predefined formulas", choices = list_formulas))
    )
    )
  }



fitSelector <- function(input, output, session) {
  # it requires the very last version of shinyWidgets
  observeEvent(c(input$predefined_formulas), {
    updateSearchInput(session = session, 
                      inputId = "formula", 
                      value = input$predefined_formulas, 
                      trigger = TRUE)
  })
  
  current_formula <- reactive({ 
    as.formula(c("~", input$formula))
    })
  
  return(current_formula)
}


# dyRadioSelector -----------------------------------------------------------

dyRadioSelectorUI <- function(id, tabs) {
  ns <- NS(id)
  tags$div(
    radioGroupButtons(ns("dy_radio_buttons"), NULL, tabs, justified = TRUE),
    dygraphOutput(ns("dy_radio_graph"), height = dy_height)
  )
}

dyRadioSelector <- function(input, output, session, 
                            iftrue, 
                            iffalse = reactive(NULL), 
                            condition = reactive(FALSE)) {
  
  choice <- reactive({
    if (is.null(iffalse()) | condition() == TRUE) {
      iftrue()
    } else {
      iffalse()
    }
  })
  
 output$dy_radio_graph <-  
   renderDygraph({
     choice()[[input$dy_radio_buttons]]
   })
}