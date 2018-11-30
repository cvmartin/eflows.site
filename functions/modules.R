

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