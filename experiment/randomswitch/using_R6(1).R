# the prep

library(R6)
MyModule <- R6Class(
  public = list(
    initialize = function(id){
      private$id <- id
    },
    bind = function(){
      callModule(private$module_server, private$id)
    },
    ui = function(ns = NS(NULL)){
      ns <- NS(ns(private$id))
      fluidPage(
        textInput(ns("text_in"), "text", "enter some text"),
        textOutput(ns("text_out"))
      )
    }
  ),
  private = list(
    id = NULL,
    module_server = function(input, output, session){
      ns <- session$ns
      output$text_out <- renderText({
        input$text_in
      })
    }
  )
)


# the shiny app -----------------------------------------------------------

library(shiny)

myObj <- MyModule$new("firsone")
myObj2 <- MyModule$new("secondone")

shinyApp(
  ui = fluidPage( myObj$ui(),
                  myObj2$ui()
  ), 
  server =  function(input, output, session){ 
    myObj$bind()
    myObj2$bind()
  }
)