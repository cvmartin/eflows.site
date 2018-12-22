library(shiny)
library(dygraphs)
library(dplyr)



ui <- fluidPage(
  tags$head(tags$script(src="synchronizer.js")),
  dygraphOutput("dy1"),
  dygraphOutput("dy2")
  
)

server <- function(input, output, session) {
  
  lungDeaths <- cbind(mdeaths, fdeaths)
  
  d1 <- reactive({dygraph(lungDeaths)})
  
  output$dy1 <- renderDygraph({
    d1()
  })
  
  d2 <- reactive({
    dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
      dySeries("mdeaths", drawPoints = TRUE, pointShape = "square", color = "blue") %>%
      dySeries("fdeaths", stepPlot = TRUE, fillGraph = TRUE, color = "red")
  })
  
  output$dy2 <- renderDygraph({
   d2()
  })
  
}

shinyApp(ui, server)