library(eflows)
library(eflows.viz)
library(dplyr)


source("utils.R", local = TRUE)

load("www/preprocessed_data.rda")


shinyServer(function(input, output) {
  
 

# test --------------------------------------------------------------------

  obj1 <- flex_mtx$new(data = as.matrix((rep(3,168))),
                       steps = c(4),
                       name = "flexibility")
  
  p_demand <- e_demand$new(fixed = sept$d_house_smooth[1:168]*10,
                           flex = list(obj1))
  
  proto <- e_frame$new(sept$datetime[1:168])
  proto$set_demand(p_demand)
  
  
  test_bundle <- reactive({
    obj1$data <- as.matrix((rep(input$vol,168)))
    obj1$steps <- input$hflex
    
    do_fore_bundle(proto)
  })
  
  
  output$test_graph <- renderDygraph({
    test_bundle()[[input$test_rbutton]]
  })  
  

# amsterdam ---------------------------------------------------------------
  
  amst <- e_frame$new(sept$datetime[1:168])
  
  original_data <-  evdistrict[["nieuw_west"]] %>%
    select(-datetime, -`0`) %>%
    slice(1:168) %>%
    as.matrix()
  
  amst_demand_original <- e_demand$new(fixed = as.vector(sept$d_house_smooth[1:168]),
                                       flex = list(flex_mtx$new(original_data, c(2,4,6,8,10,12), "ev")
                                       ))
  amst$set_demand(amst_demand_original)
  
  ams_bundle <- reactive({
    evmatrix <- evdistrict[[input$selector]] %>%
      select( -datetime, -`0`) %>%
      slice(1:168) %>%
      as.matrix()
    
    amst_demand_original$input$flex[[1]]$data <- evmatrix
    
    do_fore_bundle(amst)
  })
  
  output$select_district <- renderUI({
    selectInput("selector", "District", choices = names(evdistrict), selected = "zuid")
  })
  
  output$ams_graph <- renderDygraph({
    ams_bundle()[[input$ams.rbutton]]
  })  
  

# electric vehicles -------------------------------------------------------

  evs_demand <- e_demand$new(fixed = sept$d_house_smooth[1:168]*10,
                             flex = list(ev1, ev2, ev3, ev4))
  
  evs <- e_frame$new(sept$datetime[1:168])
  evs$set_demand(evs_demand)
  
  evs_bundle <- reactive({
    ev1$data[,1] <- peak_in_zeroes(168, input$ev1pos, input$ev1flex2)
    ev1$data[,2] <- peak_in_zeroes(168, input$ev1pos, input$ev1flex6)
    ev1$data[,3] <- peak_in_zeroes(168, input$ev1pos, input$ev1flex12)
    
    ev2$data[,1] <- peak_in_zeroes(168, input$ev2pos, input$ev2flex2)
    ev2$data[,2] <- peak_in_zeroes(168, input$ev2pos, input$ev2flex6)
    ev2$data[,3] <- peak_in_zeroes(168, input$ev2pos, input$ev2flex12)
    
    ev3$data[,1] <- peak_in_zeroes(168, input$ev3pos, input$ev3flex2)
    ev3$data[,2] <- peak_in_zeroes(168, input$ev3pos, input$ev3flex6)
    ev3$data[,3] <- peak_in_zeroes(168, input$ev3pos, input$ev3flex12)
    
    ev4$data[,1] <- peak_in_zeroes(168, input$ev4pos, input$ev4flex2)
    ev4$data[,2] <- peak_in_zeroes(168, input$ev4pos, input$ev4flex6)
    ev4$data[,3] <- peak_in_zeroes(168, input$ev4pos, input$ev4flex12)
    
    do_fore_extended(evs)
  })
  
  output$evs_graph <- renderDygraph({
    evs_bundle()[[input$evs.rbutton]]
  })
  

# variable fit curve ------------------------------------------------------

  fit_fshifted <- eventReactive(c(input$fit_formula),{
    theformula <- as.formula(c("~", input$fit_formula))
    customfit$do_foreshift(fit = theformula)
  })
  
  fit_bundle <- reactive({
    pre <- viz_fore_input(fit_fshifted())
    post <- viz_fore_output(fit_fshifted())
    comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
    
    viz_bundle(pre, post, comp,
               ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
               names = c("original", "foreshifted", "comparison"))
  })
  
  output$fit_fitcurve <- renderDygraph({
    viz_fit(fit_fshifted())
  })
  
  output$fit_graph <- renderDygraph({
    fit_bundle()[[input$fit.rbutton]]
  })
  
  output$fit_graphvars <- renderDygraph({
    fitvars[[input$fit.rbutton_vars]]
  })  

})
