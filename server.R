library(eflows)
library(eflows.viz)
library(dplyr)
library(R6)

source("functions/data_preprocessing.R", local = TRUE)
source("functions/utils.R", local = TRUE)


shinyServer(function(input, output, session) {
 

# test --------------------------------------------------------------------

  p_basic <- o_1demand$clone(deep = TRUE)
  
  
  test_bundle <- reactive({
    p_basic$demand$input$flex[[1]]$data <- as.matrix((rep(input$vol,168)))
    p_basic$demand$input$flex[[1]]$steps <- input$hflex
    
    do_fore_bundle(p_basic)
  })
  
  
  output$test_graph <- renderDygraph({
    test_bundle()[[input$test_rbutton]]
  })  
  

# single electric vehicle -------------------------------------------------

  p_1ev <- o_1ev$clone()
  
  p_1ev_bundle <- reactive({
    ev0$data <- do_ev_prof(ev0$data, 
                           inputs = c(input$ev0flex2, input$ev0flex6, input$ev0flex12), 
                           pos = input$ev0pos,
                           cap = input$ev0cap)
    pre <- viz_fore_input(p_1ev)
    
    ev0$data <- do_ev_prof(ev0$data, 
                           inputs = c(input$ev0flex2, input$ev0flex6, input$ev0flex12), 
                           pos = input$ev0pos,
                           cap = 0)
    p_1ev$do_foreshift()
    post <- viz_fore_output(p_1ev)
    comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
    
    viz_bundle(pre, post, comp,
               ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
               names = c("original", "foreshifted", "comparison"), 
               group = "initialgroup")
    
  })
  
  output$p_1ev_graph <- renderDygraph({
    p_1ev_bundle()[[input$p_1ev_rbutton]]
  })
  
  
# multiple electric vehicles -------------------------------------------------------
  
  evs <- o_Xev$clone()

  evs_bundle <- reactive({
    
    ev1$data <- do_ev_prof(ev1$data, 
                           inputs = c(input$ev1flex2, input$ev1flex6, input$ev1flex12), 
                           pos = input$ev1pos,
                           cap = input$ev1cap)
    ev2$data <- do_ev_prof(ev2$data, 
                           inputs = c(input$ev2flex2, input$ev2flex6, input$ev2flex12), 
                           pos = input$ev2pos,
                           cap = input$ev2cap)
    ev3$data <- do_ev_prof(ev3$data, 
                           inputs = c(input$ev3flex2, input$ev3flex6, input$ev3flex12), 
                           pos = input$ev3pos,
                           cap = input$ev3cap)
    ev4$data <- do_ev_prof(ev4$data, 
                           inputs = c(input$ev4flex2, input$ev4flex6, input$ev4flex12), 
                           pos = input$ev4pos,
                           cap = input$ev4cap)
    pre <- viz_fore_input(evs)
    
    ev1$data <- do_ev_prof(ev1$data, 
                           inputs = c(input$ev1flex2, input$ev1flex6, input$ev1flex12), 
                           pos = input$ev1pos,
                           cap = 0)
    ev2$data <- do_ev_prof(ev2$data, 
                           inputs = c(input$ev2flex2, input$ev2flex6, input$ev2flex12), 
                           pos = input$ev2pos,
                           cap = 0)
    ev3$data <- do_ev_prof(ev3$data, 
                           inputs = c(input$ev3flex2, input$ev3flex6, input$ev3flex12), 
                           pos = input$ev3pos,
                           cap = 0)
    ev4$data <- do_ev_prof(ev4$data, 
                           inputs = c(input$ev4flex2, input$ev4flex6, input$ev4flex12), 
                           pos = input$ev4pos,
                           cap = 0)
    
    evs$do_foreshift()
    
    post <- viz_fore_output(evs)
    post_ev <- viz_fore_output(evs, aggregate = "object")
    post_flex <- viz_fore_output(evs, aggregate = "flex")
    comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
    unstacked <- viz_fore_output(evs, aggregate = "object", show_fixed = FALSE, stacked = FALSE)
    
    viz_bundle(pre, post, post_ev,post_flex, comp, unstacked,
               ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
               names = c("original", "foreshifted","aggregated by ev", 
                         "aggregated by flex", "comparison", "unstacked"))

  })


  
  
  output$evs_graph <- renderDygraph({
    evs_bundle()[[input$evs.rbutton]]
  })


# variable fit curve ------------------------------------------------------
  
  customfit <- o_Xdemand$clone(deep = TRUE)

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

  observeEvent(input$fit_types, {
    updateSearchInput(session = session, "fit_formula", value = input$fit_types, trigger = TRUE)
  })
  

})
