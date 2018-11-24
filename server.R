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
    ev0$cap <- input$ev0cap
    
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
    ev1$cap <- input$ev1cap
    
    ev2$data <- do_ev_prof(ev2$data, 
                           inputs = c(input$ev2flex2, input$ev2flex6, input$ev2flex12), 
                           pos = input$ev2pos,
                           cap = input$ev2cap)
    ev2$cap <- input$ev2cap
    
    ev3$data <- do_ev_prof(ev3$data, 
                           inputs = c(input$ev3flex2, input$ev3flex6, input$ev3flex12), 
                           pos = input$ev3pos,
                           cap = input$ev3cap)
    ev3$cap <- input$ev3cap
    
    ev4$data <- do_ev_prof(ev4$data, 
                           inputs = c(input$ev4flex2, input$ev4flex6, input$ev4flex12), 
                           pos = input$ev4pos,
                           cap = input$ev4cap)
    ev4$cap <- input$ev4cap
    
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


# EVs power ---------------------------------------------------------------

  palette_pwr <- gg_palette(5)
  
  # define list for SOC and flow
  
  s <- list(0)
  f <- list(0)
  
  # initial 
  
  s[[1]] <- c(40, 40, 15, 5, 30)
  
  i <- 1
  while (TRUE) {
    i <- i+1
    temp <- eflows::distribute(flow = (50/60), 
                               soc = s[[i-1]], 
                               vol = c(75, 50, 30, 50, 40), 
                               cap = c(20, 12, 10,12, 20), 
                               eff = 0.9,
                               level = c(1, 3, 0, 2, 2))
    
    s[[i]] <- temp[[1]]
    f[[i]] <- temp[[2]]
    
    # if the last result is the same, aus
    if (identical(s[[i]], s[[i-1]])) break
  }
  
  soc <- do.call(rbind, s)
  
  completed <- apply(soc, 2, function(x){match(max(x), x)})
  for (i in 1:ncol(soc)) {
    soc[(completed[i]+1):nrow(soc),i] <- NA
  }
  
  
  # state of charge
  
  s2 <- soc %>% 
    as.data.frame() %>% 
    mutate(minutes = seq(1:nrow(.))) %>% 
    select(minutes, everything()) 
  colnames(s2) <- c("minutes", "EV 1", "EV 2", "EV 3", "EV 4", "EV 5")
  
  s2graph <- dygraph(s2) %>% 
    dyHighlight(highlightSeriesBackgroundAlpha = 0.6,
                highlightSeriesOpts = list(strokeWidth = 2)) %>% 
    dyLegend(show = "onmouseover", width = 50) %>% 
    dyCSS(system.file("css/dygraph_style.css", package = "eflows.viz")) %>% 
    dyOptions(fillGraph = TRUE, 
              colors = palette_pwr, 
              mobileDisableYTouch = TRUE,
              retainDateWindow = TRUE) %>% 
    dyAxis("x", label = "minutes") %>% 
    dyAxis("y", "kWh")
  for (i in 1:length(completed)) {
    s2graph <-  dyEvent(s2graph,
                        x = completed[i], 
                        label = paste0("EV", i, ": ", completed[i], " minutes"),
                        color = palette_pwr[i])
  }
  
  output$evs_soc <- renderDygraph({
    s2graph 
  })
  
  # flow
  flow <- do.call(rbind, f) 
  
  f2 <- flow %>% 
    as.data.frame() %>% 
    mutate(minutes = seq(1:nrow(.))) %>% 
    select(minutes, everything())
  colnames(f2) <- c("minutes", "EV 1", "EV 2", "EV 3", "EV 4", "EV 5")
  
  
  output$evs_flow <- renderDygraph({
    dygraph(f2) %>% 
      dyHighlight(highlightSeriesBackgroundAlpha = 0.6,
                  highlightSeriesOpts = list(strokeWidth = 2)) %>% 
      dyOptions(stackedGraph = TRUE, 
                colors = palette_pwr, 
                mobileDisableYTouch = TRUE,
                retainDateWindow = TRUE)%>% 
      dyAxis("x", label = "minutes") %>% 
      dyAxis("y", "kWh") %>% 
      dyLegend(show = "onmouseover", width = 50) %>% 
      dyCSS(system.file("css/dygraph_style.css", package = "eflows.viz")) 
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
    if (input$random_on2 == TRUE){
      random_bundle2()[[input$fit.rbutton]]
    } else {
      fit_bundle()[[input$fit.rbutton]]
    }
  })

  output$fit_graphvars <- renderDygraph({
    fitvars[[input$fit.rbutton_vars]]
  })

  observeEvent(input$fit_types, {
    updateSearchInput(session = session, "fit_formula", value = input$fit_types, trigger = TRUE)
  })
  
  
  # with randomness
  
  therandom2 <- reactive({
    input$randomize2
    
    o_random$set_demand(e_demand$new(fixed = base_demand,
                                     flex = list(flex_mtx$new(data = cbind(vec_spiked(168, 2),
                                                                           vec_spiked(168, 2),
                                                                           vec_spiked(168, 2),
                                                                           vec_spiked(168, 2),
                                                                           vec_spiked(168, 2),
                                                                           vec_spiked(168, 2)),
                                                              steps = c(2,4,6,8,10,12),
                                                              name = "obj"))))
    
  })
  
  random_bundle2 <- reactive({
    
    theformula <- as.formula(c("~", input$fit_formula))
    therandom2()$do_foreshift(fit = theformula)
    
    pre <- viz_fore_input(o_random)
    post <- viz_fore_output(o_random)
    comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
    
    viz_bundle(pre, post, comp,
               ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
               names = c("original", "foreshifted", "comparison"))
  })
  
  

# random profile ----------------------------------------------------------

  o_layered <- o_Xdemand$clone(deep = TRUE)$
    do_foreshift()
  
  layered_bundle <- reactive({
    pre <- viz_fore_input(o_layered)
    post <- viz_fore_output(o_layered)
    comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
    
    viz_bundle(pre, post, comp,
               ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
               names = c("original", "foreshifted", "comparison"))
  })

  o_random <- o_Xdemand$clone(deep = TRUE)
 
  random_bundle <- reactive({
    input$randomize
    
    o_random$set_demand(e_demand$new(fixed = base_demand,
                                     flex = list(flex_mtx$new(data = cbind(vec_spiked(168, 2),
                                                                           vec_spiked(168, 2),
                                                                           vec_spiked(168, 2),
                                                                           vec_spiked(168, 2),
                                                                           vec_spiked(168, 2),
                                                                           vec_spiked(168, 2)),
                                                              steps = c(2,4,6,8,10,12),
                                                              name = "obj"))))
    o_random$do_foreshift()
    
    pre <- viz_fore_input(o_random)
    post <- viz_fore_output(o_random)
    comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
    
    viz_bundle(pre, post, comp,
               ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
               names = c("original", "foreshifted", "comparison"))
  })
  

  output$random_graph <- renderDygraph({
    if (input$random_on == TRUE){
      random_bundle()[[input$random_rbutton]]
    } else {
      layered_bundle()[[input$random_rbutton]]
    }
  })
  
  observeEvent(input$random_on, {
    toggleState("randomize")
  })

})
