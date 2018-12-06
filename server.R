# library(eflows)
# library(eflows.viz)
# library(dplyr)
# library(R6)
# 
# source("functions/data_preprocessing.R", local = TRUE)
# source("functions/utils.R", local = TRUE)


shinyServer(function(input, output, session) { 
  
  

# reactive variables ------------------------------------------------------

  master_seed <- reactiveVal(sample(1:100,1))  
  
 

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
  
  evs <- o_Xev$clone(deep = TRUE)

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
    
    evs$do_foreshift(fit = formula_ev())
    

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
 
  input_evsoc <- reactive({
    c(input$ev1socvol[1], input$ev2socvol[1], input$ev3socvol[1], input$ev4socvol[1], input$ev5socvol[1])
  })
  input_evvol <- reactive({
    c(input$ev1socvol[2], input$ev2socvol[2], input$ev3socvol[2], input$ev4socvol[2], input$ev5socvol[2])
  })
  input_evcap <- reactive({
    c(input$ev1cap2, input$ev2cap2, input$ev3cap2, input$ev4cap2, input$ev5cap2)/60
  })
  input_evlevel <- reactive({
    c(input$ev1level, input$ev2level, input$ev3level, input$ev4level, input$ev5level)
  })
  
  
  # define list for SOC and flow
  
  socflow <- reactive({
    set.seed(master_seed() + cap_random_out()$button)
    s <- list(0)
    f <- list(0)
    # l <- c()
    
    s[[1]] <- input_evsoc()
    i <- 1
    
    defcap <- c()
    if (cap_random_out()$switch == TRUE) {
      defcap[i] <- 1 + runif(1, -0.1, 0.1)
    } else {
      defcap[i] <- 1
    }
    
    
    while (TRUE) {
      i <- i + 1
      
      if (cap_random_out()$switch == TRUE) {
        defcap[i] <- defcap[i - 1] + runif(1, -0.1, 0.1)
      } else {
        defcap[i] <- defcap[i - 1]
      }
      
      current_flow <- (input$cap_evs_pwr/60) * defcap[i]
      if (current_flow < 5/60) {
        defcap[i] <- defcap[i] + 0.2
        current_flow <- (input$cap_evs_pwr/60) * defcap[i]
      }
      
      temp <- eflows::distribute(flow = current_flow,
                                   # (input$cap_evs_pwr/60) * defcap[i], 
                                 soc = s[[i - 1]], 
                                 vol = input_evvol(), 
                                 cap = input_evcap(), 
                                 eff = input$eff_evs_pwr,
                                 level = input_evlevel()
      )
      s[[i]] <- temp[[1]]
      f[[i]] <- temp[[2]]*60
      # l[i] <- temp[[3]]*60
      
      # if the last result is the same, aus
      if (identical(s[[i]], s[[i - 1]])) break
    }
    
    # state of charge
    soc <- do.call(rbind, s)
    completed <- apply(soc, 2, function(x){match(max(x), x)})
    for (i in 1:ncol(soc)) {
      soc[(completed[i] + 1):nrow(soc),i] <- NA
    }
    s2 <- soc %>% 
      as.data.frame() %>% 
      mutate(minutes = seq(1:nrow(.))) %>% 
      select(minutes, everything()) 
    colnames(s2) <- c("minutes", "EV 1", "EV 2", "EV 3", "EV 4", "EV 5")

    # flow
    flow <- do.call(rbind, f) 
    f2 <- flow %>% 
      as.data.frame() %>% 
      mutate(minutes = seq(1:nrow(.))) %>% 
      select(minutes, everything())
    colnames(f2) <- c("minutes", "EV 1", "EV 2", "EV 3", 
                      "EV 4", "EV 5")
    
   
    list(s2, f2, completed, defcap) #l
  })
  
  output$evs_soc <- renderDygraph({
    s2graph <- dygraph(socflow()[[1]]) %>% 
      dyHighlight(highlightSeriesBackgroundAlpha = 0.6,
                  highlightSeriesOpts = list(strokeWidth = 2)) %>% 
      dyLegend(show = "onmouseover", width = 50) %>% 
      dyCSS(system.file("css/dygraph_style.css", package = "eflows.viz")) %>% 
      dyOptions(fillGraph = TRUE, 
                colors = palette_pwr, 
                mobileDisableYTouch = TRUE,
                retainDateWindow = TRUE) %>% 
      dyAxis("x", label = "minutes of charge") %>% 
      dyAxis("y", "kWh")
    for (i in 1:length(socflow()[[3]])) {
      s2graph <-  dyEvent(s2graph,
                          x = socflow()[[3]][i], 
                          label = paste0("EV", i, ": ", socflow()[[3]][i], " min."),
                          color = palette_pwr[i])
    }
    s2graph 
  })
  
  output$evs_flow <- renderDygraph({
    dygraph(socflow()[[2]]) %>% 
      dyHighlight(highlightSeriesBackgroundAlpha = 0.6,
      highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyOptions(
        stackedGraph = TRUE,
        colors = palette_pwr, 
        mobileDisableYTouch = TRUE,
        retainDateWindow = TRUE)%>% 
      dyAxis("x", label = "minutes of charge") %>% 
      dyAxis("y", "kW") %>% 
      dyLegend(show = "onmouseover") %>% 
      dyCSS(system.file("css/dygraph_style.css", package = "eflows.viz")) %>% 
      eflows.viz:::add_cap(socflow()[[4]]*input$cap_evs_pwr)
  })

# variable fit curve ------------------------------------------------------
  
  customfit <- o_Xdemand$clone(deep = TRUE)

  fit_fshifted <- reactive({
    # theformula <- as.formula(c("~", input$fit_formula))
    customfit$do_foreshift(fit = formula_fit())
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
    if (fit_random_out()$switch == TRUE) {
      random_bundle2()[[input$fit.rbutton]]
    } else {
      fit_bundle()[[input$fit.rbutton]]
    }
  })

  output$fit_graphvars <- renderDygraph({
    fitvars[[input$fit.rbutton_vars]]
  })

  # observeEvent(input$fit_types, {
  #   updateSearchInput(session = session, "fit_formula", value = input$fit_types, trigger = TRUE)
  # })
  
  
  # with randomness
  
  therandom2 <- reactive({
    fit_random_out()$button
    
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
    
    # theformula <- as.formula(c("~", input$fit_formula))
    # therandom2()$do_foreshift(fit = theformula)
    
    
    
    therandom2()$do_foreshift(fit = formula_fit())
    
    
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
    fore_random_out()$button
    
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
    if (fore_random_out()$switch == TRUE) {
      random_bundle()[[input$random_rbutton]]
    } else {
      layered_bundle()[[input$random_rbutton]]
    }
  })
  


# OBSERVERS ---------------------------------------------------------------
  

# MODULES -----------------------------------------------------------------

  cap_random_out <- callModule(randomize, "cap_random_in") 
  fit_random_out <- callModule(randomize, "fit_random_in")
  fore_random_out <- callModule(randomize, "fore_random_in")
  
  formula_fit <- callModule(fitSelector, "formula_fit")
  formula_ev <- callModule(fitSelector, "formula_ev")
  
  
  output$testext <- renderText({
    
    jsonlite::toJSON(master_seed())
    
  })

})
