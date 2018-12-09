
shinyServer(function(input, output, session) { 

# reactive variables 
  master_seed <- reactiveVal(sample(1:100,1))  
  

# fitting (fit) basic -----------------------------------------------------


  
# fitting (fit) plus ------------------------------------------------------
  
  # data
  fit_raw <- o_Xdemand$clone(deep = TRUE)
  
  fit_bundle <- reactive({
    fshifted <- fit_raw$do_foreshift(fit = formula_fit())
    cap_used <- (".cap" %in% all.vars(formula_fit()))
    
    pre <- viz_fore_input(fshifted, show_cap = cap_used)
    post <- viz_fore_output(fshifted, show_cap = cap_used)
    comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
    fitcurve <- viz_fit(fshifted)
    
    bundle <- viz_bundle(pre, post, comp,
               ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
               names = c("original", "foreshifted", "comparison"))
    bundle[["fitcurve"]] <- fitcurve
    bundle
  })
  
  fit_random_profile <- reactive({
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
  
  fit_random_bundle <- reactive({
    
    fit_random_profile()$do_foreshift(fit = formula_fit())
    cap_used <- (".cap" %in% all.vars(formula_fit()))
    
    pre <- viz_fore_input(o_random, show_cap = cap_used)
    post <- viz_fore_output(o_random, show_cap = cap_used)
    comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
    
    viz_bundle(pre, post, comp,
               ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
               names = c("original", "foreshifted", "comparison"))
  })
  
  # build
  formula_fit <- callModule(fitSelector, "formula_fit")
  
  callModule(dyRadioSelector, "factors_fit", reactive(fitvars))
  
  output$fit_fitcurve <- renderDygraph({
    fit_bundle()[["fitcurve"]]
  })
  
  callModule(dyRadioSelector, "graph_fit_plus",
             iftrue = reactive(fit_random_bundle()), 
             iffalse = reactive(fit_bundle()),
             condition = reactive(fit_random_out()$switch))
  fit_random_out <- callModule(randomize, "fit_random_in")
  
  
# foreshift (fsh) basic -------------------------------------------------------
  # data
  p_basic <- o_1demand$clone(deep = TRUE)
  
  fsh_bundle <- reactive({
    p_basic$demand$input$flex[[1]]$data <- as.matrix((rep(input$vol,168)))
    p_basic$demand$input$flex[[1]]$steps <- input$hflex
    
    p_basic$do_foreshift()
    
    pre <- viz_fore_input(p_basic, show_cap = FALSE)
    post <- viz_fore_output(p_basic, show_cap = FALSE)
    comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
    
    m2 <- max_yaxis(list_stacked = list(pre), list_unstacked = list(comp))
    
    viz_bundle(pre, post, comp,
               ymax = m2,
               names = c("original", "foreshifted", "comparison"), 
               group = "initialgroup")
  })
  
  # build
  callModule(dyRadioSelector, "graph_fsh_basic", reactive(fsh_bundle()))
  
# foreshift (fsh) plus -------------------------------------------------------
  # data (layered)
  o_layered <- o_Xdemand$clone(deep = TRUE)$do_foreshift()
  
  fsh_plus_bundle <- reactive({
    pre <- viz_fore_input(o_layered, show_cap = FALSE)
    post <- viz_fore_output(o_layered, show_cap = FALSE)
    comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
    
    viz_bundle(pre, post, comp,
               ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
               names = c("original", "foreshifted", "comparison"))
  })
  
  # data (random)
  o_random <- o_Xdemand$clone(deep = TRUE)
  
  fsh_random_bundle <- reactive({
    fsh_random_out()$button
    
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
    
    pre <- viz_fore_input(o_random, show_cap = FALSE)
    post <- viz_fore_output(o_random, show_cap = FALSE)
    comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
    
    viz_bundle(pre, post, comp,
               ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
               names = c("original", "foreshifted", "comparison"))
  })
  
  # build
  callModule(dyRadioSelector, "graph_fsh_plus",
             iftrue = reactive(fsh_random_bundle()), 
             iffalse = reactive(fsh_plus_bundle()),
             condition = reactive(fsh_random_out()$switch))
  fsh_random_out <- callModule(randomize, "fsh_random_in")
  

# ev (ev) one -------------------------------------------------------------
  # data
  p_1ev <- o_1ev$clone()
  
  ev_one_bundle <- reactive({
    ev0$data <- do_ev_prof(ev0$data, 
                           inputs = c(input$ev0flex2, input$ev0flex6, input$ev0flex12), 
                           pos = input$ev0pos,
                           cap = input$ev0cap)
    pre <- viz_fore_input(p_1ev, show_cap = FALSE)
    
    ev0$data <- do_ev_prof(ev0$data, 
                           inputs = c(input$ev0flex2, input$ev0flex6, input$ev0flex12), 
                           pos = input$ev0pos,
                           cap = 0)
    ev0$cap <- input$ev0cap
    
    p_1ev$do_foreshift()
    post <- viz_fore_output(p_1ev, show_cap = FALSE)
    comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
    
    viz_bundle(pre, post, comp,
               ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
               names = c("original", "foreshifted", "comparison"), 
               group = "initialgroup")
    
  })
  
  # build
  callModule(dyRadioSelector, "graph_ev_one", reactive(ev_one_bundle()))
  
  # output$p_1ev_graph <- renderDygraph({
  #   p_1ev_bundle()[[input$p_1ev_rbutton]]
  # })

# ev (ev) multi -----------------------------------------------------------
  # data
  evs <- o_Xev$clone(deep = TRUE)
  
  ev_multi_bundle <- reactive({
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
    
    cap_used <- (".cap" %in% all.vars(formula_ev()))
    pre <- viz_fore_input(evs, show_cap = cap_used)
    
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
    
    post <- viz_fore_output(evs, show_cap = cap_used)
    post_ev <- viz_fore_output(evs, aggregate = "object", show_cap = cap_used)
    post_flex <- viz_fore_output(evs, aggregate = "flex", show_cap = cap_used)
    comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
    unstacked <- viz_fore_output(evs, aggregate = "object", show_fixed = FALSE, stacked = FALSE, show_cap = FALSE)
    fitcurve <- viz_fit(evs)
    
    bundle <- viz_bundle(pre, post, post_ev,post_flex, comp, unstacked,
               ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
               names = c("original", "foreshifted","aggregated by ev", 
                         "aggregated by flex", "comparison", "unstacked"))
    bundle[["fitcurve"]] <- fitcurve
    bundle
  })
  
  #build
  formula_ev <- callModule(fitSelector, "formula_ev")
  
  callModule(dyRadioSelector, "factors_ev", reactive(fitvars))
  
  output$ev_multi_fitcurve <- renderDygraph({
    ev_multi_bundle()[["fitcurve"]]
    
  })
  
  callModule(dyRadioSelector, "graph_evs", reactive(ev_multi_bundle()))


# ev (ev) pwr -------------------------------------------------------------
  # data
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
  
  # reactive produced with long imperative code
  socflow <- reactive({
    set.seed(master_seed() + cap_random_out()$button)
    s <- list(0)
    f <- list(0)
    
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
                                 soc = s[[i - 1]], 
                                 vol = input_evvol(), 
                                 cap = input_evcap(), 
                                 eff = input$eff_evs_pwr,
                                 level = input_evlevel()
      )
      s[[i]] <- temp[[1]]
      f[[i]] <- temp[[2]]*60
     
      # if the last result is the same, terminate
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
   
    list(s2, f2, completed, defcap)
  })
  
  # build
  cap_random_out <- callModule(randomize, "cap_random_in") 
  
  # graph: SOC
  output$ev_pwr_soc <- renderDygraph({
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
  
  # graph: power flow
  output$ev_pwr_flow <- renderDygraph({
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
  
})
