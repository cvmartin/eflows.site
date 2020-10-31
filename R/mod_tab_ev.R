#' mod_tab_ev UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_tab_ev_ui <- function(id){
  ns <- NS(id)
  
  sidebar <- menuSubItem("Electric Vehicles charging", tabName = ns("ev"))
  
  content <- 
    tabItem(ns("ev"), 
            narrowDiv(
              include_md_text("ev/ev-1.md")
            ),
            wideDiv(title = "One Electric Vehicle", 
                    column(12, include_md_text("ev/ev-basic-pre.md")),
                    inputDiv(div(style = "padding: 10px", tabPanelEV("0", ns = ns))),
                    box(width = 12, 
                        dyRadioSelectorUI(ns("graph_ev_one"),
                                          c("original", "foreshifted","comparison"))
                    ),
                    column(12, include_md_text("ev/ev-basic-post.md"))
                    
            ), 
            narrowDiv(
              include_md_text("ev/ev-2.md")
            ),
            wideDiv(title = "Multiple Electric Vehicles", 
                    column(12, include_md_text("ev/ev-plus-pre.md")),
                    tabBox(title = "Electric Vehicles", width = 12,
                           id = ns("tab_evs"),
                           tabPanelEV("1", ns = ns), 
                           tabPanelEV("2", ns = ns), 
                           tabPanelEV("3", ns = ns),
                           tabPanelEV("4", ns = ns) 
                    ),
                    
                    fitSelectorInput(ns("formula_ev")),
                    
                    box(width = 12, title = "Factors that influence the fitting curve", 
                        collapsible = TRUE, collapsed = TRUE,
                        dyRadioSelectorUI(ns("factors_ev"), c("demand", "production", "price", "grid capacity"))
                    ),
                    box(width = 12, title = "Fitting curves", collapsible = TRUE, collapsed = TRUE,
                        dygraphs::dygraphOutput(ns("ev_multi_fitcurve"), height = Sys.getenv("DYGRAPH_HEIGHT"))
                    ), 
                    box(width = 12, 
                        dyRadioSelectorUI(ns("graph_evs"),
                                          c("original", "foreshifted","aggregated by ev", 
                                            "aggregated by flex", "comparison", "unstacked", "stacked"))
                    ),
                    column(12, include_md_text("ev/ev-plus-post.md"))
            ), 
            narrowDiv(
              include_md_text("ev/ev-3.md")
            ),
            mreDiv(
              ns("ev_timeframe"), "EV charging: Timeframe", "mre_ev_timeframe.R"
            ),
            wideDiv(title = "Electric Vehicles flexibility: large numbers",
                    column(12, include_md_text("ev/ev-dist-pre.md")),
                    inputDiv(
                      column(4, sliderInput(ns("ev_dist_evs"), "Number of EVs",
                                            min = 0, max = 200,
                                            value = 100, step = 5, ticks = FALSE)
                      ), 
                      column(4, sliderInput(ns("ev_dist_households"), "Number of households",
                                            min = 0, max = 200,
                                            value = 100, step = 5, ticks = FALSE)
                      ),
                      column(4, selectInput(ns("ev_dist_match"), 
                                            label = "Distict type",
                                            choices = list(Downtown = "Centrum", 
                                                           Residential = "Zuid", 
                                                           `Late night charging` = "Nieuw-West"))
                      )
                    ),
                    box(width = 12,
                        dyRadioSelectorUI(ns("graph_ev_dist"),
                                          c("original", "foreshifted","comparison"))
                    ),
                    column(12, include_md_text("ev/ev-dist-post.md"))
            ),
            narrowDiv(
              include_md_text("ev/ev-4.md")
            ),
            wideDiv(title = "Power distribution among multiple Electric Vehicles",
                    column(12, include_md_text("ev/ev-pwr-pre.md")),
                    tabBox(title = "Electric Vehicles", width = 12,
                           id = "tab_pwr_evs",
                           tabPanelPwrEV("1", 40, 75, 20, 2, ns = ns), 
                           tabPanelPwrEV("2", 40, 50, 12, 4, ns = ns), 
                           tabPanelPwrEV("3", 15, 30, 10, 1, ns = ns),
                           tabPanelPwrEV("4", 5, 50, 12, 3, ns = ns), 
                           tabPanelPwrEV("5", 30, 40, 20, 3, ns = ns)
                    ),
                    inputDiv(
                      column(4, sliderInput(ns("cap_evs_pwr"), "Grid capacity", min = 10, max = 100,
                                            value = 40, step = 5, ticks = FALSE, post = " kW")
                      ), 
                      column(4, 
                             div(style = "text-align:center; padding-top:22px;",
                                 randomizeInput(ns("cap_random_in"), label = "Random capacity"))
                      ),
                      column(4, sliderInput(ns("eff_evs_pwr"), "Battery charging efficiency", min = 0.75, max = 1, 
                                            value = 0.9,ticks = FALSE)
                      )
                    ),
                    box(width = 12, title = "EVs State Of Charge", 
                        dygraphs::dygraphOutput(ns("ev_pwr_soc"), height = Sys.getenv("DYGRAPH_HEIGHT"))
                    ),
                    box(width = 12, title = "Power flow into EVs",
                        dygraphs::dygraphOutput(ns("ev_pwr_flow"), height = Sys.getenv("DYGRAPH_HEIGHT"))
                    ),
                    column(12, include_md_text("ev/ev-pwr-post.md"))
            ),
            mreDiv(
              ns("ev_gridcapacity"), "EV charging: Grid Capacity", "mre_ev_gridcapacity.R"
            ),
            narrowDiv(
              include_md_text("ev/ev-5.md")
            )
    )
  
  list(
    sidebar = sidebar, 
    content = content
  )
}
    
#'  mod_tab_ev Server Function
#'
#' @noRd 
mod_tab_ev_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session
    
    master_seed <- reactiveVal(sample(1:100,1)) 
    
    p_1ev <- o_1ev$clone()
    # ev (ev) one -----------------------------------------------------------
    
    ev_one_bundle <- reactive({
      ev0$data <- do_ev_prof(ev0$data, 
                             inputs = c(input$ev0flex2, input$ev0flex6, input$ev0flex12), 
                             pos = input$ev0pos,
                             cap = input$ev0cap)
      pre <- eflows.viz::viz_fore_input(p_1ev, show_cap = FALSE)
      
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
      stacked <- viz_fore_output(evs, aggregate = "object", show_fixed = FALSE, show_cap = FALSE)
      fitcurve <- viz_fit(evs)
      
      bundle <- viz_bundle(pre, post, post_ev,post_flex, comp, unstacked, stacked,
                           ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
                           names = c("original", "foreshifted","aggregated by ev", 
                                     "aggregated by flex", "comparison", "unstacked", "stacked"))
      bundle[["fitcurve"]] <- fitcurve
      bundle
    })
    
    #build
    formula_ev <- callModule(fitSelector, "formula_ev")
    
    callModule(dyRadioSelector, "factors_ev", reactive(fitvars))
    
    output$ev_multi_fitcurve <- dygraphs::renderDygraph({
      ev_multi_bundle()[["fitcurve"]]
      
    })
    
    callModule(dyRadioSelector, "graph_evs", reactive(ev_multi_bundle()))
    
    # ev (ev) dist --------------------------------------------------------
    #data
    ev_dist <- readRDS(system.file('extdata', 'districts_flex.rds', package='eflows.site'))
    
    ev_dist_bundle <- reactive({
      
      ev_dist_df <- ev_dist %>% 
        filter(stadsdeel == input$ev_dist_match) %>% 
        select(- stadsdeel) %>% 
        spread(flex, kwh) %>% 
        slice(1:168)
      
      ev_dist_obj <- e_frame$new(ev_dist_df$datetime)
      
      ev_dist_obj$set_demand(e_demand$new(
        fixed = (eflows::sept$d_house_smooth[1:168] * input$ev_dist_households * 1.3),
        flex = list(flex_mtx$new(data = (cbind(ev_dist_df$`0`,
                                               ev_dist_df$`2`,
                                               ev_dist_df$`4`,
                                               ev_dist_df$`6`,
                                               ev_dist_df$`8`,
                                               ev_dist_df$`10`,
                                               ev_dist_df$`12`))*input$ev_dist_evs,
                                 steps = c(1,2,4,6,8,10,12),
                                 name = "district"))
      )) 
      
      ev_dist_obj$do_foreshift()
      
      pre <- viz_fore_input(ev_dist_obj)
      post <- viz_fore_output(ev_dist_obj)
      comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
      
      
      viz_bundle(pre, post, comp,
                 ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
                 names = c("original", "foreshifted", "comparison"), 
                 group = "districtgroup")
      
    })
    
    #build
    callModule(dyRadioSelector, "graph_ev_dist", reactive(ev_dist_bundle()))
    
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
    output$ev_pwr_soc <- dygraphs::renderDygraph({
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
    output$ev_pwr_flow <- dygraphs::renderDygraph({
      dygraph(socflow()[[2]]) %>% 
        dyHighlight(highlightSeriesBackgroundAlpha = 0.6,
                    highlightSeriesOpts = list(strokeWidth = 2)) %>%
        dyOptions(
          stackedGraph = TRUE,
          colors = palette_pwr, 
          mobileDisableYTouch = TRUE,
          retainDateWindow = TRUE) %>% 
        dyAxis("x", label = "minutes of charge") %>% 
        dyAxis("y", "kW") %>% 
        dyLegend(show = "onmouseover") %>% 
        dyCSS(system.file("css/dygraph_style.css", package = "eflows.viz")) %>% 
        eflows.viz:::add_cap(socflow()[[4]]*input$cap_evs_pwr)
    })
    
    })
}
    