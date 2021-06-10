#' mod_tab_backshift UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_tab_backshift_ui <- function(id){
  ns <- NS(id)
  
  sidebar <- menuSubItem("backshift (BETA)", tabName = "backshift")
  
  content <- 
    tabItem("backshift", 
            narrowDiv(
              include_md_text("backshift/backshift-intro.md")
            ), 
            wideDiv( title = "The cost of backshifing",
                     inputDiv(
                       column(3,
                              sliderInput(ns("bsh_cost_self_discharge"),
                                          "Self discharge",
                                          min = 0, max = 10, value = 1, step = 0.1,
                                          ticks = FALSE, post = " % per hour")
                       ),
                       column(3,
                              sliderInput(ns("bsh_cost_eff_to"),
                                          "Efficiency to battery:",
                                          min = 85, max = 100, value = 100,
                                          ticks = FALSE, post = " %")
                       ),
                       column(3,
                              sliderInput(ns("bsh_cost_eff_from"),
                                          "Efficiency from battery:",
                                          min = 85, max = 100, value = 100,
                                          ticks = FALSE, post = " %")
                       ),
                       column(3,
                              sliderInput(ns("bsh_cost_back_time"),
                                          "Time horizon",
                                          min = 1, max = 24, value = 12,
                                          ticks = FALSE, post = " hours")
                       ),
                       column(12,
                              div(
                                style = "padding-left: 60px",
                                tags$strong("Point in time"),
                                sliderInput(ns("bsh_cost_back_point"), label = NULL,
                                            min = 1, max = 168, value = 94,
                                            ticks = FALSE, post = " hours from the start")
                              )
                              
                       )
                     ),
                     box(width = 12,
                         dygraphs::dygraphOutput(ns("graph_bsh_cost"), height = Sys.getenv("DYGRAPH_HEIGHT")),
                         dyCornerDiv(randomizeInput(ns("bsh_cost_random_in"), label = "Random profile"))
                     )
            ),
            wideDiv(title = "Fitting to backshift", 
                    inputDiv(
                      column(3,
                             sliderInput(ns("bsh_fit_self_discharge"),
                                         "Self discharge",
                                         min = 0, max = 10, value = 1, step = 0.1,
                                         ticks = FALSE, post = " % per hour")
                      ), 
                      column(3,
                             sliderInput(ns("bsh_fit_eff_to"),
                                         "Efficiency to battery:",
                                         min = 85, max = 100, value = 100,
                                         ticks = FALSE, post = " %")
                      ), 
                      column(3,
                             sliderInput(ns("bsh_fit_eff_from"),
                                         "Efficiency from battery:",
                                         min = 85, max = 100, value = 100,
                                         ticks = FALSE, post = " %")
                      ), 
                      column(3, 
                             sliderInput(ns("bsh_fit_back_time"),
                                         "Time horizon",
                                         min = 1, max = 24, value = 12,
                                         ticks = FALSE, post = " hours")
                      ),
                      ##
                      
                      column(2, 
                             sliderInput(ns("bsh_fit_pwr_to"),
                                         "Max. charge rate",
                                         min = 0, max = 5, value = 3,
                                         ticks = FALSE, post = " kW")
                      ),
                      column(2, 
                             sliderInput(ns("bsh_fit_pwr_from"),
                                         "Max. discharge rate",
                                         min = 0, max = 5, value = 3,
                                         ticks = FALSE, post = " kW")
                      ),
                      column(3, 
                             sliderInput(ns("bsh_fit_soc_init"),
                                         "Initial State Of Charge",
                                         min = 0, max = 100, value = 5,
                                         ticks = FALSE, post = " %")
                      ),
                      column(3,
                             sliderInput(ns("bsh_fit_vol"),
                                         "Storage capacity",
                                         min = 0, max = 100, value = 5,
                                         ticks = FALSE, post = " kWh")
                      ),
                      column(2,
                             div(style = "text-align:center; padding-top:22px;",
                                 switchInput("switch_bsh_fit_vol_unlimit",
                                             label = "Unlimited",
                                             labelWidth = 100,
                                             inline = TRUE)
                             )
                      )
                    ),
                    fitSelectorInput(ns("formula_bsh_fit")),
                    
                    box(width = 12, title = "Factors that influence the fitting curve", 
                        collapsible = TRUE, collapsed = TRUE,
                        dyRadioSelectorUI(ns("factors_bsh_fit"), c("demand", "production", "price", "grid capacity"))
                    ),
                    box(width = 12, title = "Fitting curves", collapsible = TRUE, collapsed = TRUE,
                        dygraphs::dygraphOutput(ns("bsh_fit_fitcurve"), height = Sys.getenv("DYGRAPH_HEIGHT"))
                    ), 
                    box(width = 12, 
                        dyRadioSelectorUI(ns("graph_bsh_fit"), c("potential", "backshifted", "comparison")),
                        dyRadioSelectorUI(ns("graph_bsh_str"), c("storage: exchange", "storage: soc"), height = 120)
                    )
            )
    )
  
  list(
    sidebar = sidebar, 
    content = content
  )
}
    
#'  mod_tab_backshift Server Function
#'
#' @noRd 
mod_tab_backshift_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session
    
    bsh_cost_random_out <- callModule(randomize, "bsh_cost_random_in")
    
    bsh_cost_randomvec <- reactive({
      if (bsh_cost_random_out()$switch) {
        vec_normal(168, sd = 0.2)
      } else {
        rep(1,168)
      }
    })
    
    p_only_demand <- o_bare$clone(deep = TRUE)
    
    o_only_demand <- reactive({
      p_only_demand$set_demand(e_demand$new(
        fixed = ((eflows::sept$d_house_smooth[1:168]*120) - 5) * bsh_cost_randomvec()))
    })
    
    bsh_cost_backcurve <- reactive({
      vec <- o_only_demand()$demand$input$fixed

      piece <- diff(range(vec))/100

      middle <- eflows:::depreciate(
        vector = rep((vec[input$bsh_cost_back_point]) - piece,
                     (input$bsh_cost_back_time + 1)),
        self_discharge = input$bsh_cost_self_discharge/100,
        eff_to = input$bsh_cost_eff_to/100,
        eff_from = input$bsh_cost_eff_from/100,
        backwards = TRUE
      )

      tooearly <- (input$bsh_cost_back_point - input$bsh_cost_back_time) < 0

      if (tooearly) {
        from <- abs(input$bsh_cost_back_point - input$bsh_cost_back_time)
        middle <- c(middle[from:length(middle)],rep(NA, from))
      }

      behind <- rep(NA, ifelse(tooearly, 0,
                               input$bsh_cost_back_point - input$bsh_cost_back_time -1))

      front <- rep(NA, length(vec) - input$bsh_cost_back_point)

      c(behind, middle, front)
    })
    
    output$graph_bsh_cost <- dygraphs::renderDygraph({

      palette <- bsh_cost_backcurve() > o_only_demand()$demand$input$fixed
      palette <- is.na(palette) | palette == FALSE


      viz_fore_input(o_only_demand()) %>%
        eflows.viz:::add_cap(bsh_cost_backcurve(),
                             label = "depreciation curve",
                             color = "#7A378B") %>%
        dygraphs::dyRibbon(palette, palette = c("#fff5bf", "white"))
    })
    
    # backshift (bsh) basic ----------------------------------------------------
    # data
    bsh_basic_bundle <- reactive({

      o_only_demand()$
        set_storage(e_storage$new(storage$new(vol = 23,
                                              eff = list(input$bsh_cost_eff_to/100,
                                                         input$bsh_cost_eff_from/100),
                                              self_discharge = input$bsh_cost_self_discharge/100,
                                              name = "battery")))

      o_only_demand()$do_backshift(input$bsh_cost_back_time)

      potential <- viz_back_potential(o_only_demand())
      post <- viz_back_output(o_only_demand())
      comp <- viz_compare(list(potential, post), c("original", "backshifted"))

      bundle <- viz_bundle(potential, post, comp,
                           ymax = max_yaxis(list_stacked = list(potential), list_unstacked = list(comp)),
                           names = c("potential", "backshifted", "comparison"))
      bundle
    })
    #build
    callModule(dyRadioSelector, "graph_bsh_basic", reactive(bsh_basic_bundle()))
    
    # backshift (bsh) fit ----------------------------------------------------
    # data
    bsh_fit_bundle <- reactive({

      o_only_demand <- reactive({
        e_frame$new(eflows::sept$datetime[1:168])$
          set_demand(e_demand$new(fixed = eflows::sept$d_house_smooth[1:168]*120 - 5))
      })

      o_only_demand()$
        set_production(e_production$new(fixed = list(solar = eflows::sept$solar[1:168]*120)))$
        set_price(eflows::sept$eprice[1:168]*0.6, unit = "euro/mWh")$
        set_cap((sin(seq.int(1,168)/10*3) + 55))$
        set_storage(e_storage$new(storage$new(vol = input$bsh_fit_vol,
                                              eff = list(input$bsh_fit_eff_to/100,
                                                         input$bsh_fit_eff_from/100),
                                              self_discharge = input$bsh_fit_self_discharge/100,
                                              name = "battery")))

      o_only_demand()$do_backshift(horizon = input$bsh_fit_back_time, fit = formula_bsh_fit())

      cap_used <- (".cap" %in% all.vars(formula_bsh_fit()))

      potential <- viz_back_potential(o_only_demand(), show_cap = cap_used)
      post <- viz_back_output(o_only_demand(), show_cap = cap_used)
      comp <- viz_compare(list(potential, post), c("original", "backshifted"))
      fitcurve <- viz_fit(o_only_demand())
      storage_flow <- viz_storage_flows(o_only_demand())
      storage_soc <- viz_storage_soc(o_only_demand())

      bundle <- viz_bundle(potential, post, comp,
                           ymax = max_yaxis(list_stacked = list(potential), list_unstacked = list(comp)),
                           names = c("potential", "backshifted", "comparison"))
      bundle[["fitcurve"]] <- fitcurve
      bundle[["storage: exchange"]] <- storage_flow
      bundle[["storage: soc"]] <- storage_soc

      bundle <- lapply(bundle, eflows.viz:::set_group, groupname = "bsh_fit_bundle")

      bundle
    })
    
    # build
    bsh_fit_vol <- callModule(battslider, "bsh_fit_vol") 
    
    formula_bsh_fit <- callModule(fitSelector, "formula_bsh_fit")
    
    callModule(dyRadioSelector, "factors_bsh_fit", reactive(
      viz_bundle(viz_demand_fixed(o_1demand),
                 viz_production_fixed(o_1demand),
                 viz_price(o_1demand),
                 viz_cap(o_1demand),
                 names = c("demand", "production", "price", "grid capacity"))
    ))
    
    callModule(dyRadioSelector, "graph_bsh_fit", reactive(bsh_fit_bundle()))
    
    callModule(dyRadioSelector, "graph_bsh_str", reactive(bsh_fit_bundle()))
    
    output$bsh_fit_fitcurve <- dygraphs::renderDygraph({
      bsh_fit_bundle()[["fitcurve"]]
    })
    
    })
}
    