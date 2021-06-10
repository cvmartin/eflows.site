#' mod_tab_fitting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_tab_fitting_ui <- function(id) {
  ns <- NS(id)
  
  sidebar <- menuSubItem("Fitting formula and curve", tabName = "fitting")
  
  content <- 
    tabItem("fitting", 
            narrowDiv(
              include_md_text("fitting/fit-1.md")
            ),
            wideDiv(title = "Fitting curve: combining factors", 
                    column(12, include_md_text("fitting/fit-basic-pre.md")),
                    box(width = 12, title = "Factors that influence the fitting curve", 
                        collapsible = TRUE, collapsed = TRUE,
                        dyRadioSelectorUI(
                          ns("factors_fit_basic"), 
                          c("demand", "production", "price", "grid capacity")
                          )
                    ),
                    inputDiv(
                      column(3,
                             sliderInput(ns("fit_basic_demand"), 
                                         "Demand: relative importance",
                                         min = 0, max = 1, value = 0.5, 
                                         step = 0.1, ticks = FALSE)
                      ), 
                      column(3,
                             sliderInput(ns("fit_basic_solar"), 
                                         "Production: relative importance",
                                         min = 0, max = 1, value = 0.5, 
                                         step = 0.1, ticks = FALSE)
                      ), 
                      column(3,
                             sliderInput(ns("fit_basic_price"), 
                                         "Price: relative importance",
                                         min = 0, max = 1, value = 0.5, 
                                         step = 0.1, ticks = FALSE)
                      ), 
                      column(3, 
                             div(style = "text-align:center; padding-top:22px;",
                                 switchInput(ns("switch_fit_basic_cap"), 
                                             label = "Use grid capacity",
                                             labelWidth = 140, 
                                             inline = TRUE)
                             )
                      )
                    ),
                    box(width = 12,
                        dyRadioSelectorUI(
                          ns("graph_fit_basic"), 
                          c("original", "foreshifted", "comparison")
                          )
                    ),
                    column(12, include_md_text("fitting/fit-basic-post.md"))
            ),
            narrowDiv(
              include_md_text("fitting/fit-2.md")
            ),
            wideDiv(title = "Fitting curve: applying fitting formulas",
                    column(12, include_md_text("fitting/fit-plus-pre.md")),
                    fitSelectorInput(ns("formula_fit")),
                    box(width = 12, 
                        title = "Factors that influence the fitting curve", 
                        collapsible = TRUE,
                        dyRadioSelectorUI(
                          ns("factors_fit_plus"), 
                          c("demand", "production", "price", "grid capacity")
                          )
                    ),
                    box(width = 12, 
                        title = "Fitting curves", 
                        collapsible = TRUE,
                        dygraphs::dygraphOutput(
                          ns("fit_fitcurve"), 
                          height = Sys.getenv("DYGRAPH_HEIGHT")
                          )
                    ), 
                    box(width = 12,
                        dyRadioSelectorUI(
                          ns("graph_fit_plus"), 
                          c("original", "foreshifted", "comparison")
                          ),
                        dyCornerDiv(
                          randomizeInput(ns("fit_random_in"), label = "Random profile")
                          )
                    )
            ),
            mreDiv(
              ns("fit"), "Fitting formula and curve", "mre_fit.R"
            ),
            narrowDiv(
              include_md_text("fitting/fit-3.md")
            )
    )
  
  list(
    sidebar = sidebar, 
    content = content
  )
  
}

#'  mod_tab_home Server Function
#'
#' @noRd 
mod_tab_fitting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session
    
    o_random <- o_Xdemand$clone(deep = TRUE)
    
    # fitting (fit) basic -----------------------------------------------------
    callModule(dyRadioSelector, "factors_fit_basic", reactive(
      viz_bundle(viz_demand_fixed(o_1demand),
                 viz_production_fixed(o_1demand),
                 viz_price(o_1demand),
                 viz_cap(o_1demand),
                 names = c("demand", "production", "price", "grid capacity"))
    ))
    
    fit_basic_formula <- reactive({
      theformula <- sprintf(
        "(%s * .demand) + (-%s * .production_fixed) + (%s * .price)",
        input$fit_basic_demand,
        input$fit_basic_solar,
        input$fit_basic_price
      )
      if (input$switch_fit_basic_cap == TRUE) {
        theformula <- sprintf("ifelse(.demand < .cap, (%s), NA)", 
                              theformula)
      }
      stats::as.formula(paste("~", theformula))
    })
    
    # data
    fit_basic_raw <- o_Xdemand$clone(deep = TRUE)
    
    fit_basic_bundle <- reactive({
      fshifted <- fit_basic_raw$do_foreshift(fit = fit_basic_formula())
      cap_used <- (".cap" %in% all.vars(fit_basic_formula()))
      
      pre <- viz_fore_input(fshifted, show_cap = cap_used)
      post <- viz_fore_output(fshifted, show_cap = cap_used)
      comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
      
      bundle <- viz_bundle(
        pre, post, comp,
        ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
        names = c("original", "foreshifted", "comparison")
        )
      bundle
    })
    
    callModule(dyRadioSelector, "graph_fit_basic", reactive(fit_basic_bundle()))
    
    # fitting (fit) plus ------------------------------------------------------
    # data
    fit_plus_raw <- o_Xdemand$clone(deep = TRUE)
    
    fit_plus_bundle <- reactive({
      fshifted <- fit_plus_raw$do_foreshift(fit = formula_fit())
      cap_used <- (".cap" %in% all.vars(formula_fit()))
      
      pre <- viz_fore_input(fshifted, show_cap = cap_used)
      post <- viz_fore_output(fshifted, show_cap = cap_used)
      comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
      fitcurve <- viz_fit(fshifted)
      
      bundle <- viz_bundle(
        pre, post, comp,
        ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
        names = c("original", "foreshifted", "comparison")
        )
      bundle[["fitcurve"]] <- fitcurve
      bundle
    })
    
    fit_random_profile <- reactive({
      fit_random_out()$button
      
      o_random$set_demand(
        e_demand$new(fixed = base_demand,
                     flex = list(flex_mtx$new(data = cbind(vec_spiked(168, 2),
                                                           vec_spiked(168, 2),
                                                           vec_spiked(168, 2),
                                                           vec_spiked(168, 2),
                                                           vec_spiked(168, 2),
                                                           vec_spiked(168, 2)),
                                              steps = c(2,4,6,8,10,12),
                                              name = "obj"))
                     )
        )
      
    })
    
    fit_random_bundle <- reactive({
      
      fit_random_profile()$do_foreshift(fit = formula_fit())
      cap_used <- (".cap" %in% all.vars(formula_fit()))
      
      pre <- viz_fore_input(o_random, show_cap = cap_used)
      post <- viz_fore_output(o_random, show_cap = cap_used)
      comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
      
      viz_bundle(
        pre, post, comp,
        ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
        names = c("original", "foreshifted", "comparison"))
    })
    
    # build
    formula_fit <- callModule(fitSelector, "formula_fit")
    
    callModule(dyRadioSelector, "factors_fit_plus", reactive(
      viz_bundle(viz_demand_fixed(o_1demand),
                 viz_production_fixed(o_1demand),
                 viz_price(o_1demand),
                 viz_cap(o_1demand),
                 names = c("demand", "production", "price", "grid capacity"))
    ))
    
    output$fit_fitcurve <- dygraphs::renderDygraph({
      fit_plus_bundle()[["fitcurve"]]
    })
    
    callModule(dyRadioSelector, "graph_fit_plus",
               iftrue = reactive(fit_random_bundle()), 
               iffalse = reactive(fit_plus_bundle()),
               condition = reactive(fit_random_out()$switch))
    fit_random_out <- callModule(randomize, "fit_random_in")
    
  })
}
