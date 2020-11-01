#' mod_tab_foreshift UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_tab_foreshift_ui <- function(id){
  ns <- NS(id)
  
  sidebar <- menuSubItem("foreshift", tabName = "foreshift")
  
  content <- 
    tabItem("foreshift", 
            narrowDiv(
              include_md_text("fsh/fsh-1.md")
            ),
            wideDiv(title = "Flexibility layer",
                    column(12, include_md_text("fsh/fsh-basic-pre.md")),
                    inputDiv(
                      column(width = 6,
                             sliderInput(ns("vol"), label = "Volume of flexible demand", min = 0,
                                         max = 30, value = 10, step = 1, ticks = FALSE, post = " kWh")
                      ),
                      column(width = 6, 
                             sliderInput(ns("hflex"), label = "Flexibility", min = 1,
                                         max = 12, value = 4, step = 1, ticks = FALSE, post = " hours")
                      )
                    ),
                    box(width = 12,
                        dyRadioSelectorUI(ns("graph_fsh_basic"), c("original", "foreshifted", "comparison"))), 
                    column(12, include_md_text("fsh/fsh-basic-post.md"))
            ),
            mreDiv(
              ns("fsh_oneflex"), "Foreshift: one flexibility", "mre_fsh_oneflex.R"
            ),
            narrowDiv(
              include_md_text("fsh/fsh-2.md")
            ),
            mreDiv(
              ns("fsh_multiflex"), "Foreshift: one object, multiple flexibility", "mre_fsh_multiflex.R"
            ),
            wideDiv(title = "Layers of flexibility",
                    box(width = 12,
                        column(12, include_md_text("fsh/fsh-plus-pre.md")),
                        dyRadioSelectorUI(ns("graph_fsh_plus"), c("original", "foreshifted", "comparison", "unstacked by flex")),
                        dyCornerDiv(randomizeInput(ns("fsh_random_in"), label = "Random profile")),
                        column(12, include_md_text("fsh/fsh-plus-post.md"))
                    )
            ),
            mreDiv(
              ns("fsh_multiobject"), "Foreshift: multiple objects, multiple flexibility", "mre_fsh_multiobject.R"
            ),
            narrowDiv(
              include_md_text("fsh/fsh-3.md")
            )
    )
  
  list(
    sidebar = sidebar, 
    content = content
  )
}
    
#'  mod_tab_foreshift Server Function
#'
#' @noRd 
mod_tab_foreshift_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session
    
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
      unstacked <- viz_fore_output(o_layered, aggregate = "flex", 
                                   show_fixed = FALSE, stacked = FALSE, show_cap = FALSE)
      
      
      viz_bundle(pre, post, comp, unstacked,
                 ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
                 names = c("original", "foreshifted", "comparison", "unstacked by flex"))
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
      unstacked <- viz_fore_output(o_random, aggregate = "flex", 
                                   show_fixed = FALSE, stacked = FALSE, show_cap = FALSE)
      
      
      viz_bundle(pre, post, comp, unstacked,
                 ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
                 names = c("original", "foreshifted", "comparison", "unstacked by flex"))
    })
    
    # build
    callModule(dyRadioSelector, "graph_fsh_plus",
               iftrue = reactive(fsh_random_bundle()), 
               iffalse = reactive(fsh_plus_bundle()),
               condition = reactive(fsh_random_out()$switch))
    fsh_random_out <- callModule(randomize, "fsh_random_in")
    
    })
}
    