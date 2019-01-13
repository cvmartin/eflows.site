
library(eflows)
library(eflows.viz)

# introduce the parameters for 2 EVs
ev1input <- list(flex2 = 10, flex6 = 10, flex12 = 10,
                 pos = 50, cap = 12)
ev2input <- list(flex2 = 15, flex6 = 5, flex12 = 15,
                 pos = 100, cap = 15)

# support functions -------------------------------------------------------

peak_in_zeroes <- function(l, pos, vol, cap = 0){
  v <- rep(0,l)
  
  if (cap == 0){
    v[pos] <- vol
    return(v)
  }
  
  full <- vol %/% cap
  left <- vol %% cap
  
  if (full == 0) {
    v[pos] <- left
  } else {
    v[pos: (pos + full-1)] <- cap
    v[pos + full] <- left
  }
  
  if (length(v) > l) v <- v[1:l]
  v
}

do_ev_prof <- function(matrix, inputs = c(), pos, cap) {
  l <- nrow(matrix)
  ind_cap <- (inputs/sum(inputs)) * cap
  
  for (i in 1:length(inputs)) {
    matrix[,i] <- peak_in_zeroes(l, pos, inputs[i], ind_cap[i])
  }
  matrix
}

# calculations (eflows) ---------------------------------------------------
# define an R6 object and populate it with data

test_object <- e_frame$new(sept$datetime[1:168])$
  set_production(e_production$new(fixed = list(solar = sept$solar[1:168]*120)))$
  set_price(sept$eprice[1:168]*0.6, unit = "euro/mWh")$
  set_cap((sin(seq.int(1,168)/10*3) + 55))

# set the empty demand of both EVs with custom flexible matrices
ev1 <- flex_mtx$new(data = cbind(rep(0,168), rep(0,168), rep(0,168)),
                    steps = c(2, 6, 12),
                    name = "ev1")
ev2 <- flex_mtx$new(data = cbind(rep(0,168), rep(0,168), rep(0,168)),
                    steps = c(2, 6, 12),
                    name = "ev2") 
 
test_object$set_demand(
  e_demand$new(fixed = as.vector(sept$d_house_smooth[1:168]*20),
                                    flex = list(ev1, ev2))
  )

# modify the demand of the two EVs using cap, and elaborate the original graph
ev1$data <- do_ev_prof(ev1$data, 
                       inputs = c(ev1input$flex2, 
                                  ev1input$flex6, 
                                  ev1input$flex12), 
                       pos = ev1input$pos,
                       cap = ev1input$cap)
ev1$cap <- ev1input$cap

ev2$data <- do_ev_prof(ev2$data, 
                       inputs = c(ev2input$flex2, 
                                  ev2input$flex6, 
                                  ev2input$flex12), 
                       pos = ev2input$pos,
                       cap = ev2input$cap)
ev2$cap <- ev2input$cap

pre <- viz_fore_input(test_object)

# modify again the demand, this time without cap 
# in order to do the foreshifted graph
ev1$data <- do_ev_prof(ev1$data, 
                       inputs = c(ev1input$flex2, 
                                  ev1input$flex6,
                                  ev1input$flex12), 
                       pos = ev1input$pos,
                       cap = 0)
ev2$data <- do_ev_prof(ev2$data, 
                       inputs = c(ev2input$flex2, 
                                  ev2input$flex6, 
                                  ev2input$flex12), 
                       pos = ev2input$pos,
                       cap = 0)

test_object$do_foreshift()

# visualization (eflows.viz) ---------------------------------------------
# elaborate multiple graphs, and bundle them together
post <- viz_fore_output(test_object)
post_ev <- viz_fore_output(test_object, aggregate = "object")
post_flex <- viz_fore_output(test_object, aggregate = "flex")
comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
unstacked <- viz_fore_output(test_object, aggregate = "object", 
                             show_fixed = FALSE, stacked = FALSE, show_cap = FALSE)

ev <- viz_bundle(pre, post, post_ev,post_flex, comp, unstacked,
                     ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
                     names = c("original", "foreshifted","aggregated by ev", 
                               "aggregated by flex", "comparison", "unstacked"))

# additional graphs can be added to the bundle
ev[["fitcurve"]] <- viz_fit(test_object)

# display
vizget(ev)


