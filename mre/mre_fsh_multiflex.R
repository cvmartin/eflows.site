
library(eflows)
library(eflows.viz)

# calculations (eflows) --------------------------------------------------
# define an R6 object for the flexible demand
evprof <- as.matrix(sept$d_ev[1:168])

evmtx4 <- cbind(evprof, evprof, evprof, evprof)

# demand: one device with several layers of flexibility (each of them is equal)
test_demand <- 
  e_demand$new(fixed = as.vector(sept$d_house_smooth[1:168]*8),
               flex = list(flex_mtx$new(evmtx4, c(2, 4, 8, 15), "flex")
                           )
)

# define an R6 object to manipulate the demand, and see how does it look when
# displaced to the future to shave peaks of consumption (foreshift)
test_object <- 
  e_frame$new(sept$datetime[1:168])$
  set_demand(test_demand)$
  do_foreshift()

# visualization (eflows.viz) ---------------------------------------------
# generate dygraphs from the results
pre <- viz_fore_input(test_object)
post <- viz_fore_output(test_object)
comp <- viz_compare(list(pre, post), c("original", "foreshifted"))

# bundle them
multiple_flex <- 
  viz_bundle(pre, post, comp,
             ymax = max_yaxis(list_stacked = list(pre), 
                              list_unstacked = list(comp)),
             names = c("original", "foreshifted", "comparison"))

# display
vizget(multiple_flex)
