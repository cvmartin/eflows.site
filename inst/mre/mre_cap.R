
# load packages. Install with devtools
# devtools::install_github('cvmartin/eflows')
# devtools::install_github('cvmartin/eflows.viz')

library(eflows)
library(eflows.viz)

# calculations (eflows) ---------------------------------------------------

# this example uses data from eflows::sept

# define an R6 object for the flexible demand
evprof <- as.matrix(sept$d_ev[1:168])
test_demand <- e_demand$new(fixed = as.vector(sept$d_house_smooth[1:168]*8),
                         flex = list(flex_mtx$new(evprof, c(2), "aBitFlexible", cap = 0.4),
                                     flex_mtx$new(evprof, c(4), "flexible"),
                                     flex_mtx$new(evprof, c(8), "quiteFlexible", cap = 0.5),
                                     flex_mtx$new(evprof, c(15), "veryFlexible")
                                     )
)

# define an R6 object to manipulate the demand, and see how does it look when
# displaced to the future to shave peaks of consumption (foreshift)
test_object <- e_frame$new(sept$datetime[1:168])$
  set_demand(test_demand)$
  set_cap((sin(seq.int(1,168)/10*3)/8+3))$
  do_foreshift()

# visualization (eflows.viz) ----------------------------------------------

# generate dygraphs from the results
pre <- viz_fore_input(test_object)
post <- viz_fore_output(test_object)
comp <- viz_compare(list(pre, post), c("original", "foreshifted"))

# ... and bundle them. Now it is a list of dygraphs
test_bundled <- viz_bundle(pre, post, comp,
                      ymax = max_yaxis(list_stacked = list(pre), list_unstacked = list(comp)),
                      names = c("original", "foreshifted", "comparison"))

# Than, even easier, can be visualized with a shiny gadget
vizget(test_bundled)
# - Original: the initial data, with the fixed demand (blue) and layers of flexible demand
# - foreshifted: what if the flexible demand could be optimized to reduce peaks of consumption
# - comparison: the two previous graphs, side by side

