
library(eflows)
library(eflows.viz)

# calculations (eflows) ---------------------------------------------------
# define an R6 object and populate it with data
# note the use of `$` to chain methods

test_object <- e_frame$new(sept$datetime[1:168])$
  set_demand(e_demand$new(fixed = as.vector(sept$d_house_smooth[1:168]*120),
                           flex = list(
                             flex_mtx$new(data = cbind(rep(2, 168),
                                                       rep(2, 168),
                                                       rep(2, 168),
                                                       rep(2, 168),
                                                       rep(2, 168),
                                                       rep(2,168)),
                                          steps = c(2,4,6,8,10,12),
                                          name = "flex"))
                          ))$
  set_production(e_production$new(fixed = list(solar = sept$solar[1:168]*120)))$
  set_price(sept$eprice[1:168]*0.6, unit = "euro/mWh")$
  set_cap((sin(seq.int(1,168)/10*3) + 55))

# define the fitting formula. Note that it is unquoted, and includes `~`
fitting_formula = ~ ifelse(.demand < .cap, .price, NA)

# foreshift using the forumula. Edit it to obtain different results
test_object$do_foreshift(fit = fitting_formula)

# visualization (eflows.viz) ---------------------------------------------
# generate dygraphs from the results
pre <- viz_fore_input(test_object)
post <- viz_fore_output(test_object)
comp <- viz_compare(list(pre, post), c("original", "foreshifted"))

# bundle them
fitting_formula_example <- 
  viz_bundle(pre, post, comp,
             ymax = max_yaxis(list_stacked = list(pre), 
                              list_unstacked = list(comp)),
             names = c("original", "foreshifted", "comparison"))

# additional graphs can be added to the bundle
# for instance, the graph for the fitting curves.
fitting_formula_example[["fitcurve"]] <- viz_fit(test_object)

# display
vizget(fitting_formula_example)

