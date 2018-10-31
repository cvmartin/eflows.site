library(eflows)
library(eflows.viz)

source("utils.R", local = TRUE)

base_demand <- sept$d_house_smooth[1:168]*120

o_bare <- e_frame$new(sept$datetime[1:168])

###
  
o_1demand <- o_bare$clone(deep = TRUE)
o_1demand$set_demand(e_demand$new(fixed = base_demand,
                          flex = list(flex_mtx$new(data = as.matrix((rep(3,168))),
                                                   steps = c(4),
                                                   name = "flexibility"))))
o_1demand$set_production(e_production$new(fixed = list(solar = sept$solar[1:168]*120)))
o_1demand$set_price(sept$eprice[1:168], unit = "euro/mWh")

##

fitvars <- viz_bundle(viz_demand_fixed(o_1demand),
                      viz_production_fixed(o_1demand),
                      viz_price(o_1demand),
                      names = c("demand", "production", "price"))

###
ev0 <- flex_mtx$new(data = cbind(rep(0,168), rep(0,168), rep(0,168)),
                    steps = c(2, 6, 12),
                    name = "ev0")

ev1 <- flex_mtx$new(data = cbind(rep(0,168), rep(0,168), rep(0,168)),
                    steps = c(2, 6, 12),
                    name = "ev1")
ev2 <- flex_mtx$new(data = cbind(rep(0,168), rep(0,168), rep(0,168)),
                    steps = c(2, 6, 12),
                    name = "ev2")
ev3 <- flex_mtx$new(data = cbind(rep(0,168), rep(0,168), rep(0,168)),
                    steps = c(2, 6, 12),
                    name = "ev3")
ev4 <- flex_mtx$new(data = cbind(rep(0,168), rep(0,168), rep(0,168)),
                    steps = c(2, 6, 12),
                    name = "ev4")


o_1ev <- o_1demand$clone(deep = TRUE)
o_1ev$set_demand(e_demand$new(fixed = base_demand,
                           flex = list(ev0)))

o_Xev <- o_1demand$clone(deep = TRUE)
o_Xev$set_demand(e_demand$new(fixed = base_demand,
                          flex = list(ev1, ev2, ev3, ev4)))



##

o_Xdemand <- o_1demand$clone(deep = TRUE)
o_Xdemand$set_demand(e_demand$new(fixed = base_demand,
                                  flex = list(flex_mtx$new(data = cbind(rep(2, 168),
                                                                        rep(2, 168),
                                                                        rep(2, 168),
                                                                        rep(2, 168),
                                                                        rep(2, 168),
                                                                        rep(2,168)),
                                                           steps = c(2,4,6,8,10,12),
                                                           name = "obj"))))



##

# proto$demand$input$fixed[1:10]
# customfit$demand$input$fixed[1:10]

# save(list = ls(), file = "www/preprocessed_data.rda")

##

# amst <- e_frame$new(sept$datetime[1:168])
# 
# original_data <-  evdistrict[["nieuw_west"]] %>%
#   select(-datetime, -`0`) %>%
#   slice(1:168) %>%
#   as.matrix()
# 
# amst_demand_original <- e_demand$new(fixed = as.vector(sept$d_house_smooth[1:168]),
#                                      flex = list(flex_mtx$new(original_data, c(2,4,6,8,10,12), "ev")
#                                      ))
# amst$set_demand(amst_demand_original)

##