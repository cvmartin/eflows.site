library(eflows)

base_demand <- (sept$d_house_smooth[1:168]*120) - 5 # a bit shorter seems to be better

o_bare <- e_frame$new(sept$datetime[1:168])

###

o_1demand <- o_bare$clone(deep = TRUE)
o_1demand$set_demand(e_demand$new(fixed = base_demand,
                                  flex = list(flex_mtx$new(data = as.matrix((rep(3,168))),
                                                           steps = c(4),
                                                           name = "flexibility"))))
o_1demand$set_production(e_production$new(fixed = list(solar = sept$solar[1:168]*120)))
o_1demand$set_price(sept$eprice[1:168]*0.6, unit = "euro/mWh")
o_1demand$set_cap((sin(seq.int(1,168)/10*3) + 55))

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




usethis::use_data(
  base_demand, o_bare, o_1demand, o_1ev, o_Xev, o_Xdemand, 
  ev0, ev1, ev2, ev3, ev4,
  internal = TRUE, overwrite = TRUE
  )
