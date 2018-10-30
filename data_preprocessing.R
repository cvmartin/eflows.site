library(eflows)
library(eflows.viz)

load("www/evdistrict.rda")
source("utils.R", local = TRUE)


obj1 <- flex_mtx$new(data = as.matrix((rep(3,168))),
                     steps = c(4),
                     name = "flexibility")

p_demand <- e_demand$new(fixed = sept$d_house_smooth[1:168]*10,
                         flex = list(obj1))

proto <- e_frame$new(sept$datetime[1:168])
proto$set_demand(p_demand)

##

amst <- e_frame$new(sept$datetime[1:168])

original_data <-  evdistrict[["nieuw_west"]] %>%
  select(-datetime, -`0`) %>%
  slice(1:168) %>%
  as.matrix()

amst_demand_original <- e_demand$new(fixed = as.vector(sept$d_house_smooth[1:168]),
                                     flex = list(flex_mtx$new(original_data, c(2,4,6,8,10,12), "ev")
                                     ))
amst$set_demand(amst_demand_original)

##

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


evs_demand <- e_demand$new(fixed = sept$d_house_smooth[1:168]*10,
                           flex = list(ev1, ev2, ev3, ev4))

evs <- e_frame$new(sept$datetime[1:168])
evs$set_demand(evs_demand)

##
objfit <- flex_mtx$new(data = cbind(rep(2, 168),
                                    rep(2, 168),
                                    rep(2, 168),
                                    rep(2, 168),
                                    rep(2, 168),
                                    rep(2,168)),
                       steps = c(2,4,6,8,10,12),
                       name = "obj")

customfit <- e_frame$new(sept$datetime[1:168])
customfit$set_demand(e_demand$new(fixed = sept$d_house_smooth[1:168]*120,
                                  flex = list(objfit)))
customfit$set_production(e_production$new(fixed = list(solar = sept$solar[1:168]*120)))
customfit$set_price(sept$eprice[1:168], unit = "euro/mWh")

fitvars <- viz_bundle(viz_demand_fixed(customfit), 
                      viz_production_fixed(customfit),
                      viz_price(customfit), 
                      names = c("demand", "production", "price"))

##

save(list = ls(), file = "www/preprocessed_data.rda")

