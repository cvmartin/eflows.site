library(eflows)
library(eflows.viz)

proto <- e_frame$new(sept$datetime[1:168])

# proto <- primal$clone(deep = TRUE)

proto$set_demand(e_demand$new(fixed = sept$d_house_smooth[1:168]*10,
                              flex = list(flex_mtx$new(data = as.matrix((rep(3,168))),
                                                       steps = c(4),
                                                       name = "flexibility"))))


# customfit <- e_frame$new(sept$datetime[1:168])
# CLONE DEEP: THIS IS THE KEY
# APPARENTLY, ONE HAS TO COPY THE OTHER. TWO initializaions of the fixed variable overlap
customfit <- proto$clone(deep = TRUE)
customfit$set_demand(e_demand$new(fixed = sept$d_house_smooth[1:168]*120,
                                  flex = list(flex_mtx$new(data = cbind(rep(2, 168),
                                                                        # rep(2, 168),
                                                                        # rep(2, 168),
                                                                        # rep(2, 168),
                                                                        # rep(2, 168),
                                                                        rep(2,168)),
                                                           steps = c(2,4),
                                                           name = "object"))))
customfit$set_production(e_production$new(fixed = list(solar = sept$solar[1:168]*120)))
customfit$set_price(sept$eprice[1:168], unit = "euro/mWh")


proto$demand$input$fixed[1:10]
customfit$demand$input$fixed[1:10]