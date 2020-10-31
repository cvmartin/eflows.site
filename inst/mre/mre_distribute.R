
library(eflows)

# flow larger than the available storage capacity
distribute(flow = 100, soc = 40, vol = 80)

# parameter `eff`: efficiency of the storage
distribute(flow = 100, soc = 40, vol = 80, eff = 0.85)

# parameter `cap`: capacity of the storage to assimilate power
distribute(flow = 100, soc = 40, vol = 80, cap = 30)

# using `eff` and `cap` simultaneously
distribute(flow = 100, soc = 40, vol = 80, eff = 0.85, cap = 30)

# using distribute() in a vectorized way
distribute(flow = 90, 
           soc = c(40, 50, 30), 
           vol = c(80, 90, 70))

# parameter `share`: proportion of `flow` that shall go to each storage
distribute(flow = 90, 
           soc = c(40, 50, 30), 
           vol = c(80, 90, 70), 
           share = c(0.4, 0.25, 0.35))

# What happens when one of `share` is zero
distribute(flow = 90, 
           soc = c(40, 50, 30), 
           vol = c(80, 90, 70), 
           share = c(0, 0.25, 0.35))

# parameter `level`: resolve first the flows to some storages
eflows::distribute(flow = 60, 
                   soc = c(40, 50, 30), 
                   vol = c(80, 90, 70), 
                   level = c(3, 2, 1))

# parameter `active`: define if the storage participates
eflows::distribute(flow = 100, 
                   soc = c(40, 50, 30), 
                   vol = c(80, 90, 70), 
                   share = c(0.8, 0.1, 0.1),
                   level = c(3, 2, 1),
                   active = c(FALSE, TRUE, TRUE))

# total discharge
distribute(flow = -100, 
           soc = c(40, 50, 30), 
           vol = c(0, 0, 0))

# partial discharge
distribute(flow = -100, 
           soc = c(40, 50, 30), 
           vol = c(10, 40, 30))