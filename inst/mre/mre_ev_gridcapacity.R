# This example is out of the original purpose of eflows
# It doesn't use e_frames, because the main function at work is distribute()
# The code is much less terse, and instead of eflows.viz, 
# the graphs are built from the ground up

library(eflows)
library(dplyr)
library(dygraphs)

# calculations --------------------------------------------------
# input relevant variables. Note how each parameter (except grid capacity)
# is a vector, and each element of the vector is that parameter for an EV
grid_capacity <- 100
input_evsoc <- c(40, 40, 15, 5, 30)
input_evvol <- c(75, 50, 30, 50, 40)
input_evcap <- c(20, 12, 10, 12, 20)
input_evlevel <- c(1, 3, 0, 2, 2)
input_eveff <- c(0.9, 0.9, 0.9, 0.9, 0.9)
  
# define two lists: for soc (state of charge) and flow
s <- list(0)
f <- list(0)

s[[1]] <- input_evsoc
i <- 1

defcap <- c()
defcap[i] <- 1

# ugly imperative core, that would be faster in C++
while (TRUE) {
  i <- i + 1
  
  defcap[i] <- defcap[i - 1]
  
  current_flow <- (grid_capacity/60) * defcap[i]
  if (current_flow < 5/60) {
    defcap[i] <- defcap[i] + 0.2
    current_flow <- (input$cap_evs_pwr/60) * defcap[i]
  }
  
  temp <- eflows::distribute(flow = current_flow,
                             soc = s[[i - 1]], 
                             vol = input_evvol, 
                             cap = input_evcap, 
                             eff = input_eveff,
                             level = input_evlevel
  )
  s[[i]] <- temp[[1]]
  f[[i]] <- temp[[2]]*60
  
  # if the last result is the same, terminate
  if (identical(s[[i]], s[[i - 1]])) break
}

# state of charge
soc <- do.call(rbind, s)
completed <- apply(soc, 2, function(x){match(max(x), x)})
for (i in 1:ncol(soc)) {
  soc[(completed[i] + 1):nrow(soc),i] <- NA
}
s2 <- soc %>% 
  as.data.frame() %>% 
  mutate(minutes = seq(1:nrow(.))) %>% 
  select(minutes, everything()) 
colnames(s2) <- c("minutes", "EV 1", "EV 2", "EV 3", "EV 4", "EV 5")

# flow
flow <- do.call(rbind, f) 
f2 <- flow %>% 
  as.data.frame() %>% 
  mutate(minutes = seq(1:nrow(.))) %>% 
  select(minutes, everything())
colnames(f2) <- c("minutes", "EV 1", "EV 2", "EV 3", 
                  "EV 4", "EV 5")

results <- list(s2, f2, completed, defcap)


# visualization -----------------------------------------------------------

# Beware: using the internals of a package is not good practice
palette_pwr <- eflows.viz:::gg_palette(5)

# graph: SOC
  soc_graph <- dygraph(results[[1]]) %>% 
    dyHighlight(highlightSeriesBackgroundAlpha = 0.6,
                highlightSeriesOpts = list(strokeWidth = 2)) %>% 
    dyLegend(show = "onmouseover", width = 50) %>% 
    dyCSS(system.file("css/dygraph_style.css", package = "eflows.viz")) %>% 
    dyOptions(fillGraph = TRUE, 
              colors = palette_pwr, 
              mobileDisableYTouch = TRUE,
              retainDateWindow = TRUE) %>% 
    dyAxis("x", label = "minutes of charge") %>% 
    dyAxis("y", "kWh")
  for (i in 1:length(results[[3]])) {
    soc_graph <-  dyEvent(soc_graph,
                        x = results[[3]][i], 
                        label = paste0("EV", i, ": ", results[[3]][i], " min."),
                        color = palette_pwr[i])
  }
  soc_graph 

# graph: power flow
  flow_graph <- dygraph(results[[2]]) %>% 
    dyHighlight(highlightSeriesBackgroundAlpha = 0.6,
                highlightSeriesOpts = list(strokeWidth = 2)) %>%
    dyOptions(
      stackedGraph = TRUE,
      colors = palette_pwr, 
      mobileDisableYTouch = TRUE,
      retainDateWindow = TRUE)%>% 
    dyAxis("x", label = "minutes of charge") %>% 
    dyAxis("y", "kW") %>% 
    dyLegend(show = "onmouseover") %>% 
    dyCSS(system.file("css/dygraph_style.css", package = "eflows.viz")) %>% 
    eflows.viz:::add_cap(results[[4]]*grid_capacity)
  
  flow_graph
