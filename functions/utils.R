start_date <- function(date_range) {
  return(Sys.Date() - (switch(date_range, daily = 2, weekly = 14, monthly = 60, quarterly = 90) + 1))
}

do_fore_bundle <- function(obj, ...){
  obj$do_foreshift(...)
  
  pre <- viz_fore_input(obj)
  post <- viz_fore_output(obj)
  comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
  
  m2 <- max_yaxis(list_stacked = list(pre), list_unstacked = list(comp))
  
  viz_bundle(pre, post, comp,
             ymax = m2,
             names = c("original", "foreshifted", "comparison"), 
             group = "initialgroup")
}

do_fore_extended <- function(obj){
  obj$do_foreshift()
  
  pre <- viz_fore_input(obj)
  post <- viz_fore_output(obj)
  post_ev <- viz_fore_output(obj, aggregate = "object")
  post_flex <- viz_fore_output(obj, aggregate = "flex")
  comp <- viz_compare(list(pre, post), c("original", "foreshifted"))
  unstacked <- viz_fore_output(obj, aggregate = "object", show_fixed = FALSE, stacked = FALSE)
  
  m2 <- max_yaxis(list_stacked = list(pre), list_unstacked = list(comp))
  
  viz_bundle(pre, post, post_ev,post_flex, comp, unstacked,
             ymax = m2,
             names = c("original", "foreshifted","aggregated by ev", "aggregated by flex", "comparison", "unstacked"))
}

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
peak_in_zeroes(25, 23, 10, 3)

do_ev_prof <- function(matrix, inputs = c(), pos, cap) {
  l <- nrow(matrix)
  ind_cap <- (inputs/sum(inputs)) * cap
  
  for (i in 1:length(inputs)) {
    matrix[,i] <- peak_in_zeroes(l, pos, inputs[i], ind_cap[i])
  }
  matrix
}

# generators --------------------------------------------------------------



