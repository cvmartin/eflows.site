start_date <- function(date_range) {
  return(Sys.Date() - (switch(date_range, daily = 2, weekly = 14, monthly = 60, quarterly = 90) + 1))
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

do_ev_prof <- function(matrix, inputs = c(), pos, cap) {
  l <- nrow(matrix)
  ind_cap <- (inputs/sum(inputs)) * cap
  
  for (i in 1:length(inputs)) {
    matrix[,i] <- peak_in_zeroes(l, pos, inputs[i], ind_cap[i])
  }
  matrix
}


# profile generators ------------------------------------------------------
vec_const <-function(l, v = 1){rep(v,l)}

vec_normal <- function(l, v = 1, sd = v/2){rnorm(l, v, sd)}

vec_spiked <- function(l, v = 1, k = round(l/10)) {
  x <- cumsum(sample(c(-1, 1), l, TRUE))
  y <- caTools::runquantile(x, k, probs = 0.3)
  (ifelse(x-y >= 0, x-y, 0)) * v
}

vec_peak <- function(l, v = 1, loc) {
  x <- rep(0, l)
  x[loc] <- v
  x
}

# for the random cap
ondulation <- function(l, v = 1, k = round(l/10)) {
  x <- cumsum(sample(c(-1, 1), l, TRUE))
  y <- caTools::runquantile(x, k, probs = 0.3)
  (scale(y)/10)+1
}

# aesthetics --------------------------------------------------------------
gg_palette <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

