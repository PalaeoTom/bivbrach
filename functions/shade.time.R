shade.time <- function(y.top, y.bottom, strat.data, cols = c("grey95", "grey97")){
  cc <- rep(cols,(nrow(strat.data)))
  rect(xleft = strat.data[,"b_age"], ybottom = rep(y.bottom, nrow(strat.data)), xright = strat.data[,"t_age"],
       ytop = rep(y.top, nrow(strat.data)), col = cc, border=NA)
}
