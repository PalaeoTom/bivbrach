add.geo.scale <- function(y.bottom, scale.bottom, time.data, strat.data, drop.L.label = F, drop.R.label = F){
  rect(xleft = strat.data[, "b_age"], ybottom = rep(y.bottom, nrow(strat.data)), xright = strat.data[, "t_age"],
       ytop = rep(scale.bottom, nrow(strat.data)), border = "black", col = strat.data[, "color"])
  bt <- (y.bottom+scale.bottom)/2
  samp.time <- c(max(time.data),strat.data[which(strat.data[,"b_age"] < max(time.data) & strat.data[,"b_age"] > min(time.data)),"b_age"], min(time.data))
  tpl <- samp.time+c(diff(samp.time)/2,0)
  lab.no <- c(which(strat.data[,"b_age"] > max(time.data))[length(which(strat.data[,"b_age"] > max(time.data)))],
              which(strat.data[,"b_age"] < max(time.data) & strat.data[,"b_age"] > min(time.data)))
  tpl <- tpl[-length(samp.time)]
  if(drop.L.label){
    tpl <- tpl[-1]
    lab.no <- lab.no[-1]
  }
  if(drop.R.label){
    tpl <- tpl[-length(tpl)]
    lab.no <- lab.no[-length(lab.no)]
  }
  text(x=tpl, y=bt, labels = strat.data[lab.no, "abbrev"], cex = 1)
}
