get.bins <- function(max.t, min.t, bin.s){
  ## set bins
  bins <- seq(max.t, min.t, -bin.s)
  ## convert to list of vectors
  bins <- lapply(2:length(bins), function(x) out <- c(bins[x-1],bins[x]))
}
