get.midpoints <- function(df){
  out <- apply(df, 1, function(x) (x[-length(x)] + x[-1L])/2)
}
