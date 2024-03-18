get.midpoints <- function(df){
  out <- apply(df, 1, function(x) (x[1]+x[2])/2)
  names(out) <- NULL
  return(out)
}
