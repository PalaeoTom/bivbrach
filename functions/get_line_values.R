get_line_values <- function(slope, x, y){
  ## get yintercept
  yintercept <- y - (slope*x)
  ## Get y values for range 0:x
  out <- c()
  for(i in seq(1,x,1)){
    out <- c(out, ((slope*i)+yintercept))
  }
  return(out)
}
