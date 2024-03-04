getOverlap <- function(d, r, a){
  if(d >= (2*r)){
    o <- 0
  }
  if(d == 0){
    o <- 1
  }
  if(d > 0 && d < (2*r)){
    o <- (2*(((r^2)*acos(d/(2*r)))-((d/4)*(sqrt((4*(r^2))-(d^2))))))/a
  }
  return(o)
}
