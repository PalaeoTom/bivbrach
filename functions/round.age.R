round.age <- function(vect, digits, round_up=TRUE){
  vect <- (10^digits)*vect
  ifelse(round_up==TRUE, vect <- ceiling(vect), vect <- floor(vect))
  return( vect / (10^digits) )
}
