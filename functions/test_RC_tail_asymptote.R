test_RC_tail_asymptote <- function(RCs, n = 5, threshold = 0.05){
  boolean <- apply(RCs, 2, function(x){
    curve <- x[which(!is.na(x))]
    tail <- tail(curve, n = n)
    samples <- seq(1,n,1)
    out <- lm(tail ~ samples)$coefficients[2]
    if(out <= threshold){
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
}
