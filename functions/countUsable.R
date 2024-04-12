countUsable <- function(VC, threshold){
  out <- matrix(0, nrow = nrow(VC), ncol = ncol(VC))
  for(r in 1:nrow(VC)){
    for(c in 1:ncol(VC)){
      if(VC[r,c]>=threshold){
        out[r,c] <- 1
      } else {
        out[r,c] <- 0
      }
    }
  }
  rowSums(out)
}
