wrap.countUsable <- function(VC, threshold, eg, var.names){
  UTBs <- countUsable(VC, threshold)
  out <- cbind(eg, UTBs)
  rownames(out) <- rownames(VC)
  colnames(out) <- c(var.names, "usableTimeBins")
  return(out)
}
