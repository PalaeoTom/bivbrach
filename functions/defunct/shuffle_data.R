shuffle_data <- function(data, reps, variables, n.cores = 1){
  output <- mclapply(1:reps, mc.cores = n.cores, function(all){
    out <- data
    for(i in variables){
      out[,i] <- out[sample(1:nrow(data), nrow(data), replace = F),i]
    }
    return(out)
  })
  return(output)
}
