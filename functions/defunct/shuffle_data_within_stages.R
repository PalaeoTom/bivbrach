shuffle_data_within_stages <- function(data, reps, variables, stageVar, n.cores = 1){
  output <- mclapply(1:reps, mc.cores = n.cores, function(all){
    out <- data
    for(s in unique(data[,stageVar])){
      for(i in variables){
        values <- out[which(out[,stageVar]==s),i]
        out[which(out[,stageVar]==s),i] <- sample(values, length(values), replace = F)
      }
    }
  })
  return(output)
}
