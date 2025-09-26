shuffle_richness_within_stages <- function(data, reps, richness, stageVar, n.cores = 1){
  stages <- unique(data[,stageVar])
  output <- mclapply(1:reps, mc.cores = n.cores, function(all){
    out <- data
    for(s in stages){
      values <- unlist(out[which(out[,stageVar]==s),richness])
      r <- sample(values,length(values),replace = F)
      out[which(out[,stageVar]==s),richness] <- r
    }
    return(out)
  })
  return(output)
}
