refine.references <- function(data, level, quantiles, drop.ref.singletons.first = T){
  if(drop.ref.singletons.first){
    ## Get references
    refs <- unique(data[,"reference_no"])
    ## Get richness of each reference
    count <- c()
    for(i in refs){
      count <- c(count, length(unique(data[which(data[,"reference_no"] %in% i),level])))
    }
    ## Drop singleton references
    data <- data[-which(data[,"reference_no"] %in% refs[which(count == 1)]),]
  }
  ## Get references
  refs <- unique(data[,"reference_no"])
  count <- c()
  for(i in refs){
    count <- c(count, length(unique(data[which(data[,"reference_no"] %in% i),level])))
  }
  ## Get cutoffs
  cutoffs <- quantile(count, quantiles)
  ## Drop references at or above/below threshold
  below <- refs[which(count <= cutoffs[1])]
  above <- refs[which(count >= cutoffs[2])]
  droppers <- c(below, above)
  ## Drop references above/below threshold
  data <- data[-which(data[,"reference_no"] %in% droppers),]
}
