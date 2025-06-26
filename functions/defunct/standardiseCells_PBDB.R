standardiseCells_PBDB <- function(data, collMinimum, refMinimum, multitonRatioMin, level, n.cores){
  ## store labels
  labels <- names(data)
  ## iterate over each time bin
  output <- mclapply(1:length(data), mc.cores = n.cores, function(x){
    if(nrow(data[[x]]) > 0){
      cells <- sort(unique(data[[x]]$cell))
      retain <- c()
      for(i in cells){
        if(length(unique(data[[x]][which(data[[x]][,"cell"] %in% i),"collection_no"])) >= collMinimum && length(unique(data[[x]][which(data[[x]][,"cell"] %in% i), "reference_no"])) >= refMinimum){
          if(level == "species"){
            count <- table(data[[x]][which(data[[x]][,"cell"] %in% i),"unique_name"])
            mult <- length(which(count > 1))
            mr <- mult/length(count)
            if(mr >= multitonRatioMin){
              retain <- c(retain, i)
            }
          } else {
            if(level == "genera"){
              count <- table(data[[x]][which(data[[x]][,"cell"] %in% i),"genus"])
              mult <- length(which(count > 1))
              mr <- mult/length(count)
              if(mr >= multitonRatioMin){
                retain <- c(retain, i)
              }
            } else {
              stop("level not species or genera")
            }
          }
        }
      }
      return(data[[x]][which(data[[x]][,"cell"] %in% retain),])
    } else {
      return(data[[x]])
    }
  })
  names(output) <- labels
  return(output)
}
