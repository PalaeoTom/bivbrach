standardiseCells <- function(data, collMinimum, refMinimum, multitonRatioMin, level){
  cells <- sort(unique(data$cell))
  retain <- c()
  for(i in cells){
    if(length(unique(data[which(data[,"cell"] %in% i),"collection_no"])) >= collMinimum && length(unique(data[which(data[,"cell"] %in% i), "reference_no"])) >= refMinimum){
      if(level == "species"){
        count <- table(data[which(data[,"cell"] %in% i),"unique_name"])
        mult <- length(which(count > 1))
        mr <- mult/length(count)
        if(mr >= multitonRatioMin){
          retain <- c(retain, i)
        }
      } else {
        if(level == "genera"){
          count <- table(data[which(data[,"cell"] %in% i),"genus"])
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
  return(data[which(data[,"cell"] %in% retain),])
}
