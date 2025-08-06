cov.check <- function(data, covVar, cells = "stage_cell"){
  ## Get grid cells
  GCs <- data[,cells]
  if(length(covVar)==1){
    ## get count
    covC <- which(!is.na(data[,covVar]))
    ## return grid cells index
    return(GCs[covC])
  } else {
    ## return intersect
    covC <- c()
    for(i in 1:length(covVar)){
      if(i == 1){
        covC <- which(!is.na(data[,covVar[i]]))
      } else {
        covC <- intersect(covC,which(!is.na(data[,covVar[i]])))
      }
    }
    return(GCs[covC])
  }
}
