stage.check <- function(data, n = 2, stageVar = "stage", cells = "stage_cell", output = "stages"){
  ## Get count of cells per stage
  stgC <- table(data[,stageVar])
  ## Get stages equal to or above n
  stgKeep <- names(stgC)[which(stgC >= n)]
  ## Get grid cells to keep
  cellsKeep <- unique(data[which(data[,stageVar] %in% stgKeep),cells])
  ## return how many have n
  if(output == "stages"){
    return(stgKeep)
  } else {
    if(output == "cells"){
      return(cellsKeep)
    }
  }
}
