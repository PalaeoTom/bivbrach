stage.prune <- function(data, coords, cellMin, stageVar = "stage", stage_cell = "stage_cell"){
  ## Get unique cells
  data_uniq <- data[,c(stage_cell,coords)]
  data_uniq <- data[which(!duplicated(data_uniq)),]
  ## Trim out cells without data
  output <- data[which(data[,stage_cell] %in% stage.check(data = data_uniq, n = cellMin, stageVar = stageVar, cells = stage_cell, output = "cells")),]
  return(output)
}
