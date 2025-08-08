extract_cell_metaData <- function(data, target, cell = "stage_cell", label = F){
  extract <- data[which(!duplicated(data[,cell])),target]
  cells <- data[which(!duplicated(data[,cell])),cell]
  if(label){
    names(extract) <- cells
  }
  return(extract)
}
