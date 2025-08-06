cov.prune <- function(data, stage_cell, coords, covariates){
  ## Get unique cells
  data_uniq <- data[,c(stage_cell,coords)]
  data_uniq <- data[which(!duplicated(data_uniq)),]
  ## Trim out cells without data
  output <- data[which(data[,stage_cell] %in% cov.check(data = data_uniq, covVar = covariates)),]
  return(output)
}
