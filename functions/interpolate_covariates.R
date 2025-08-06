interpolate_covariates <- function(data, raster, covariate, coords, stage_cell = "stage_cell", max_ring = 1, n.cores = 8){
  ## Get unique version of dataset
  data_uniq <- data[,which(colnames(data) %in% c(coords,stage_cell,covariate))]
  data_uniq <- data_uniq[which(!duplicated(data_uniq)),]
  ## Split stages, cells, and isolate covariate data
  stages <- as.numeric(str_split_i(data_uniq[,stage_cell],"_",1))
  cells <- as.numeric(str_split_i(data_uniq[,stage_cell],"_",2))
  covariates <- as.numeric(data_uniq[,covariate])
  ## Get index of stage cells that need addressing
  problems <- which(is.na(data_uniq[,covariate]))
  ## Create copy of covariates to serve as output vector
  output <- covariates
  ## For each problem cell
  for(i in problems){
    ## Get target cell
    cell <- cells[i]
    ## Get stage
    stage <- stages[i]
    ## Get cells in stage
    cells_in_stage <- cells[which(stages == stage)]
    covariates_in_stage <- covariates[which(stages == stage)]
    ## Get rings
    rings <- get_cell_rings(raster, cell, max_ring, n.cores)
    ## start with first layer, see if any cells present
    value <- NA
    for(r in 1:length(rings)){
      ## If any matches between cells in stage and in loop, get covariates
      if(any(which(cells_in_stage %in% rings[[r]]))){
        ring_covs <- covariates_in_stage[which(cells_in_stage %in% rings[[r]])]
        ## If any covariates are not NA, change value to mean of these
        if(any(!is.na(ring_covs))){
          value <- mean(ring_covs[!is.na(ring_covs)])
        }
      }
      ## If value has been updated, break. Otherwise, continue loop
      if(!is.na(value)) break
    }
    ## Update output vector
    output[i] <- value
  }
  ## Now map new values
  data[,covariate] <- output[match(data[,stage_cell],data_uniq[,stage_cell])]
  return(data)
}
