get.cell.covariate <- function(data, cell, unknown, ref, value){
  ## Get cell IDs
  cells <- data[,cell]
  ## Get unique cells
  sc <- unique(cells)
  ## Get covariates
  covs <- data[,ref]
  ## create container
  container <- rep(NA, length(cells))
  ## for each cell
  for(b in sc){
    ## Get index of occurrences in this spacetime sample
    occ_ind <- which(cells==b)
    ## get covariate values
    occ_covs <- covs[occ_ind]
    ## Get number of occurrences with assignments
    n <- length(occ_covs)-length(which(occ_covs==unknown))
    ## If at least 1 occurrence has a record,
    if(n>0){
      ## Assign proportion as value
      container[occ_ind] <- length(which(occ_covs == value))/n
    } else {
      ## If 0, set as NA. Can change later
      container[occ_ind] <- NA
    }
  }
  ## Add to data as column
  return(container)
}
