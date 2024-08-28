add.occ.covariate <- function(data, name, ref, varsLabs, var1, var2){
  ## initialise vectors
  v <- rep(NA, nrow(data))
  ## Get column number of ref
  refC <- which(colnames(data) == ref)
  ## populate vector
  v[data[[refC]] %in% var1] <- varsLabs[1]
  v[data[[refC]] %in% var2] <- varsLabs[2]
  ## Convert v to data frame
  vDF <- data.frame(v, stringsAsFactors = T)
  colnames(vDF) <- name
  out <- cbind(data, vDF)
}
