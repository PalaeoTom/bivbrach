isolate.coeffs <- function(models){
  ## Get terms
  terms <- models[[1]][,"term"]
  ## get column names
  cn <- c(paste0(terms, "_coefficients"), paste0(terms, "_pValues"))
  ## create output matrix
  output <- data.frame(matrix(data = NA, nrow = length(models), ncol = length(cn)))
  colnames(output) <- cn
  ## populate
  for(i in 1:length(models)){
    output[i,1:length(terms)] <- models[[i]][,"estimate"]
    output[i,(length(terms)+1):(length(terms)*2)] <- models[[i]][,"p.value"]
  }
  return(output)
}
