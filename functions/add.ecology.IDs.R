add.ecology.IDs <- function(data, key){
  ## Create column for categories
  data[,"ecology"] <- NA
  data[,"ecological_cat"] <- NA
  ## Populate columns
  for(i in 1:nrow(key)){
    if(any((data[,"genus"] %in% key[i,"genus"]))){
      data[which(data[,"genus"] %in% key[i,"genus"]),"ecology"] <- key[i,"ecology"]
      data[which(data[,"genus"] %in% key[i,"genus"]),"ecological_cat"] <- key[i,"category"]
    }
  }
  return(data)
}
