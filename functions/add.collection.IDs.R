add.collection.IDs <- function(data){
  ## Create column for comp labels
  data[,"collection_comp"] <- NA
  ## Get IDs
  coll.id <- unique(data$collection_no)
  ## Populate column
  for(i in coll.id){
    comp <- unique(data[which(data$collection_no %in% i),"phylum"])
    if(length(comp) > 1 && any(comp == "Brachiopoda") && any(comp == "Mollusca")){
      data[which(data$collection_no %in% i),"collection_comp"] <- "both"
    } else {
      if(comp == "Brachiopoda"){
        data[which(data$collection_no %in% i),"collection_comp"] <- "brach"
      } else {
        if(comp == "Mollusca"){
          data[which(data$collection_no %in% i),"collection_comp"] <- "biv"
        } else {
          stop("something's not right here...phylum not Mollusca and/or Brachiopoda")
        }
      }
    }
  }
  return(data)
}
