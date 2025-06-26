add.reference.IDs <- function(data){
  ## Create column for comp labels
  data[,"reference_comp"] <- NA
  ## Get IDs
  coll.id <- unique(data$reference_no)
  ## Populate column
  for(i in coll.id){
    comp <- unique(data[which(data$reference_no %in% i),"phylum"])
    if(length(comp) > 1 && any(comp == "Brachiopoda") && any(comp == "Mollusca")){
      data[which(data$reference_no %in% i),"reference_comp"] <- "both"
    } else {
      if(comp == "Brachiopoda"){
        data[which(data$reference_no %in% i),"reference_comp"] <- "brach"
      } else {
        if(comp == "Mollusca"){
          data[which(data$reference_no %in% i),"reference_comp"] <- "biv"
        } else {
          stop("something's not right here...phylum not Mollusca and/or Brachiopoda")
        }
      }
    }
  }
  return(data)
}
