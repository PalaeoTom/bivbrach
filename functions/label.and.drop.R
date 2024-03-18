label.and.drop <- function(data, midpoints, taxa = T){
  if(taxa){
    ## label each taxon list
    for(l in 1:length(data)){
      names(data[[l]]) <- midpoints
    }
    ## initialise drop vector
    drop.vector <- c()
    ## for each element in data, identify NAs and then drop all
    for(i in 1:length(data)){
      for(d in 1:length(data[[i]])){
        if(all(is.na(data[[i]][[d]]))){
          drop.vector <- c(drop.vector,d)
        }
      }
    }
    ## if drop vector length exceeds 0, reduce to unique entries, then drop
    if(length(drop.vector) > 0){
      drop.vector <- unique(drop.vector)
      for(n in 1:length(data)){
        data[[n]] <- data[[n]][-drop.vector]
      }
    }
    return(data)
  } else {
    names(data) <- midpoints
    drop.vector <- c()
    for(d in 1:length(data)){
      if(all(is.na(data[[i]][[d]]))){
        drop.vector <- c(drop.vector,d)
      }
    }
    if(length(drop.vector) > 0){
      drop.vector <- unique(drop.vector)
      data <- data[-drop.vector]
    }
    return(data)
  }
}
