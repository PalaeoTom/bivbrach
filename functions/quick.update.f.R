quick.update.f <- function(data, name, search, replace, GBIF.country = NULL){
  if(is.null(GBIF.country)){
    if(name == "GBIF"){
      data[which(data[,"formation1"] == search),"formation1"] <- replace
      data[which(data[,"formation2"] == search),"formation2"] <- replace
      data[which(data[,"formation3"] == search),"formation3"] <- replace
      data[which(data[,"formation4"] == search),"formation4"] <- replace
    }
    if(name == "AMNH"){
      data[which(data[,"formation1"] == search),"formation1"] <- replace
      data[which(data[,"formation2"] == search),"formation2"] <- replace
    }
    if(name == "NMS"){
      data[which(data[,"formation1"] == search),"formation1"] <- replace
      data[which(data[,"formation2"] == search),"formation2"] <- replace
    }
    if(name == "Peabody"){
      data[which(data[,"formation1"] == search),"formation1"] <- replace
      data[which(data[,"formation2"] == search),"formation2"] <- replace
    }
    if(name == "PBDB"){
      data[which(data[,"formation"] == search),"formation"] <- replace
    }
  } else {
    if(name == "GBIF"){
      intersection1 <- intersect(which(data[,"formation1"] == search),which(data[,"ISO3"] == GBIF.country))
      if(length(intersection1)>0){
        data[intersection1,"formation1"] <- replace
      }
      intersection2 <- intersect(which(data[,"formation2"] == search),which(data[,"ISO3"] == GBIF.country))
      if(length(intersection2)>0){
        data[intersection2,"formation2"] <- replace
      }
      intersection3 <- intersect(which(data[,"formation3"] == search),which(data[,"ISO3"] == GBIF.country))
      if(length(intersection3)>0){
        data[intersection3,"formation3"] <- replace
      }
      intersection4 <- intersect(which(data[,"formation4"] == search),which(data[,"ISO3"] == GBIF.country))
      if(length(intersection4)>0){
        data[intersection4,"formation4"] <- replace
      }
    }
  }
  return(data)
}
