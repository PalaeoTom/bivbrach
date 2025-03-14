quick.update.l <- function(data, name, unit, rank = "formation", locality){
  if(rank == "formation"){
  if(name == "GBIF"){
    data[which(data[,"formation1"] == unit),"locality"] <- locality
    data[which(data[,"formation2"] == unit),"locality"] <- locality
    data[which(data[,"formation3"] == unit),"locality"] <- locality
    data[which(data[,"formation4"] == unit),"locality"] <- locality
  }
  if(name == "AMNH"){
    data[which(data[,"formation1"] == unit),"locality"] <- locality
    data[which(data[,"formation2"] == unit),"locality"] <- locality
  }
  if(name == "NMS"){
    data[which(data[,"formation1"] == unit),"locality"] <- locality
    data[which(data[,"formation2"] == unit),"locality"] <- locality
  }
  if(name == "Peabody"){
    data[which(data[,"formation1"] == unit),"locality"] <- locality
    data[which(data[,"formation2"] == unit),"locality"] <- locality
  }
  }
  if(rank == "member"){
    if(name == "GBIF"){
      data[which(data[,"member"] == unit),"locality"] <- locality
    }
    if(name == "AMNH"){
      data[which(data[,"member"] == unit),"locality"] <- locality
    }
    if(name == "NMS"){
      stop("NMS doesn't have members")
    }
    if(name == "Peabody"){
      data[which(data[,"member"] == unit),"locality"] <- locality
    }
  }
  return(data)
}
