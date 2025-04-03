quick.update.s <- function(data, name, search, stage){
  if(name == "GBIF"){
    data[which(data[,"formation1"] == search),"chronostratigraphy"] <- stage
    data[which(data[,"formation2"] == search),"chronostratigraphy"] <- stage
    data[which(data[,"formation3"] == search),"chronostratigraphy"] <- stage
    data[which(data[,"formation4"] == search),"chronostratigraphy"] <- stage
  }
  if(name == "AMNH"){
    data[which(data[,"formation1"] == search),"chronostratigraphy"] <- stage
    data[which(data[,"formation2"] == search),"chronostratigraphy"] <- stage
  }
  if(name == "NMS"){
    data[which(data[,"formation1"] == search),"chronostratigraphy"] <- stage
    data[which(data[,"formation2"] == search),"chronostratigraphy"] <- stage
  }
  if(name == "Peabody"){
    data[which(data[,"formation1"] == search),"chronostratigraphy"] <- stage
    data[which(data[,"formation2"] == search),"chronostratigraphy"] <- stage
  }
  return(data)
}
