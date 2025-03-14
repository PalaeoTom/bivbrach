quick.update.s <- function(data, name, search, stage){
  if(name == "GBIF"){
    data[which(data[,"formation1"] == search),"earliestAgeOrLowestStage1"] <- stage
    data[which(data[,"formation2"] == search),"earliestAgeOrLowestStage1"] <- stage
    data[which(data[,"formation3"] == search),"earliestAgeOrLowestStage1"] <- stage
    data[which(data[,"formation4"] == search),"earliestAgeOrLowestStage1"] <- stage
  }
  if(name == "AMNH"){
    data[which(data[,"formation1"] == search),"stage1"] <- stage
    data[which(data[,"formation2"] == search),"stage1"] <- stage
  }
  if(name == "NMS"){
    data[which(data[,"formation1"] == search),"stage"] <- stage
    data[which(data[,"formation2"] == search),"stage"] <- stage
  }
  if(name == "Peabody"){
    data[which(data[,"formation1"] == search),"stage1"] <- stage
    data[which(data[,"formation2"] == search),"stage1"] <- stage
  }
  return(data)
}
