## 1.8 Converting interval names to numbers

## Load libraries
packages <- c("stringr", "fossilbrush", "divDyn")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(stringr)
library(fossilbrush)
library(divDyn)

##  Clean directory
rm(list = ls())

## Read in updated databases
PBDB <- readRDS("data/PBDB/PBDB.Rds")
NMS <- readRDS("data/museum/NMS_geocoded.Rds")
AMNH <- readRDS("data/museum/AMNH_geocoded.Rds")
Peabody <- readRDS("data/museum/Peabody_geocoded.Rds")
GBIF <- readRDS("data/GBIF/GBIF_geocoded.Rds")

## Read in macrostrat formation metadata
ms.forms <- readRDS("data/metadata/macrostrat_output.Rds")

## Load divDyn category keys
data(keys)

#### Update formations ####
## Get formations that need updating
FormKey <- ms.forms[,c(1,3)]

## Only retain unique
FormKey <- FormKey[which(!FormKey[,1] == FormKey[,2]),]

## Load function
source("functions/update.formations.R")

## Run the function
GBIF <- update.formations(GBIF, columns = c("formation1", "formation2", "formation3", "formation4"), FormKey)
AMNH <- update.formations(AMNH, columns = c("formation1", "formation2"), FormKey)
Peabody <- update.formations(Peabody, columns = c("formation1", "formation2"), FormKey)
NMS <- update.formations(NMS, columns = c("formation1", "formation2"), FormKey)
## Update PBDB NAs to gaps
PBDB[which(is.na(PBDB$formation)),"formation"] <- ""
PBDB <- update.formations(PBDB, columns = "formation", FormKey)
## Back to NA
PBDB[which(PBDB$formation==""),"formation"] <- NA

#### Condense formations into single column and add environment/lithology #####
## Filter out duplicated updated names
ELKey <- ms.forms[-which(duplicated(ms.forms$updated_name)),]

## Load function
source("functions/condense.forms.R")

## Condense formations, add environmental metadata, and categorise where possible
GBIF <- condense.forms(data = GBIF, columns = c("formation1", "formation2", "formation3", "formation4"), formations = ELKey, keys = keys)
NMS <- condense.forms(data = NMS, columns = c("formation1", "formation2"), formations = ELKey, keys = keys)
Peabody <- condense.forms(data = Peabody, columns = c("formation1", "formation2"), formations = ELKey, keys = keys)
AMNH <- condense.forms(data = AMNH, columns = c("formation1", "formation2"), formations = ELKey, keys = keys)

## Final categorisation. Where environment is black, reef should be unknown
GBIF[which(GBIF$environment == ""),"reefCat"] <- "unknown"
NMS[which(NMS$environment == ""),"reefCat"] <- "unknown"
AMNH[which(AMNH$environment == ""),"reefCat"] <- "unknown"
Peabody[which(Peabody$environment == ""),"reefCat"] <- "unknown"
PBDB[which(PBDB$environment == ""),"reefCat"] <- "unknown"

## Export
saveRDS(GBIF, "data/GBIF/GBIF_1_8_1.Rds")
saveRDS(AMNH, "data/museum/AMNH_1_8_1.Rds")
saveRDS(NMS, "data/museum/NMS_1_8_1.Rds")
saveRDS(Peabody, "data/museum/Peabody_1_8_1.Rds")
saveRDS(PBDB, "data/PBDB//PBDB_1_8_1.Rds")

#### Function to add early_interval, late_interval, max_ma, and min_ma to non-PBDB data ####
## Load data
PBDB <- readRDS("data/PBDB/PBDB_1_8_1.Rds")
NMS <- readRDS("data/museum/NMS_1_8_1.Rds")
AMNH <- readRDS("data/museum/AMNH_1_8_1.Rds")
Peabody <- readRDS("data/museum/Peabody_1_8_1.Rds")
GBIF <- readRDS("data/GBIF/GBIF_1_8_1.Rds")

## Read in macrostrat chronostratigraphy
ms.strat <- read.csv("data/metadata/macrostrat_raw.csv", header = T, row.names = 1)[,-1]

## Cut down to necessary columns
StratKey <- ms.strat[,c(2,4,5,9)]

## Load function
source("functions/quant.chrono.R")
#data <- GBIF
#key = StratKey
#column = "chronostratigraphy"
#i = 46459

quant.chrono <- function(data, key, column = "chronostratigraphy"){
  ## add relevant columns
  data$early_interval <- NA
  data$late_interval <- NA
  data$max_ma <- NA
  data$min_ma <- NA
  data$midpoint <- NA
  ## Populate new cells using chronostratigraphy
  for(i in 1:nrow(data)){
    print(i)
    c <- data[i,column]
    ## if comma, split
    if(str_detect(c, fixed(","))){
      ## split
      sc <- unlist(str_split(c, fixed(",")))
      ## isolate intervals
      min_ma <- c()
      max_ma <- c()
      midpoint <- c()
      for(int in sc){
        min_ma <- c(min_ma,key[which(key[,"name"] %in% int),"t_age"])
        max_ma <- c(max_ma,key[which(key[,"name"] %in% int),"b_age"])
        midpoint <- c(midpoint,key[which(key[,"name"] %in% int),"Midpoint"])
      }
      ## Start with late interval
      late_int <- sc[which(min_ma %in% min(min_ma))]
      late_min_ma <- min_ma[which(min_ma %in% min(min_ma))]
      ## if length is two, take shortest interval
      if(length(late_int) > 1){
        late_max_ma <- max_ma[which(min_ma %in% min(min_ma))]
        ## get duration
        dur <- late_max_ma-late_min_ma
        ## retain minimum dur
        late_int <- late_int[which(dur %in% min(dur))]
        late_min_ma <- late_min_ma[which(dur %in% min(dur))]
      }
      if(length(late_int) > 1){
        stop(paste0("Row ", i, " has two intervals with matching durations that could fit in late_interval slot: ", late_int))
      } else {
        data[i,"late_interval"] <- late_int
        data[i,"min_ma"] <- late_min_ma
      }
      ## Now do the same for the early interval
      early_int <- sc[which(max_ma %in% max(max_ma))]
      early_max_ma <- max_ma[which(max_ma %in% max(max_ma))]
      ## if length is two, take shortest interval
      if(length(early_int) > 1){
        early_min_ma <- min_ma[which(max_ma %in% max(max_ma))]
        ## get duration
        dur <- early_max_ma-early_min_ma
        ## retain minimum dur
        early_int <- early_int[which(dur %in% min(dur))]
        early_max_ma <- early_max_ma[which(dur %in% min(dur))]
      }
      if(length(early_int) > 1){
        stop(paste0("Row ", i, " has two intervals with matching durations that could fit in early_interval slot: ", early_int))
      } else {
        data[i,"early_interval"] <- early_int
        data[i,"max_ma"] <- early_max_ma
      }
      ## Then assign midpoint
      data[i,"midpoint"] <- (early_max_ma+late_min_ma)/2
    } else {
      ## Add populate early interval
      data[i,"early_interval"] <- c
      ## Find interval in ms.strat
      data[i,"min_ma"] <- key[which(key[,"name"] %in% c),"t_age"]
      data[i,"max_ma"] <- key[which(key[,"name"] %in% c),"b_age"]
      data[i,"midpoint"] <- key[which(key[,"name"] %in% c),"Midpoint"]
    }
  }
  ## delete old column
  data[,column] <- NULL
  return(data)
}

## Run function and export
# NMS
NMS <- quant.chrono(NMS, key = StratKey, column = "chronostratigraphy")
saveRDS(NMS, "data/museum/NMS_1_8_2.Rds")
# AMNH
AMNH <- quant.chrono(AMNH, key = StratKey, column = "chronostratigraphy")
saveRDS(AMNH, "data/museum/AMNH_1_8_2.Rds")
# Peabody
Peabody <- quant.chrono(Peabody, key = StratKey, column = "chronostratigraphy")
saveRDS(Peabody, "data/museum/Peabody_1_8_2.Rds")
# PBDB - just export new version
saveRDS(PBDB, "data/PBDB//PBDB_1_8_2.Rds")
# GBIF
GBIF <- quant.chrono(GBIF, key = StratKey, column = "chronostratigraphy")
saveRDS(GBIF, "data/GBIF/GBIF_1_8_2.Rds")

#### Clean up using fossilbrush ####
## Load data
PBDB <- readRDS("data/PBDB/PBDB_1_8_2.Rds")
NMS <- readRDS("data/museum/NMS_1_8_2.Rds")
AMNH <- readRDS("data/museum/AMNH_1_8_2.Rds")
Peabody <- readRDS("data/museum/Peabody_1_8_2.Rds")
GBIF <- readRDS("data/GBIF/GBIF_1_8_2.Rds")





