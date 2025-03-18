## 1.5 Formation cleaning, updating, and dating

## Load libraries
packages <- c("readr", "stringr", "stringi", "divDyn", "rmacrostrat")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(readr)
library(stringr)
library(stringi)
library(divDyn)
library(rmacrostrat)

##  Clean directory
rm(list = ls())

#### Preparing to manually check formations ####
## Start with PBDB formations for reference
#PBDB_cleaned <- read.csv("data/metadata/AC_cleaned_PBDB_formations.csv", row.names = 1)[,c(2,4)]

## Add PBDB label
#PBDB_cleaned$origin <- "PBDB"
#PBDB_cleaned <- PBDB_cleaned[,c(3,1,2)]

## Now load GBIF
#GBIF <- unique(unlist(readRDS("data/GBIF/GBIF.Rds")[,c(15, 16, 17, 18)]))

## Convert to data frame and add other columns
#GBIF <- data.frame(GBIF)
#GBIF <- GBIF[-which(GBIF == "")]
#colnames(GBIF) <- "old_formation"
#GBIF$change_to <- GBIF$old_formation
#GBIF$origin <- "GBIF"
#GBIF <- GBIF[,c(3,1,2)]

## NMS
#NMS <- unique(unlist(readRDS("data/museum/NMS.Rds")[,c(7, 8)]))

## Convert to data frame and add other columns
#NMS <- data.frame(NMS)
#NMS <- NMS[-which(NMS == "")]
#colnames(NMS) <- "old_formation"
#NMS$change_to <- NMS$old_formation
#NMS$origin <- "NMS"
#NMS <- NMS[,c(3,1,2)]

## AMNH
#AMNH <- unique(unlist(readRDS("data/museum/AMNH.Rds")[,c(19, 20)]))

## Convert to data frame and add other columns
#AMNH <- data.frame(AMNH)
#AMNH <- AMNH[-which(AMNH == "")]
#colnames(AMNH) <- "old_formation"
#AMNH$change_to <- AMNH$old_formation
#AMNH$origin <- "AMNH"
#AMNH <- AMNH[,c(3,1,2)]

## Peabody
#Peabody <- unique(unlist(readRDS("data/museum/Peabody.Rds")[,c(13, 14)]))

## Convert to data frame and add other columns
#Peabody <- data.frame(Peabody)
#Peabody <- Peabody[-which(Peabody == "")]
#colnames(Peabody) <- "old_formation"
#Peabody$change_to <- Peabody$old_formation
#Peabody$origin <- "Peabody"
#Peabody <- Peabody[,c(3,1,2)]

## Combine into one
#incomplete_key <- rbind(GBIF, NMS, AMNH, Peabody)

## Remove duplicates
#incomplete_key <- incomplete_key[-which(duplicated(incomplete_key[,c(2,3)])),]

## Combine with PBDB
#incomplete_key <- rbind(PBDB_cleaned, incomplete_key)

## Sort alphabetically by middle column
#incomplete_key <- incomplete_key[order(incomplete_key$old_formation),]

## Drop gaps
#incomplete_key <- incomplete_key[-which(incomplete_key$old_formation == ""),]

## Add group and member columns
#incomplete_key$group <- ""
#incomplete_key$member <- ""

## Export as csv for manual correction
#write_excel_csv(incomplete_key, file = "data/metadata/incomplete_formation_key.csv")

#### Search for specific records - use to manually check formations####
#GBIF <- readRDS("data/GBIF/GBIF.Rds")
#View(GBIF[which(GBIF$formation1 == "Yumuri Fm"),])

#AMNH <- readRDS("data/museum/AMNH.Rds")
#View(AMNH[which(AMNH$formation1 == "Zone L"),])

#NMS <- readRDS("data/museum/NMS.Rds")
#View(NMS[which(NMS$formation1 == "Woolhope Beds"),])

#Peabody <- readRDS("data/museum/Peabody.Rds")
#View(Peabody[which(Peabody$formation1 == "Wanwankou Dolomite"),])

#PBDB <- readRDS("data/PBDB/PBDB.Rds")
#View(PBDB[which(PBDB$formation == "Eureka"),])
#### Final checks for corrected formations ####
## Set home directory
home <- getwd()

## Read in key
setwd("~/Library/CloudStorage/OneDrive-Nexus365/Bivalve_brachiopod/data")
key <- read_csv(file = "incomplete_formation_key_v2.csv", col_names = T)
setwd(home)

## Split into PBDB and non PBDB
PBDB_key <- key[which(key$origin == "PBDB"),]
other_key <- key[which(!key$origin == "PBDB"),]
rm(key)

## Get stages to be used
stage.data <- read.csv("data/metadata/cleaned_stages.csv", row.names = 1, header = T)

## Read in datasets
GBIF <- readRDS("data/GBIF/GBIF.Rds")
AMNH<- readRDS("data/museum/AMNH.Rds")
NMS <- readRDS("data/museum/NMS.Rds")
Peabody <- readRDS("data/museum/Peabody.Rds")
PBDB <- readRDS("data/PBDB/PBDB.Rds")

## Update formations - GBIF
for(i in 1:nrow(other_key)){
  if(any(GBIF$formation1 %in% other_key$old_formation[i])){
    GBIF[which(GBIF$formation1 %in% other_key$old_formation[i]),"formation1"] <- other_key$change_to[i]
  }
  if(any(GBIF$formation2 %in% other_key$old_formation[i])){
    GBIF[which(GBIF$formation2 %in% other_key$old_formation[i]),"formation2"] <- other_key$change_to[i]
  }
  if(any(GBIF$formation3 %in% other_key$old_formation[i])){
    GBIF[which(GBIF$formation3 %in% other_key$old_formation[i]),"formation3"] <- other_key$change_to[i]
  }
  if(any(GBIF$formation4 %in% other_key$old_formation[i])){
    GBIF[which(GBIF$formation4 %in% other_key$old_formation[i]),"formation4"] <- other_key$change_to[i]
  }
  if(any(AMNH$formation1 %in% other_key$old_formation[i])){
    AMNH[which(AMNH$formation1 %in% other_key$old_formation[i]),"formation1"] <- other_key$change_to[i]
  }
  if(any(AMNH$formation2 %in% other_key$old_formation[i])){
    AMNH[which(AMNH$formation2 %in% other_key$old_formation[i]),"formation2"] <- other_key$change_to[i]
  }
  if(any(NMS$formation1 %in% other_key$old_formation[i])){
    NMS[which(NMS$formation1 %in% other_key$old_formation[i]),"formation1"] <- other_key$change_to[i]
  }
  if(any(NMS$formation2 %in% other_key$old_formation[i])){
    NMS[which(NMS$formation2 %in% other_key$old_formation[i]),"formation2"] <- other_key$change_to[i]
  }
  if(any(Peabody$formation1 %in% other_key$old_formation[i])){
    Peabody[which(Peabody$formation1 %in% other_key$old_formation[i]),"formation1"] <- other_key$change_to[i]
  }
  if(any(Peabody$formation2 %in% other_key$old_formation[i])){
    Peabody[which(Peabody$formation2 %in% other_key$old_formation[i]),"formation2"] <- other_key$change_to[i]
  }
}

## Update formations - PBDB
for(i in 1:nrow(PBDB_key)){
  if(any(PBDB$formation %in% PBDB_key$old_formation[i])){
    PBDB[which(PBDB$formation %in% PBDB_key$old_formation[i]),"formation1"] <- PBDB_key$change_to[i]
  }
}

## Get rows with stages in formations. Move stages to stage columns, then clean up formation
## GBIF
GBIF[which(GBIF$formation1 %in% stage.data$name),"earliestAgeOrLowestStage1"] <- GBIF[which(GBIF$formation1 %in% stage.data$name),"formation1"]
GBIF[which(GBIF$formation1 %in% stage.data$name),"formation1"] <- ""

GBIF[which(GBIF$formation2 %in% stage.data$name),"formation2"]
GBIF[which(GBIF$formation3 %in% stage.data$name),"formation3"]
GBIF[which(GBIF$formation4 %in% stage.data$name),"formation4"]

## AMNH
AMNH[which(AMNH$formation1 %in% stage.data$name),"stage1"] <- AMNH[which(AMNH$formation1 %in% stage.data$name),"formation1"]
AMNH[which(AMNH$formation1 %in% stage.data$name),"formation1"] <- ""

AMNH[which(AMNH$formation2 %in% stage.data$name),"formation2"]

## NMS - none!
NMS[which(NMS$formation1 %in% stage.data$name),"stage"] <- NMS[which(NMS$formation1 %in% stage.data$name),"formation1"]
NMS[which(NMS$formation1 %in% stage.data$name),"formation1"] <- ""

NMS[which(NMS$formation2 %in% stage.data$name),"formation2"]

## Peabody
Peabody[which(Peabody$formation1 %in% stage.data$name),"stage1"] <- Peabody[which(Peabody$formation1 %in% stage.data$name),"formation1"]
Peabody[which(Peabody$formation1 %in% stage.data$name),"formation1"] <- ""

Peabody[which(Peabody$formation2 %in% stage.data$name),"formation2"]

## Add 'formation' suffix where needed and clean up capitalization
source("functions/tidy.formations.R")
GBIF$formation1 <- tidy.formations(GBIF$formation1)
GBIF$formation2 <- tidy.formations(GBIF$formation2)
GBIF$formation3 <- tidy.formations(GBIF$formation3)
GBIF$formation4 <- tidy.formations(GBIF$formation4)

AMNH$formation1 <- tidy.formations(AMNH$formation1)
AMNH$formation2 <- tidy.formations(AMNH$formation2)

Peabody$formation1 <- tidy.formations(Peabody$formation1)
Peabody$formation2 <- tidy.formations(Peabody$formation2)

NMS$formation1 <- tidy.formations(NMS$formation1)
NMS$formation2 <- tidy.formations(NMS$formation2)

## Export updated versions of databases
saveRDS(PBDB, "data/PBDB/PBDB.Rds")
saveRDS(NMS, file = "data/museum/NMS.Rds")
saveRDS(AMNH, file = "data/museum/AMNH.Rds")
saveRDS(Peabody, file = "data/museum/Peabody.Rds")
saveRDS(GBIF, file = "data/GBIF/GBIF.Rds")

#### Using formations with Macrostrat and/or xDD ####
## Read in updated databases
PBDB <- readRDS("data/PBDB/PBDB.Rds")
NMS <- readRDS("data/museum/NMS.Rds")
AMNH <- readRDS("data/museum/AMNH.Rds")
Peabody <- readRDS("data/museum/Peabody.Rds")
GBIF <- readRDS("data/GBIF/GBIF.Rds")

## Get Macrostrat info for formation - read function
source("functions/get.units.wrapper.R")

## Isolate formations to be checked against Macrostrat - doing all for now
#final.forms <- c(GBIF$formation1, GBIF$formation2, GBIF$formation3, GBIF$formation4, AMNH$formation1, AMNH$formation2, NMS$formation1, NMS$formation2, Peabody$formation1, Peabody$formation2)
#final.forms <- sort(unique(final.forms))[-1]

## run function
#enriched_forms <- get.units.wrapper(forms = final.forms)

## Problems identified - checking all 45 manually
#probs <- enriched_forms[which(enriched_forms[,"check"] == "Unlisted addendum"),"verbatim_name"]
#problem.forms <- enriched_forms[which(enriched_forms[,"check"] == "Unlisted addendum"),"name"]

## Work through each problem formation
#i = 1
#probs[i]
#prob.unit <- get_units(strat_name = problem.forms[i])

## Check GBIF
#View(GBIF[which(GBIF$formation1 == probs[i]),])
#View(AMNH[which(AMNH$formation1 == probs[i]),])
#View(NMS[which(NMS$formation1 == probs[i]),])
#View(Peabody[which(Peabody$formation1 == probs[i]),])
#View(PBDB[which(PBDB$formation == probs[i]),])

#### Correcting issues identified by macrostrat wrapper function ####
## Load quick update functions
source("functions/quick.update.f.R")
source("functions/quick.update.l.R")
source("functions/quick.update.s.R")

#### Updating formations
## Morrow formation to group
GBIF <- quick.update.f(GBIF, "GBIF", search = "Morrow formation", replace = "Morrow group")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Morrow formation", replace = "Morrow group")
NMS <- quick.update.f(NMS, "NMS", search = "Morrow formation", replace = "Morrow group")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Morrow formation", replace = "Morrow group")
PBDB <- quick.update.f(PBDB, "PBDB", search = "Morrow formation", replace = "Morrow group")

## Longview formation to Longview dolomite
GBIF <- quick.update.f(GBIF, "GBIF", search = "Longview formation", replace = "Longview dolomite")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Longview formation", replace = "Longview dolomite")
NMS <- quick.update.f(NMS, "NMS", search = "Longview formation", replace = "Longview dolomite")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Longview formation", replace = "Longview dolomite")
PBDB <- quick.update.f(PBDB, "PBDB", search = "Longview formation", replace = "Longview dolomite")

## Attalla formation to Attalla chert conglomerate
GBIF <- quick.update.f(GBIF, "GBIF", search = "Attalla formation", replace = "Attalla chert conglomerate")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Attalla formation", replace = "Attalla chert conglomerate")
NMS <- quick.update.f(NMS, "NMS", search = "Attalla formation", replace = "Attalla chert conglomerate")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Attalla formation", replace = "Attalla chert conglomerate")
PBDB <- quick.update.f(PBDB, "PBDB", search = "Attalla formation", replace = "Attalla chert conglomerate")

## Burn formation to Burn chert conglomerate
GBIF <- quick.update.f(GBIF, "GBIF", search = "Burnett formation", replace = "Barnett formation")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Burnett formation", replace = "Barnett formation")
NMS <- quick.update.f(NMS, "NMS", search = "Burnett formation", replace = "Barnett formation")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Burnett formation", replace = "Barnett formation")
PBDB <- quick.update.f(PBDB, "PBDB", search = "Burnett formation", replace = "Barnett formation")

## Alum formation to Alum Shale
GBIF <- quick.update.f(GBIF, "GBIF", search = "Alum formation", replace = "Alum Shale")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Alum formation", replace = "Alum Shale")
NMS <- quick.update.f(NMS, "NMS", search = "Alum formation", replace = "Alum Shale")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Alum formation", replace = "Alum Shale")
PBDB <- quick.update.f(PBDB, "PBDB", search = "Alum formation", replace = "Alum Shale")

## Comanche group to Comanche Peak
GBIF <- quick.update.f(GBIF, "GBIF", search = "Comanche group", replace = "Comanche Peak")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Comanche group", replace = "Comanche Peak")
NMS <- quick.update.f(NMS, "NMS", search = "Comanche group", replace = "Comanche Peak")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Comanche group", replace = "Comanche Peak")
PBDB <- quick.update.f(PBDB, "PBDB", search = "Comanche group", replace = "Comanche Peak")

## Dewey formation to Dewey limestone
GBIF <- quick.update.f(GBIF, "GBIF", search = "Dewey formation", replace = "Dewey limestone")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Dewey formation", replace = "Dewey limestone")
NMS <- quick.update.f(NMS, "NMS", search = "Dewey formation", replace = "Dewey limestone")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Dewey formation", replace = "Dewey limestone")
PBDB <- quick.update.f(PBDB, "PBDB", search = "Dewey formation", replace = "Dewey limestone")

## Fayette formation to Jackson group
GBIF <- quick.update.f(GBIF, "GBIF", search = "Fayette formation", replace = "Jackson group")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Fayette formation", replace = "Jackson group")
NMS <- quick.update.f(NMS, "NMS", search = "Fayette formation", replace = "Jackson group")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Fayette formation", replace = "Jackson group")
PBDB <- quick.update.f(PBDB, "PBDB", search = "Fayette formation", replace = "Jackson group")

## Ford formation (USA) to Wreford limestone
GBIF <- quick.update.f(GBIF, "GBIF", search = "Ford formation", replace = "Wreford limestone", GBIF.country = "USA")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Ford formation", replace = "Wreford limestone")

## Franciscan formation to Franciscan Complex supergroup
GBIF <- quick.update.f(GBIF, "GBIF", search = "Franciscan formation", replace = "Franciscan Complex supergroup")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Franciscan formation", replace = "Franciscan Complex supergroup")
NMS <- quick.update.f(NMS, "NMS", search = "Franciscan formation", replace = "Franciscan Complex supergroup")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Franciscan formation", replace = "Franciscan Complex supergroup")
PBDB <- quick.update.f(PBDB, "PBDB", search = "Franciscan formation", replace = "Franciscan Complex supergroup")

## Hills formation to Fox Hills formation
GBIF <- quick.update.f(GBIF, "GBIF", search = "Hills formation", replace = "Fox Hills formation")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Hills formation", replace = "Fox Hills formation")
NMS <- quick.update.f(NMS, "NMS", search = "Hills formation", replace = "Fox Hills formation")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Hills formation", replace = "Fox Hills formation")
PBDB <- quick.update.f(PBDB, "PBDB", search = "Hills formation", replace = "Fox Hills formation")

## Hungerford formation to Skeels Corner Slate
GBIF <- quick.update.f(GBIF, "GBIF", search = "Hungerford formation", replace = "Skeels Corner Slate")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Hungerford formation", replace = "Skeels Corner Slate")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Hungerford formation", replace = "Skeels Corner Slate")
NMS <- quick.update.f(NMS, "NMS", search = "Hungerford formation", replace = "Skeels Corner Slate")

## Joaquin formation to San Joaquin formation
GBIF <- quick.update.f(GBIF, "GBIF", search = "Joaquin formation", replace = "San Joaquin formation")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Joaquin formation", replace = "San Joaquin formation")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Joaquin formation", replace = "San Joaquin formation")
NMS <- quick.update.f(NMS, "NMS", search = "Joaquin formation", replace = "San Joaquin formation")

## Liberty formation to Liberty Hall formation
GBIF <- quick.update.f(GBIF, "GBIF", search = "Liberty formation", replace = "Liberty Hall formation")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Liberty formation", replace = "Liberty Hall formation")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Liberty formation", replace = "Liberty Hall formation")
NMS <- quick.update.f(NMS, "NMS", search = "Liberty formation", replace = "Liberty Hall formation")

## Linden formation to San Linden Hall formation
GBIF <- quick.update.f(GBIF, "GBIF", search = "Linden formation", replace = "Linden Hall formation")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Linden formation", replace = "Linden Hall formation")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Linden formation", replace = "Linden Hall formation")
NMS <- quick.update.f(NMS, "NMS", search = "Linden formation", replace = "Linden Hall formation")

## Lowell formation to Mural limestone formation
GBIF <- quick.update.f(GBIF, "GBIF", search = "Lowell formation", replace = "Mural limestone")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Lowell formation", replace = "Mural limestone")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Lowell formation", replace = "Mural limestone")
NMS <- quick.update.f(NMS, "NMS", search = "Lowell formation", replace = "Mural limestone")

## Manuels formation to manuels river formation
GBIF <- quick.update.f(GBIF, "GBIF", search = "Manuels formation", replace = "Manuels River formation")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Manuels formation", replace = "Manuels River formation")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Manuels formation", replace = "Manuels River formation")
NMS <- quick.update.f(NMS, "NMS", search = "Manuels formation", replace = "Manuels River formation")

## McCoy to Minturn
GBIF <- quick.update.f(GBIF, "GBIF", search = "McCoy formation", replace = "Minturn formation")
AMNH <- quick.update.f(AMNH, "AMNH", search = "McCoy formation", replace = "Minturn formation")
Peabody <- quick.update.f(Peabody, "Peabody", search = "McCoy formation", replace = "Minturn formation")
NMS <- quick.update.f(NMS, "NMS", search = "McCoy formation", replace = "Minturn formation")

## Monroe to Bass Islands
GBIF <- quick.update.f(GBIF, "GBIF", search = "Monroe formation", replace = "Bass Islands formation")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Monroe formation", replace = "Bass Islands formation")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Monroe formation", replace = "Bass Islands formation")
NMS <- quick.update.f(NMS, "NMS", search = "Monroe formation", replace = "Bass Islands formation")

## Nicolet to Nicolet River
GBIF <- quick.update.f(GBIF, "GBIF", search = "Nicolet formation", replace = "Nicolet River formation")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Nicolet formation", replace = "Nicolet River formation")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Nicolet formation", replace = "Nicolet River formation")
NMS <- quick.update.f(NMS, "NMS", search = "Nicolet formation", replace = "Nicolet River formation")

## Nye to Nye River
GBIF <- quick.update.f(GBIF, "GBIF", search = "Nye formation", replace = "Nye mudstone")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Nye formation", replace = "Nye mudstone")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Nye formation", replace = "Nye mudstone")
NMS <- quick.update.f(NMS, "NMS", search = "Nye formation", replace = "Nye mudstone")

## Oak Grove to Oak Grove River
GBIF <- quick.update.f(GBIF, "GBIF", search = "Oak Grove formation", replace = "Oak Grove Sand")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Oak Grove formation", replace = "Oak Grove Sand")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Oak Grove formation", replace = "Oak Grove Sand")
NMS <- quick.update.f(NMS, "NMS", search = "Oak Grove formation", replace = "Oak Grove Sand")

## Portage formation to Chemung
GBIF <- quick.update.f(GBIF, "GBIF", search = "Portage formation", replace = "Chemung formation")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Portage formation", replace = "Chemung formation")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Portage formation", replace = "Chemung formation")
NMS <- quick.update.f(NMS, "NMS", search = "Portage formation", replace = "Chemung formation")

## Portage group to Chemung
GBIF <- quick.update.f(GBIF, "GBIF", search = "Portage group", replace = "Chemung formation")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Portage group", replace = "Chemung formation")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Portage group", replace = "Chemung formation")
NMS <- quick.update.f(NMS, "NMS", search = "Portage group", replace = "Chemung formation")

## Red Bank formation to Red Bank Sand
GBIF <- quick.update.f(GBIF, "GBIF", search = "Red Bank formation", replace = "Red Bank Sand")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Red Bank formation", replace = "Red Bank Sand")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Red Bank formation", replace = "Red Bank Sand")
NMS <- quick.update.f(NMS, "NMS", search = "Red Bank formation", replace = "Red Bank Sand")

## Sherburne formation to Sherburne Siltstone
GBIF <- quick.update.f(GBIF, "GBIF", search = "Sherburne formation", replace = "Sherburne Siltstone")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Sherburne formation", replace = "Sherburne Siltstone")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Sherburne formation", replace = "Sherburne Siltstone")
NMS <- quick.update.f(NMS, "NMS", search = "Sherburne formation", replace = "Sherburne Siltstone")

## Swan Peak formation to Swan Peak Quartzite
GBIF <- quick.update.f(GBIF, "GBIF", search = "Swan Peak formation", replace = "Swan Peak Quartzite")
AMNH <- quick.update.f(AMNH, "AMNH", search = "Swan Peak formation", replace = "Swan Peak Quartzite")
Peabody <- quick.update.f(Peabody, "Peabody", search = "Swan Peak formation", replace = "Swan Peak Quartzite")
NMS <- quick.update.f(NMS, "NMS", search = "Swan Peak formation", replace = "Swan Peak Quartzite")

#### Updating localities
## Add information to Barnett formation
GBIF <- quick.update.l(GBIF, "GBIF", unit = "Barnett formation", locality = "about 5 miles east of San Saba, Texas, USA")
AMNH <- quick.update.l(AMNH, "AMNH", unit = "Barnett formation", locality = "about 5 miles east of San Saba, Texas, USA")
NMS <- quick.update.l(NMS, "NMS", unit = "Barnett formation", locality = "about 5 miles east of San Saba, Texas, USA")
Peabody <- quick.update.l(Peabody, "Peabody",  unit = "Barnett formation", locality = "about 5 miles east of San Saba, Texas, USA")

## Add locality to Dewey limestone
GBIF <- quick.update.l(GBIF, "GBIF", unit = "Dewey limestone", locality = "Dewey, Washington County, Oklahoma, USA")
AMNH <- quick.update.l(AMNH, "AMNH", unit = "Dewey limestone", locality = "Dewey, Washington County, Oklahoma, USA")
NMS <- quick.update.l(NMS, "NMS", unit = "Dewey limestone", locality = "Dewey, Washington County, Oklahoma, USA")
Peabody <- quick.update.l(Peabody, "Peabody",  unit = "Dewey limestone", locality = "Dewey, Washington County, Oklahoma, USA")

## Add locality to "Pumpkin Creek" member of GBIF
GBIF <- quick.update.l(GBIF, name = "GBIF", unit = "Pumpkin Creek", rank = "member", locality = "Carter County, Oklahoma, USA")

#### Updating stages
## Add stage to "Elkhorn formation"
GBIF <- quick.update.s(GBIF, "GBIF", search = "Elkhorn formation", stage = "Richmondian")
AMNH <- quick.update.s(AMNH, "AMNH", search = "Elkhorn formation", stage = "Richmondian")
NMS <- quick.update.s(NMS, "NMS", search = "Elkhorn formation", stage = "Richmondian")
Peabody <- quick.update.s(Peabody, "Peabody",  search = "Elkhorn formation", stage = "Richmondian")

## Add stage to "Mill formation"
GBIF <- quick.update.s(GBIF, "GBIF", search = "Mill formation", stage = "Katian")
AMNH <- quick.update.s(AMNH, "AMNH", search = "Mill formation", stage = "Katian")
NMS <- quick.update.s(NMS, "NMS", search = "Mill formation", stage = "Katian")
Peabody <- quick.update.s(Peabody, "Peabody",  search = "Mill formation", stage = "Katian")

## Custom approach to add bracket stages for Ford formation
GBIF[which(GBIF$formation1 == "Ford formation"),"earliestAgeOrLowestStage1"] <- "Roadian"
GBIF[which(GBIF$formation1 == "Ford formation"),"latestAgeOrHighestStage1"] <- "Changhsingian"

## Do same for San Carlos formation
GBIF[which(GBIF$formation1 == "San Carlos formation"),"earliestAgeOrLowestStage1"] <- "Coniacian"
GBIF[which(GBIF$formation1 == "San Carlos formation"),"latestAgeOrHighestStage1"] <- "Maastrichtian"

## Do same for AMNH
AMNH[which(AMNH$formation1 == "Ford formation"),"stage1"] <- "Roadian"
AMNH[which(AMNH$formation1 == "Ford formation"),"stage2"] <- "Changhsingian"

## Isolate formations once again for checking
final.forms <- c(GBIF$formation1, GBIF$formation2, GBIF$formation3, GBIF$formation4, AMNH$formation1, AMNH$formation2, NMS$formation1, NMS$formation2, Peabody$formation1, Peabody$formation2)
final.forms <- sort(unique(final.forms))[-1]

## Run function again to get final version - remaining errors can be ignored
enriched_forms <- get.units.wrapper(forms = final.forms)









#### Additional data to send to Shanan if necessary ####
## Isolate stages#
# Get stages to be used
stage.data <- read.csv("data/metadata/cleaned_stages.csv", row.names = 1, header = T)
stages <- stage.data$name

## Isolate palaeoenvironmental data
data(keys)
environments <- unique(unlist(c(keys[[3]],keys[[4]],keys[[6]])))

## Export as text files
#setwd("~/Desktop/bivbrach_strings")
#writeLines(final.forms,"formations.txt")
#writeLines(stages,"stages.txt")
#writeLines(environments,"descriptors.txt")

