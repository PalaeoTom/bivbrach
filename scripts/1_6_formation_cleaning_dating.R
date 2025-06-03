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
key <- read_csv(file = "incomplete_formation_key_v2.csv", col_names = T, show_col_types = F)
key$change_to[is.na(key$change_to)] <- ""
key$member[is.na(key$member)] <- ""
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

## Update formations - GBIF and museum data
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
    PBDB[which(PBDB$formation %in% PBDB_key$old_formation[i]),"formation"] <- PBDB_key$change_to[i]
  }
}

## Get rows with stages in formations. Move stages to stage columns, then clean up formation
## GBIF
GBIF[which(GBIF$formation1 %in% stage.data$name),"chronostratigraphy"] <- GBIF[which(GBIF$formation1 %in% stage.data$name),"formation1"]
GBIF[which(GBIF$formation1 %in% stage.data$name),"formation1"] <- ""

GBIF[which(GBIF$formation2 %in% stage.data$name),"formation2"]
GBIF[which(GBIF$formation3 %in% stage.data$name),"formation3"]
GBIF[which(GBIF$formation4 %in% stage.data$name),"formation4"]

## AMNH
AMNH[which(AMNH$formation1 %in% stage.data$name),"chronostratigraphy"] <- AMNH[which(AMNH$formation1 %in% stage.data$name),"formation1"]
AMNH[which(AMNH$formation1 %in% stage.data$name),"formation1"] <- ""

AMNH[which(AMNH$formation2 %in% stage.data$name),"formation2"]

## NMS
NMS[which(NMS$formation1 %in% stage.data$name),"chronostratigraphy"] <- NMS[which(NMS$formation1 %in% stage.data$name),"formation1"]
NMS[which(NMS$formation1 %in% stage.data$name),"formation1"] <- ""

NMS[which(NMS$formation2 %in% stage.data$name),"formation2"]

## Peabody
Peabody[which(Peabody$formation1 %in% stage.data$name),"chronostratigraphy"] <- Peabody[which(Peabody$formation1 %in% stage.data$name),"formation1"]
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

PBDB$formation <- tidy.formations(PBDB$formation)

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
GBIF[which(GBIF$formation1 == "Ford formation"),"chronostratigraphy"] <- "Roadian,Changhsingian"

## Do same for San Carlos formation
GBIF[which(GBIF$formation1 == "San Carlos formation"),"chronostratigraphy"] <- "Coniacian,Maastrichtian"

## Do same for AMNH
AMNH[which(AMNH$formation1 == "Ford formation"),"chronostratigraphy"] <- "Roadian,Changhsingian"

## Isolate formations once again for checking
#final.forms <- c(GBIF$formation1, GBIF$formation2, GBIF$formation3, GBIF$formation4, AMNH$formation1, AMNH$formation2, NMS$formation1, NMS$formation2, Peabody$formation1, Peabody$formation2)
#final.forms <- sort(unique(final.forms))[-1]

## Run function again to get final version
#enriched_forms <- get.units.wrapper(forms = final.forms)

## Save enriched forms
#saveRDS(enriched_forms, file = "data/metadata/macrostrat_output.Rds")

## Export updated versions of databases
saveRDS(PBDB, "data/PBDB/PBDB.Rds")
saveRDS(NMS, file = "data/museum/NMS.Rds")
saveRDS(AMNH, file = "data/museum/AMNH.Rds")
saveRDS(Peabody, file = "data/museum/Peabody.Rds")
saveRDS(GBIF, file = "data/GBIF/GBIF.Rds")

#### Filling in chronostratigraphy gaps using Macrostrat ####
## Read in updated databases
PBDB <- readRDS("data/PBDB/PBDB.Rds")
NMS <- readRDS("data/museum/NMS.Rds")
AMNH <- readRDS("data/museum/AMNH.Rds")
Peabody <- readRDS("data/museum/Peabody.Rds")
GBIF <- readRDS("data/GBIF/GBIF.Rds")
macrostrat <- readRDS("data/metadata/macrostrat_output.Rds")

##### GBIF #####
## Create column to track where age data comes from
GBIF$chronostratigraphySource <- NA
GBIF[which(!GBIF$chronostratigraphy == ""),"chronostratigraphySource"] <- "record"

## Get missing entries
missing <- which(GBIF$chronostratigraphy == "")
GBIF.n.usable <- nrow(GBIF)-length(missing)
GBIF.prop.usable <- (nrow(GBIF)-length(missing))/nrow(GBIF)

## Cycle through all rows
for(i in 1:nrow(GBIF)){
  print(i)
  ## get formations
  form <- c(GBIF[i, "formation1"],GBIF[i, "formation2"],GBIF[i, "formation3"],GBIF[i, "formation4"])
  ## if all gaps, skip
  if(all(form == "")){
    next
  } else {
    ## if at least one is not blank, cut down
    if(any(form == "")){
      form <- form[-which(form == "")]
    }
    ## if more than one formation, do something
    if(length(form) > 1){
      ## initialise
      chronos.f <- c()
      for(f in form){
        ## Step 1: check within macrostrat data
        hits <- which(macrostrat$verbatim_name == f)
        ## If there is a hit, proceed
        if(length(hits)>0){
          ## get interval
          chronos <- macrostrat[hits, "interval"]
          ## if interval is NA, skip
          if(is.na(chronos)){
            next
          } else {
            ## Split if there is punctuation
            if(any(str_detect(chronos, pattern = ","))){
              chronos <- unique(unlist(str_split(chronos, pattern = ",")))
            }
            ## record
            chronos.f <- c(chronos.f, chronos)
          }
        } else {
          next
        }
      }
      if(length(chronos.f) > 0){
        ## Get unique
        chronos.f <- unique(chronos.f)
        ## If 2 or more unique elements, flatten into one.
        if(length(chronos.f) > 1){
          chronos.f <- str_flatten(chronos.f, collapse = ",")
          GBIF[i,"chronostratigraphy"] <- chronos.f
          GBIF[i,"chronostratigraphySource"] <- "Macrostrat"
        } else {
          GBIF[i,"chronostratigraphy"] <- chronos.f
          GBIF[i,"chronostratigraphySource"] <- "Macrostrat"
        }
      } else {
        next
      }
    } else {
      ## Step 1: check within macrostrat
      hits <- which(macrostrat$verbatim_name == form)
      ## If hits has at least one number, proceed
      if(length(hits)>0){
        ## get interval
        chronos <- macrostrat[hits,"interval"]
        ## if NA, skip
        if(is.na(chronos)){
          next
        } else {
          ## can only be 1 hit - just use that
          GBIF[i,"chronostratigraphy"] <- chronos
          GBIF[i,"chronostratigraphySource"] <- "Macrostrat"
        }
      } else {
        next
      }
    }
  }
}

## Get remaining missing
missing <- which(GBIF$chronostratigraphy == "")
GBIF.n.usable.pMacro <- nrow(GBIF)-length(missing)
GBIF.prop.usable.pMacro <- (nrow(GBIF)-length(missing))/nrow(GBIF)

## Drop missing
GBIF <- GBIF[-missing,]

## Remove weird stage if present
GBIF[which(GBIF$chronostratigraphy == "Tatarian"),"chronostratigraphy"] <- "Lopingian,Wordian"

## Export updated GBIF
saveRDS(GBIF, file = "data/GBIF/GBIF_1_6_1.Rds")

##### AMNH #####
## Create column to track where age data comes from
AMNH$chronostratigraphySource <- NA
AMNH[which(!AMNH$chronostratigraphy == ""),"chronostratigraphySource"] <- "record"

## Get missing entries
missing <- which(AMNH$chronostratigraphy == "")
AMNH.n.usable <- nrow(AMNH)-length(missing)
AMNH.prop.usable <- (nrow(AMNH)-length(missing))/nrow(AMNH)

## Cycle through all rows
for(i in 1:nrow(AMNH)){
  print(i)
  ## get formations
  form <- c(AMNH[i, "formation1"],AMNH[i, "formation2"])
  ## if all gaps, skip
  if(all(form == "")){
    next
  } else {
    ## if at least one is not blank, cut down
    if(any(form == "")){
      form <- form[-which(form == "")]
    }
    ## if more than one formation, do something
    if(length(form) > 1){
      ## initialise
      chronos.f <- c()
      for(f in form){
        ## Step 1: check within macrostrat data
        hits <- which(macrostrat$verbatim_name == f)
        ## If there is a hit, proceed
        if(length(hits)>0){
          ## get interval
          chronos <- macrostrat[hits, "interval"]
          ## if interval is NA, skip
          if(is.na(chronos)){
            next
          } else {
            ## Split if there is punctuation
            if(any(str_detect(chronos, pattern = ","))){
              chronos <- unique(unlist(str_split(chronos, pattern = ",")))
            }
            ## record
            chronos.f <- c(chronos.f, chronos)
          }
        } else {
          next
        }
      }
      if(length(chronos.f) > 0){
        ## Get unique
        chronos.f <- unique(chronos.f)
        ## If 2 or more unique elements, flatten into one.
        if(length(chronos.f) > 1){
          chronos.f <- str_flatten(chronos.f, collapse = ",")
          AMNH[i,"chronostratigraphy"] <- chronos.f
          AMNH[i,"chronostratigraphySource"] <- "Macrostrat"
        } else {
          AMNH[i,"chronostratigraphy"] <- chronos.f
          AMNH[i,"chronostratigraphySource"] <- "Macrostrat"
        }
      } else {
        next
      }
    } else {
      ## Step 1: check within PBDB
      hits <- which(macrostrat$verbatim_name == form)
      ## If hits has at least one number, proceed
      if(length(hits)>0){
        ## get interval
        chronos <- macrostrat[hits,"interval"]
        ## if NA, skip
        if(is.na(chronos)){
          next
        } else {
          ## can only be 1 hit - just use that
          AMNH[i,"chronostratigraphy"] <- chronos
          AMNH[i,"chronostratigraphySource"] <- "Macrostrat"
        }
      } else {
        next
      }
    }
  }
}

## Get missing entries
missing <- which(AMNH$chronostratigraphy == "")
AMNH.n.usable.pMacro <- nrow(AMNH)-length(missing)
AMNH.prop.usable.pMacro <- (nrow(AMNH)-length(missing))/nrow(AMNH)

## Drop remaining missing
AMNH <- AMNH[-missing,]

## Export updated AMNH
saveRDS(AMNH, file = "data/museum/AMNH_1_6_1.Rds")

##### Peabody #####
## Create column to track where age data comes from
Peabody$chronostratigraphySource <- NA
Peabody[which(!Peabody$chronostratigraphy == ""),"chronostratigraphySource"] <- "record"

## Get missing entries
missing <- which(Peabody$chronostratigraphy == "")
Peabody.n.usable <- nrow(Peabody)-length(missing)
Peabody.prop.usable <- (nrow(Peabody)-length(missing))/nrow(Peabody)

## Cycle through all rows
for(i in 1:nrow(Peabody)){
  print(i)
  ## get formations
  form <- c(Peabody[i, "formation1"],Peabody[i, "formation2"])
  ## if all gaps, skip
  if(all(form == "")){
    next
  } else {
    ## if at least one is not blank, cut down
    if(any(form == "")){
      form <- form[-which(form == "")]
    }
    ## if more than one formation, do something
    if(length(form) > 1){
      ## initialise
      chronos.f <- c()
      for(f in form){
        ## Step 1: check within macrostrat data
        hits <- which(macrostrat$verbatim_name == f)
        ## If there is a hit, proceed
        if(length(hits)>0){
          ## get interval
          chronos <- macrostrat[hits, "interval"]
          ## if interval is NA, skip
          if(is.na(chronos)){
            next
          } else {
            ## Split if there is punctuation
            if(any(str_detect(chronos, pattern = ","))){
              chronos <- unique(unlist(str_split(chronos, pattern = ",")))
            }
            ## record
            chronos.f <- c(chronos.f, chronos)
          }
        } else {
          next
        }
      }
      if(length(chronos.f) > 0){
        ## Get unique
        chronos.f <- unique(chronos.f)
        ## If 2 or more unique elements, flatten into one.
        if(length(chronos.f) > 1){
          chronos.f <- str_flatten(chronos.f, collapse = ",")
          Peabody[i,"chronostratigraphy"] <- chronos.f
          Peabody[i,"chronostratigraphySource"] <- "Macrostrat"
        } else {
          Peabody[i,"chronostratigraphy"] <- chronos.f
          Peabody[i,"chronostratigraphySource"] <- "Macrostrat"
        }
      } else {
        next
      }
    } else {
      ## Step 1: check within PBDB
      hits <- which(macrostrat$verbatim_name == form)
      ## If hits has at least one number, proceed
      if(length(hits)>0){
        ## get interval
        chronos <- macrostrat[hits,"interval"]
        ## if NA, skip
        if(is.na(chronos)){
          next
        } else {
          ## can only be 1 hit - just use that
          Peabody[i,"chronostratigraphy"] <- chronos
          Peabody[i,"chronostratigraphySource"] <- "Macrostrat"
        }
      } else {
        next
      }
    }
  }
}

## Get missing entries
missing <- which(Peabody$chronostratigraphy == "")
Peabody.n.usable.pMacro <- nrow(Peabody)-length(missing)
Peabody.prop.usable.pMacro <- (nrow(Peabody)-length(missing))/nrow(Peabody)

## Drop remaining missing
Peabody <- Peabody[-missing,]

## Export updated Peabody
saveRDS(Peabody, file = "data/museum/Peabody_1_6_1.Rds")

##### NMS #####
## Create column to track where age data comes from
NMS$chronostratigraphySource <- NA
NMS[which(!NMS$chronostratigraphy == ""),"chronostratigraphySource"] <- "record"

## Get missing entries
missing <- which(NMS$chronostratigraphy == "")
NMS.n.usable <- nrow(NMS)-length(missing)
NMS.prop.usable <- (nrow(NMS)-length(missing))/nrow(NMS)

## Cycle through all rows
for(i in 1:nrow(NMS)){
  print(i)
  ## get formations
  form <- c(NMS[i, "formation1"],NMS[i, "formation2"])
  ## if all gaps, skip
  if(all(form == "")){
    next
  } else {
    ## if at least one is not blank, cut down
    if(any(form == "")){
      form <- form[-which(form == "")]
    }
    ## if more than one formation, do something
    if(length(form) > 1){
      ## initialise
      chronos.f <- c()
      for(f in form){
        ## Step 1: check within macrostrat data
        hits <- which(macrostrat$verbatim_name == f)
        ## If there is a hit, proceed
        if(length(hits)>0){
          ## get interval
          chronos <- macrostrat[hits, "interval"]
          ## if interval is NA, skip
          if(is.na(chronos)){
            next
          } else {
            ## Split if there is punctuation
            if(any(str_detect(chronos, pattern = ","))){
              chronos <- unique(unlist(str_split(chronos, pattern = ",")))
            }
            ## record
            chronos.f <- c(chronos.f, chronos)
          }
        } else {
          next
        }
      }
      if(length(chronos.f) > 0){
        ## Get unique
        chronos.f <- unique(chronos.f)
        ## If 2 or more unique elements, flatten into one.
        if(length(chronos.f) > 1){
          chronos.f <- str_flatten(chronos.f, collapse = ",")
          NMS[i,"chronostratigraphy"] <- chronos.f
          NMS[i,"chronostratigraphySource"] <- "Macrostrat"
        } else {
          NMS[i,"chronostratigraphy"] <- chronos.f
          NMS[i,"chronostratigraphySource"] <- "Macrostrat"
        }
      } else {
        next
      }
    } else {
      ## Step 1: check within PBDB
      hits <- which(macrostrat$verbatim_name == form)
      ## If hits has at least one number, proceed
      if(length(hits)>0){
        ## get interval
        chronos <- macrostrat[hits,"interval"]
        ## if NA, skip
        if(is.na(chronos)){
          next
        } else {
          ## can only be 1 hit - just use that
          NMS[i,"chronostratigraphy"] <- chronos
          NMS[i,"chronostratigraphySource"] <- "Macrostrat"
        }
      } else {
        next
      }
    }
  }
}

## Get missing entries
missing <- which(NMS$chronostratigraphy == "")
NMS.n.usable.pMacro <- nrow(NMS)-length(missing)
NMS.prop.usable.pMacro <- (nrow(NMS)-length(missing))/nrow(NMS)

## Drop remaining missing entries
NMS <- NMS[-missing,]

## Export updated NMS
saveRDS(NMS, file = "data/museum/NMS_1_6_1.Rds")

#### Checking formations sampled ####
GBIF.f <- c(GBIF$formation1, GBIF$formation2, GBIF$formation3, GBIF$formation4)
GBIF.f <- unique(GBIF.f)

AMNH.f <- c(AMNH$formation1, AMNH$formation2)
AMNH.f <- unique(AMNH.f)

Peabody.f <- c(Peabody$formation1, Peabody$formation2)
Peabody.f <- unique(Peabody.f)

NMS.f <- c(NMS$formation1, NMS$formation2)
NMS.f <- unique(NMS.f)

PBDB.f <- unique(PBDB$formation)

length(which(!GBIF.f %in% PBDB.f))
length(which(!GBIF.f %in% PBDB.f))/length(GBIF.f)

length(which(!AMNH.f %in% PBDB.f))
length(which(!AMNH.f %in% PBDB.f))/length(AMNH.f)

length(which(!NMS.f %in% PBDB.f))
length(which(!NMS.f %in% PBDB.f))/length(NMS.f)

length(which(!Peabody.f %in% PBDB.f))
length(which(!Peabody.f %in% PBDB.f))/length(Peabody.f)

#### Checking occurrences sampled ####
GBIF.occs <- which(GBIF$formation1 %in% GBIF.f[(!GBIF.f %in% PBDB.f)])
GBIF.occs <- c(GBIF.occs,which(GBIF$formation2 %in% GBIF.f[(!GBIF.f %in% PBDB.f)]))
GBIF.occs <- c(GBIF.occs,which(GBIF$formation3 %in% GBIF.f[(!GBIF.f %in% PBDB.f)]))
GBIF.occs <- c(GBIF.occs,which(GBIF$formation4 %in% GBIF.f[(!GBIF.f %in% PBDB.f)]))
GBIF.occs <- unique(GBIF.occs)
length(GBIF.occs)/nrow(GBIF)

AMNH.occs <- which(AMNH$formation1 %in% AMNH.f[(!AMNH.f %in% PBDB.f)])
AMNH.occs <- c(AMNH.occs,which(AMNH$formation2 %in% AMNH.f[(!AMNH.f %in% PBDB.f)]))
AMNH.occs <- unique(AMNH.occs)
length(AMNH.occs)/nrow(AMNH)

NMS.occs <- which(NMS$formation1 %in% NMS.f[(!NMS.f %in% PBDB.f)])
NMS.occs <- c(NMS.occs,which(NMS$formation2 %in% NMS.f[(!NMS.f %in% PBDB.f)]))
NMS.occs <- unique(NMS.occs)
length(NMS.occs)/nrow(NMS)

Peabody.occs <- which(Peabody$formation1 %in% Peabody.f[(!Peabody.f %in% PBDB.f)])
Peabody.occs <- c(Peabody.occs,which(Peabody$formation2 %in% Peabody.f[(!Peabody.f %in% PBDB.f)]))
Peabody.occs <- unique(Peabody.occs)
length(Peabody.occs)/nrow(Peabody)

