## 1.5 Formation and genera cleaning

## Load libraries
packages <- c("readr", "stringr", "stringi", "divDyn")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(readr)
library(stringr)
library(stringi)
library(divDyn)

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

## Isolate formations to be sent. Drop first entry as it is ""
final.forms <- c(GBIF$formation1, GBIF$formation2, GBIF$formation3, GBIF$formation4, AMNH$formation1, AMNH$formation2, NMS$formation1, NMS$formation2, Peabody$formation1, Peabody$formation2)
final.forms <- sort(unique(final.forms))[-1]

## Isolate stages
stages <- stage.data$name

## Isolate palaeoenvironmental data
data(keys)
environments <- unique(unlist(c(keys[[3]],keys[[4]],keys[[6]])))

## Export as text files
#setwd("~/Desktop/bivbrach_strings")
#writeLines(final.forms,"formations.txt")
#writeLines(stages,"stages.txt")
#writeLines(environments,"descriptors.txt")

## Export updated versions of databases
saveRDS(PBDB, "data/PBDB/PBDB.Rds")
saveRDS(NMS, file = "data/museum/NMS.Rds")
saveRDS(AMNH, file = "data/museum/AMNH.Rds")
saveRDS(Peabody, file = "data/museum/Peabody.Rds")
saveRDS(GBIF, file = "data/GBIF/GBIF.Rds")

