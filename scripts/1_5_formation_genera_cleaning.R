## 1.5 Non-PBDB formation and genera cleaning

## Load libraries
packages <- c("")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}

## Clean directory
rm(list = ls())

#### Load formations ####
## Start with PBDB formations for reference
PBDB_cleaned <- read.csv("data/metadata/AC_cleaned_PBDB_formations.csv", row.names = 1)[,c(2,4)]

## Add PBDB label
PBDB_cleaned$origin <- "PBDB"
PBDB_cleaned <- PBDB_cleaned[,c(3,1,2)]

## Now load GBIF
GBIF <- unique(unlist(readRDS("data/GBIF/GBIF.Rds")[,c(15, 16, 17, 18)]))

## Convert to data frame and add other columns
GBIF <- data.frame(GBIF)
GBIF <- GBIF[-which(GBIF == "")]
colnames(GBIF) <- "old_formation"
GBIF$change_to <- GBIF$old_formation
GBIF$origin <- "GBIF"
GBIF <- GBIF[,c(3,1,2)]

## NMS
NMS <- unique(unlist(readRDS("data/museum/NMS.Rds")[,c(7, 8)]))

## Convert to data frame and add other columns
NMS <- data.frame(NMS)
NMS <- NMS[-which(NMS == "")]
colnames(NMS) <- "old_formation"
NMS$change_to <- NMS$old_formation
NMS$origin <- "NMS"
NMS <- NMS[,c(3,1,2)]

## AMNH
AMNH <- unique(unlist(readRDS("data/museum/AMNH.Rds")[,c(19, 20)]))

## Convert to data frame and add other columns
AMNH <- data.frame(AMNH)
AMNH <- AMNH[-which(AMNH == "")]
colnames(AMNH) <- "old_formation"
AMNH$change_to <- AMNH$old_formation
AMNH$origin <- "AMNH"
AMNH <- AMNH[,c(3,1,2)]

## Peabody
Peabody <- unique(unlist(readRDS("data/museum/Peabody.Rds")[,c(13, 14)]))

## Convert to data frame and add other columns
Peabody <- data.frame(Peabody)
Peabody <- Peabody[-which(Peabody == "")]
colnames(Peabody) <- "old_formation"
Peabody$change_to <- Peabody$old_formation
Peabody$origin <- "Peabody"
Peabody <- Peabody[,c(3,1,2)]

## Combine into one
incomplete_key <- rbind(PBDB_cleaned, GBIF, NMS, AMNH, Peabody)

## Sort alphabetically by middle column
test <- incomplete_key[order(incomplete_key$old_formation),]

