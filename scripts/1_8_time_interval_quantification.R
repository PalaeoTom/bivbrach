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

## use fossilbrush to update Chronostratigraphy - PBDB already done
NMS <- chrono_scale(NMS,  tscale = "GTS_2020", srt = "early_interval", end = "late_interval",
                     max_ma = "max_ma", min_ma = "min_ma", verbose = FALSE)
AMNH <- chrono_scale(AMNH,  tscale = "GTS_2020", srt = "early_interval", end = "late_interval",
                    max_ma = "max_ma", min_ma = "min_ma", verbose = FALSE)
Peabody <- chrono_scale(Peabody,  tscale = "GTS_2020", srt = "early_interval", end = "late_interval",
                    max_ma = "max_ma", min_ma = "min_ma", verbose = FALSE)
GBIF <- chrono_scale(GBIF,  tscale = "GTS_2020", srt = "early_interval", end = "late_interval",
                    max_ma = "max_ma", min_ma = "min_ma", verbose = FALSE)

## Move to max_ma and min_ma, then drop columns
NMS$max_ma <- NMS$newFAD
NMS$min_ma <- NMS$newLAD
NMS$newFAD <- NULL
NMS$newLAD <- NULL

AMNH$max_ma <- AMNH$newFAD
AMNH$min_ma <- AMNH$newLAD
AMNH$newFAD <- NULL
AMNH$newLAD <- NULL

Peabody$max_ma <- Peabody$newFAD
Peabody$min_ma <- Peabody$newLAD
Peabody$newFAD <- NULL
Peabody$newLAD <- NULL

GBIF$max_ma <- GBIF$newFAD
GBIF$min_ma <- GBIF$newLAD
GBIF$newFAD <- NULL
GBIF$newLAD <- NULL

## Check for "Late Miocene" late intervals - issue with min_ma being set to 8.333 instead of 5.333
any(which(NMS$late_interval == "Late Miocene"))
any(which(AMNH$late_interval == "Late Miocene"))
any(which(Peabody$late_interval == "Late Miocene"))
any(which(GBIF$late_interval == "Late Miocene"))
## Some in GBIF
GBIF[which(GBIF$late_interval == "Late Miocene"),"min_ma"] <- 5.333
any(which(PBDB$late_interval == "Late Miocene"))

## Re-calculate midpoints
source("functions/get.midpoints.R")
NMS$midpoint <- get.midpoints(NMS[,c(21,22)])
AMNH$midpoint <- get.midpoints(AMNH[,c(40,41)])
Peabody$midpoint <- get.midpoints(Peabody[,c(33,34)])
GBIF$midpoint <- get.midpoints(GBIF[,c(38,39)])
PBDB$midpoint <- get.midpoints(PBDB[,c(9,10)])

## Export
## Run function and export
saveRDS(NMS, "data/museum/NMS_1_8_3.Rds")
saveRDS(AMNH, "data/museum/AMNH_1_8_3.Rds")
saveRDS(Peabody, "data/museum/Peabody_1_8_3.Rds")
saveRDS(PBDB, "data/PBDB//PBDB_1_8_3.Rds")
saveRDS(GBIF, "data/GBIF/GBIF_1_8_3.Rds")

