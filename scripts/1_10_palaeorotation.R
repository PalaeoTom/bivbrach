## 1.10 Rotating for paleocoordinates

## Load libraries
packages <- c("palaeoverse")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(palaeoverse)

##  Clean directory
rm(list = ls())

## Read in updated databases
PBDB <- readRDS("data/PBDB/PBDB_1_9.Rds")
NMS <- readRDS("data/museum/NMS_1_9.Rds")
AMNH <- readRDS("data/museum/AMNH_1_9.Rds")
Peabody <- readRDS("data/museum/Peabody_1_9.Rds")
GBIF <- readRDS("data/GBIF/GBIF_1_9.Rds")

## Converting coordinates and midpoints to numeric
GBIF$decimalLatitude <- as.numeric(GBIF$decimalLatitude)
GBIF$decimalLongitude <- as.numeric(GBIF$decimalLongitude)
GBIF$midpoint <- as.numeric(GBIF$midpoint)

AMNH$latitudeDecimal <- as.numeric(AMNH$latitudeDecimal)
AMNH$longitudeDecimal <- as.numeric(AMNH$longitudeDecimal)
AMNH$midpoint <- as.numeric(AMNH$midpoint)

NMS$latitude <- as.numeric(NMS$latitude)
NMS$longitude <- as.numeric(NMS$longitude)
NMS$midpoint <- as.numeric(NMS$midpoint)

Peabody$latitude <- as.numeric(Peabody$latitude)
Peabody$longitude <- as.numeric(Peabody$longitude)
Peabody$midpoint <- as.numeric(Peabody$midpoint)

#### Rotating for palaeocoordinates ####
## Rotate
PBDB_rotated <- palaeorotate(PBDB, lng = "lng", lat = "lat", age = "midpoint", model = c("MERDITH2021", "PALEOMAP", "GOLONKA", "TorsvikCocks2017"), method = "point", uncertainty = T)
GBIF_rotated <- palaeorotate(GBIF, lng = "decimalLongitude", lat = "decimalLatitude", age = "midpoint", model = c("MERDITH2021", "PALEOMAP", "GOLONKA", "TorsvikCocks2017"), method = "point", uncertainty = T)
AMNH_rotated <- palaeorotate(AMNH, lng = "longitudeDecimal", lat = "latitudeDecimal", age = "midpoint", model = c("MERDITH2021", "PALEOMAP", "GOLONKA", "TorsvikCocks2017"), method = "point", uncertainty = T)
NMS_rotated <- palaeorotate(NMS, lng = "longitude", lat = "latitude", age = "midpoint", model = c("MERDITH2021", "PALEOMAP", "GOLONKA", "TorsvikCocks2017"), method = "point", uncertainty = T)
Peabody_rotated <- palaeorotate(Peabody, lng = "longitude", lat = "latitude", age = "midpoint", model = c("MERDITH2021", "PALEOMAP", "GOLONKA", "TorsvikCocks2017"), method = "point", uncertainty = T)

## Save raw output
saveRDS(PBDB_rotated, "data/PBDB/PBDB_1_10_1.Rds")
saveRDS(GBIF_rotated, "data/GBIF/GBIF_1_10_1.Rds")
saveRDS(AMNH_rotated, "data/museum/AMNH_1_10_1.Rds")
saveRDS(NMS_rotated, "data/museum/NMS_1_10_1.Rds")
saveRDS(Peabody_rotated, "data/museum/Peabody_1_10_1.Rds")

#### Get average coordinates for main analysis #####
## Read in updated databases
PBDB <- readRDS("data/PBDB/PBDB_1_10_1.Rds")
NMS <- readRDS("data/museum/NMS_1_10_1.Rds")
AMNH <- readRDS("data/museum/AMNH_1_10_1.Rds")
Peabody <- readRDS("data/museum/Peabody_1_10_1.Rds")
GBIF <- readRDS("data/GBIF/GBIF_1_10_1.Rds")

## Drop PBDB paleo
PBDB[,"paleolat"] <- NULL
PBDB[,"paleolng"] <- NULL

## Get average coordinates
lats <- c("p_lat_MERDITH2021", "p_lat_PALEOMAP", "p_lat_GOLONKA", "p_lat_TorsvikCocks2017")
longs <- c("p_lng_MERDITH2021", "p_lng_PALEOMAP", "p_lng_GOLONKA", "p_lng_TorsvikCocks2017")

## Read in function
source("functions/average.Coords.R")

## Get average coordinates
PBDB <- average.Coords(PBDB, lats, longs)
GBIF <- average.Coords(GBIF, lats, longs)
NMS <- average.Coords(NMS, lats, longs)
AMNH <- average.Coords(AMNH, lats, longs)
Peabody <- average.Coords(Peabody, lats, longs)

## Drop duplicates from intervals
for(i in 1:nrow(GBIF)){
  if(!is.na(GBIF[i,"late_interval"])){
  if(GBIF[i,"early_interval"]==GBIF[i,"late_interval"]){
    GBIF[i,"late_interval"] <- NA
  }
  }
}

for(i in 1:nrow(AMNH)){
  if(!is.na(AMNH[i,"late_interval"])){
    if(AMNH[i,"early_interval"]==AMNH[i,"late_interval"]){
      AMNH[i,"late_interval"] <- NA
    }
  }
}

for(i in 1:nrow(NMS)){
  if(!is.na(NMS[i,"late_interval"])){
    if(NMS[i,"early_interval"]==NMS[i,"late_interval"]){
      NMS[i,"late_interval"] <- NA
    }
  }
}

for(i in 1:nrow(AMNH)){
  if(!is.na(AMNH[i,"late_interval"])){
    if(AMNH[i,"early_interval"]==AMNH[i,"late_interval"]){
      AMNH[i,"late_interval"] <- NA
    }
  }
}


## Export
saveRDS(PBDB, "data/PBDB/PBDB_1_10_2.Rds")
saveRDS(GBIF, "data/GBIF/GBIF_1_10_2.Rds")
saveRDS(AMNH, "data/museum/AMNH_1_10_2.Rds")
saveRDS(NMS, "data/museum/NMS_1_10_2.Rds")
saveRDS(Peabody, "data/museum/Peabody_1_10_2.Rds")

#### Creating master dataset ####
## Read in updated databases
PBDB <- readRDS("data/PBDB/PBDB_1_10_2.Rds")
NMS <- readRDS("data/museum/NMS_1_10_2.Rds")
AMNH <- readRDS("data/museum/AMNH_1_10_2.Rds")
Peabody <- readRDS("data/museum/Peabody_1_10_2.Rds")
GBIF <- readRDS("data/GBIF/GBIF_1_10_2.Rds")

## Rearrange and export final versions
#View(data.frame(colnames(PBDB)))
PBDB_index <- c(1,2,3,12,13,14,15,16,17,6,4,7,5,29,8,9,10,11,28,19,18,40,41,38,39,31,30,33,32,35,34,37,36,20,21,22,23,24,25,26,27)
#length(sort(unique(PBDB_index)))
PBDB <- PBDB[,PBDB_index]
saveRDS(PBDB, "data/PBDB/PBDB_final.Rds")

#View(data.frame(colnames(GBIF)))
GBIF_index <- c(1,25,24,26,2,3,4,5,6,7,8,41,36,37,38,39,40,27,22,23,12,13,14,29,15,16,17,18,19,20,21,28,52,53,50,51,43,42,45,44,47,46,49,48,9,30,10,11,31,32,33,34,35)
#length(sort(unique(GBIF_index)))
GBIF <- GBIF[,GBIF_index]
saveRDS(GBIF, "data/GBIF/GBIF_final.Rds")

#View(data.frame(colnames(AMNH)))
AMNH_index <- c(1,2,27,3,4,5,6,7,8,9,10,43,38,39,40,41,42,28,11,12,13,14,15,19,20,29,31,21,22,23,24,25,26,30,54,55,52,53,45,44,47,46,49,48,51,50,16,17,32,18,33,34,35,36,37)
#length(sort(unique(AMNH_index)))
AMNH <- AMNH[,AMNH_index]
saveRDS(AMNH, "data/museum/AMNH_final.Rds")

#View(data.frame(colnames(NMS)))
NMS_index <- c(1,2,3,4,24,19,20,21,22,23,8,5,6,10,11,12,7,9,35,36,33,34,26,25,28,27,30,29,32,31,13,14,15,16,17,18)
#length(sort(unique(NMS_index)))
NMS <- NMS[,NMS_index]
saveRDS(NMS, "data/museum/NMS_final.Rds")

#View(data.frame(colnames(Peabody)))
Peabody_index <- c(1,19,20,18,2,3,4,5,6,7,8,36,31,32,33,34,35,21,9,10,11,14,15,22,24,16,17,23,47,48,45,46,38,37,40,39,42,41,44,43,25,12,13,26:30)
#length(sort(unique(Peabody_index)))
Peabody <- Peabody[,Peabody_index]
saveRDS(Peabody, "data/museum/Peabody_final.Rds")

## Now add missing columns
## Add source columns
PBDB$source <- "PBDB"
GBIF$source <- "GBIF"
NMS$source <- "NMS"
AMNH$source <- "AMNH"
Peabody$source <- "Peabody"

## Add paleocoord_average_uncertainty column to all
PBDB$paleocoord_average_uncertainty <- PBDB$max_dist/2
GBIF$paleocoord_average_uncertainty <- GBIF$max_dist/2
NMS$paleocoord_average_uncertainty <- NMS$max_dist/2
AMNH$paleocoord_average_uncertainty <- AMNH$max_dist/2
Peabody$paleocoord_average_uncertainty <- Peabody$max_dist/2

## PBDB: add species, chronostratigraphy_source, coordinate_uncertainty, coordinate_source, and locality
#View(data.frame(colnames(PBDB)))
## species
PBDB$species <- str_split_i(PBDB$accepted_name, pattern = fixed(" "), i = 2)
PBDB[which(is.na(PBDB$species)),"species"] <- "sp."

# chronostratigraphy source
PBDB$chronostratigraphy_source <- "record"

# coordinate_uncertainty
PBDB$coordinate_uncertainty <- 0

# coordinate_source
PBDB$coordinate_source <- "record"

# locality
PBDB$locality <- ""

## NMS: add coll_cat_number, reference, class, order, family, coordinate_source
# coll_cat_number
NMS$coll_cat_number <- ""

# reference
NMS$reference <- ""

# class
NMS$class <- ""

# order
NMS$order <- ""

# family
NMS$family <- ""

# coordinate_source
NMS$coordinate_source <- "GoogleMapAPI"

## Combine into master matrix
#View(data.frame(colnames(PBDB)))
PBDB_master <- PBDB[,c(42,1,2,4,5,6,7,8,9,44,14,15,16,17,18,19,45,20,21,46,47,48,22,23,43,26:34,37:41)]

#View(data.frame(colnames(GBIF)))
GBIF_master <- GBIF[,c(54,1,2,3,6,7,8,9,10,11,12,13,14,15,16,17,18,21,22,23,24,32,33,34,55,37:44,46,49:53)]

#View(data.frame(colnames(AMNH)))
AMNH_master <- AMNH[,c(56,1,2,3,4,5,6,7,8,10,12,13,14,15,16,17,18,24,25,26,27,34,35,36,57,39:46,49,51:55)]

#View(data.frame(colnames(Peabody)))
Peabody_master <- Peabody[,c(49,1,2,4,5,7,8,9,10,11,12,13,14,15,16,17,18,22,23,24,25,28,29,30,50,33:41,44:48)]

NMS_master <- NMS[,c(37,1,39,40,2,41,42,43,3,4,5,6:11,14:16,44,18,19,20,38,23:36)]

## Update column names
columns <- c("source",
             "occurrence_ID",
             "coll_cat_number",
             "reference",
             "phylum",
             "class",
             "order",
             "family",
             "genus",
             "species",
             "combined_name",
             "early_interval",
             "late_interval",
             "max_ma",
             "min_ma",
             "midpoint_ma",
             "chronostratigraphy_source",
             "latitude",
             "longitude",
             "coordinate_uncertainty",
             "coordinate_source",
             "locality",
             "paleolat_average",
             "paleolong_average",
             "paleocoord_average_uncertainty",
             "paleolat_MERDITH2021",
             "paleolong_MERDITH2021",
             "paleolat_PALEOMAP",
             "paleolong_PALEOMAP",
             "paleolat_GOLONKA",
             "paleolong_GOLONKA",
             "paleolat_TorsvikCocks2017",
             "paleolong_TorsvikCocks2017",
             "formation",
             "lithology",
             "environment",
             "lith_category",
             "bath_category",
             "reef_category")
colnames(PBDB_master) <- colnames(GBIF_master) <- colnames(AMNH_master) <- colnames(NMS_master) <- colnames(Peabody_master) <- columns

## Create master dataset
master <- rbind(PBDB_master, GBIF_master, AMNH_master, NMS_master, Peabody_master)

## Export
saveRDS(master, file="data/final/master.Rds")
