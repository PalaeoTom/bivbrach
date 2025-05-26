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

#### Rotating for palaeocoordinates ####
## Rotate
PBDB_rotated <- palaeorotate(PBDB, lng = "lng", lat = "lat", age = "midpoint", model = c("MERDITH2021", "PALEOMAP", "GOLONKA", "TorsvikCocks2017"), method = "point", uncertainty = T)
GBIF_rotated <- palaeorotate(GBIF, lng = "decimalLongitude", lat = "decimalLatitude", age = "midpoint", model = c("MERDITH2021", "PALEOMAP", "GOLONKA", "TorsvikCocks2017"), method = "point", uncertainty = T)
AMNH_rotated <- palaeorotate(AMNH, lng = "longitudeDecimal", lat = "latitudeDecimal", age = "midpoint", model = c("MERDITH2021", "PALEOMAP", "GOLONKA", "TorsvikCocks2017"), method = "point", uncertainty = T)
NMS_rotated <- palaeorotate(NMS, lng = "longitude", lat = "latitude", age = "midpoint", model = c("MERDITH2021", "PALEOMAP", "GOLONKA", "TorsvikCocks2017"), method = "point", uncertainty = T)
Peabody_rotated <- palaeorotate(Peabody, lng = "longitude", lat = "latitude", age = "midpoint", model = c("MERDITH2021", "PALEOMAP", "GOLONKA", "TorsvikCocks2017"), method = "point", uncertainty = T)

## Save raw output
saveRDS(PBDB, "data/PBDB/PBDB_1_10_1.Rds")
saveRDS(GBIF, "data/GBIF/GBIF_1_10_1.Rds")
saveRDS(AMNH, "data/museum/AMNH_1_10_1.Rds")
saveRDS(NMS, "data/museum/NMS_1_10_1.Rds")
saveRDS(Peabody, "data/museum/Peabody_1_10_1.Rds")
