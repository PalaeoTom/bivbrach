## 1.8 Converting interval names to numbers

## Load libraries
packages <- c("stringr", "paleobioDB")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(stringr)
library(paleobioDB)

##  Clean directory
rm(list = ls())

## Read in updated databases
PBDB <- readRDS("data/PBDB/PBDB.Rds")
NMS <- readRDS("data/museum/NMS_1_6_1.Rds")
AMNH <- readRDS("data/museum/AMNH_1_6_1.Rds")
Peabody <- readRDS("data/museum/Peabody_1_6_1.Rds")
GBIF <- readRDS("data/GBIF/GBIF_1_6_1.Rds")

## Read in macrostrat
macrostrat <- readRDS("data/metadata/macrostrat_output.Rds")

