## 1.9 Generic name cleaning

## Load libraries
packages <- c("stringr", "fossilbrush")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(stringr)
library(fossilbrush)

##  Clean directory
rm(list = ls())

## Read in updated databases
PBDB <- readRDS("data/PBDB/PBDB_1_8_3.Rds")
NMS <- readRDS("data/museum/NMS_1_8_3.Rds")
AMNH <- readRDS("data/museum/AMNH_1_8_3.Rds")
Peabody <- readRDS("data/museum/Peabody_1_8_3.Rds")
GBIF <- readRDS("data/GBIF/GBIF_1_8_3.Rds")

## PBDB synonymisations applied in script 1_5

#### clean_name, misspell, punctuation, capitalisation ####

#### Adding combined name to use ####


#### Functions previously used for non-PBDB data ####
## Use misspell to address dipthongs
source("functions/misspell.R")
NMS_biv$genus <- misspell(NMS_biv$genus)
NMS_brach$genus <- misspell(NMS_brach$genus)

## Punctuation
NMS_biv$genus <- str_replace_all(NMS_biv$genus, pattern = "[:punct:]", replacement = "")
NMS_brach$genus <- str_replace_all(NMS_brach$genus, pattern = "[:punct:]", replacement = "")

## Capitalisation
NMS_biv$genus <- str_to_title(NMS_biv$genus)
NMS_brach$genus <- str_to_title(NMS_brach$genus)

#### Functions previously used for PBDB data ####
## Stip out subgenera in brackets
PBDB$genus <- str_split_i(PBDB$genus, pattern = ' ', i = 1)

## Standardise dipthongs for genera
source("functions/misspell.R")
PBDB$genus <- misspell(PBDB$genus)
