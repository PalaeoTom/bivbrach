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

#### clean_name, misspell, punctuation, capitalisation ####
## Clean name
PBDB$genus <- clean_name(PBDB$genus)
GBIF$genus <- clean_name(GBIF$genus)
NMS$genus <- clean_name(NMS$genus)
AMNH$genus <- clean_name(AMNH$genus)
Peabody$genus <- clean_name(Peabody$genus)

## Standardise dipthongs
source("functions/misspell.R")
PBDB$genus <- misspell(PBDB$genus)
GBIF$genus <- misspell(GBIF$genus)
NMS$genus <- misspell(NMS$genus)
AMNH$genus <- misspell(AMNH$genus)
Peabody$genus <- misspell(Peabody$genus)

## Check for punctuation
any(str_detect(PBDB$genus, pattern = "[:punct:]"))
any(str_detect(GBIF$genus, pattern = "[:punct:]"))
any(str_detect(AMNH$genus, pattern = "[:punct:]"))
any(str_detect(Peabody$genus, pattern = "[:punct:]"))
any(str_detect(NMS$genus, pattern = "[:punct:]"))

## Capitalisation
PBDB$genus <- str_to_title(PBDB$genus)
GBIF$genus <- str_to_title(GBIF$genus)
NMS$genus <- str_to_title(NMS$genus)
AMNH$genus <- str_to_title(AMNH$genus)
Peabody$genus <- str_to_title(Peabody$genus)

## Combine phylum and genus into single name
View(data.frame(colnames(PBDB)))
PBDB$combined_name <- apply(PBDB, 1, function(x) paste0(x[12], "_", x[16]))
View(data.frame(colnames(GBIF)))
GBIF$combined_name <- apply(GBIF, 1, function(x) paste0(x[3], "_", x[7]))
View(data.frame(colnames(AMNH)))
AMNH$combined_name <- apply(AMNH, 1, function(x) paste0(x[3], "_", x[7]))
View(data.frame(colnames(NMS)))
NMS$combined_name <- apply(NMS, 1, function(x) paste0(x[2], "_", x[3]))
View(data.frame(colnames(Peabody)))
Peabody$combined_name <- apply(Peabody, 1, function(x) paste0(x[2], "_", x[7]))

## Export
saveRDS(PBDB, "data/PBDB/PBDB_1_9.Rds")
saveRDS(GBIF, "data/GBIF/GBIF_1_9.Rds")
saveRDS(AMNH, "data/museum/AMNH_1_9.Rds")
saveRDS(NMS, "data/museum/NMS_1_9.Rds")
saveRDS(Peabody, "data/museum/Peabody_1_9.Rds")
