## 1.5 Initial data processing - PDBD cleaning
## Started by TJS on 08/01/2024

## If packages aren't installed, install them, then load them
packages <- c("fossilbrush", "rnaturalearth", "rnaturalearthdata", "divvy", "stringr", "divDyn")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(fossilbrush)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(divvy)
library(divDyn)

## install divvyCompanion from github and load
#library(remotes)
#install_github("PalaeoTom/divvyCompanion")
library(divvyCompanion)

## Clean directory
rm(list = ls())

#### Cleaning PBDB ####
## Load raw PBDB data
raw_PBDB <- readRDS("data/unclean_data/PBDB_Nov23.Rds")

## Clean time data
## Isolate bivalve and brachiopod data
raw_PBDB <- raw_PBDB[c(which(raw_PBDB$phylum == "Brachiopoda"),which(raw_PBDB$class == "Bivalvia")),]

## use fossilbrush to update Chronostratigraphy
PBDB <- chrono_scale(raw_PBDB,  tscale = "GTS2020", srt = "early_interval", end = "late_interval",
                     max_ma = "max_ma", min_ma = "min_ma", verbose = FALSE)

## set new chronostratigraphy as "max_ma" and "min_ma", then remove added columns newFAD and newLAD
PBDB$max_ma <- PBDB$newFAD
PBDB$min_ma <- PBDB$newLAD
PBDB <- PBDB[,-c(29,30)]

## check for and remove any entries with nonsensical entries (LAD older than FAD)
if(any(PBDB$max_ma < PBDB$min_ma)){
  PBDB <- PBDB[-which(PBDB$max_ma < PBDB$min_ma),]
}

#### Checking ages and coordinates ####
#View(data.frame(table(PBDB$paleolat)))
#View(data.frame(table(PBDB$paleolng)))
#View(data.frame(table(PBDB$max_ma)))
#View(data.frame(table(PBDB$min_ma)))
## All good!

#### Cleaning taxonomy ####
## Replace PBDB default missing data entry with NA for other taxonomic levels
PBDB[grep("NO_", PBDB[,"class"]),"class"] <- NA
PBDB[grep("NO_", PBDB[,"order"]), "order"] <- NA
PBDB[grep("NO_", PBDB[,"family"]), "family"] <- NA
PBDB[grep("NO_", PBDB[,"genus"]), "genus"] <- NA

## Check all classes with NA are Brachiopoda
all(PBDB[which(is.na(PBDB$class)),"phylum"] == "Brachiopoda")
## All good!

## Drop all genera with NA
PBDB <- PBDB[-which(is.na(PBDB$genus)), ]

## Manually inspect genera
#View(data.frame(table(PBDB$genus)))
## Only issue - subgenera in brackets. Should be stripped out. Use Regex search to reduce to first string.
PBDB$genus <- str_split_i(PBDB$genus, pattern = ' ', i = 1)

## Use misspell function to update genus, family, and order designations
source("functions/misspell.R")
PBDB$genus <- misspell(PBDB$genus)

## Check for punctuation and capitalisation
PBDB$genus <- str_replace_all(PBDB$genus, pattern = "[:punct:]", replacement = "")
PBDB$genus <- str_to_title(PBDB$genus)

#### Adding environmental covariates ####
## Define categories
data(keys)
PBDB$lithCat <- categorize(PBDB$lithology1, keys$lith)
PBDB$bathCat <- categorize(PBDB$environment, keys$bath)
PBDB$reefCat <- categorize(PBDB$environment, keys$reefs)
PBDB$reefCat[PBDB$lithCat == "siliciclastic" & PBDB$environment == "marine indet."] <- "non-reef"

#### Add ecological data based on Guo et al. (2023) ####
## Read in depth categories
#setwd("/Users/tjs/Library/CloudStorage/OneDrive-Nexus365/Bivalve_brachiopod/data")
#bivalve.ecology <- read.csv("Bivalvia_ecology_Guo2023.csv")
#brachiopod.ecology <- read.csv("Brachiopoda_ecology_Guo2023.csv")
#setwd(home)

## Convert to ecology and add infaunal/epifaunal supercategories
#bivalve.ecology[,"ecology"] <- NA
#brachiopod.ecology[,"ecology"] <- NA
#bivalve.ecology[,"category"] <- NA
#brachiopod.ecology[,"category"] <- NA
#biv.code <- c("epibyssate", "cemented", "recliner", "shallowI", "deepI", "unknownE")
#brach.code <- c("pedicle", "cemented", "recliner", "infaunal", "unknownE")
#biv.cat.code <- c("epifaunal", "epifaunal", "epifaunal", "infaunal", "infaunal", "epifaunal")
#brach.cat.code <- c("epifaunal", "epifaunal", "epifaunal", "infaunal", "epifaunal")
#for(i in 1:length(biv.code)){
#  bivalve.ecology[which(bivalve.ecology[,"lifestyle"] == i),"ecology"] <- biv.code[i]
#  bivalve.ecology[which(bivalve.ecology[,"lifestyle"] == i),"category"] <- biv.cat.code[i]
#}
#for(i in 1:length(brach.code)){
#  brachiopod.ecology[which(brachiopod.ecology[,"lifestyle"] == i),"ecology"] <- brach.code[i]
#  brachiopod.ecology[which(brachiopod.ecology[,"lifestyle"] == i),"category"] <- brach.cat.code[i]
#}
#bivalve.ecology <- bivalve.ecology[,-2]
#brachiopod.ecology <- brachiopod.ecology[,-2]
#
### Export
#write.csv(brachiopod.ecology, "data/Guo2023_brachiopod_ecology.csv")
#write.csv(bivalve.ecology, "data/Guo2023_bivalve_ecology.csv")

#### Assign depth categories to PBDB data
## Define key
brachiopod.ecology <- read.csv("data/metadata/Guo2023_brachiopod_ecology.csv", row.names = 1)
bivalve.ecology <- read.csv("data/metadata/Guo2023_bivalve_ecology.csv", row.names = 1)
key <- rbind(brachiopod.ecology, bivalve.ecology)

## Read in function
source("functions/add.ecology.IDs.R")
PBDB <- add.ecology.IDs(data = PBDB, key)

## Export data
saveRDS(PBDB, "data/PBDB/PBDB.Rds")
