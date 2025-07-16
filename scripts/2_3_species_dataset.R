## 2.3 Generating a species dataset
## Started by TJS on 08/01/2024

## If packages aren't installed, install them, then load them
packages <- c("divvy", "stringr", "fossilbrush")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divvy)
library(stringr)
library(fossilbrush)

## Clean directory
rm(list = ls())

#### 50km ####
## Read in data
master_50 <- readRDS("data/final/master_50_2_2.Rds")

## Clean up names
master_50$species <- clean_name(master_50$species)

## Drop NAs
master_50 <- master_50[-which(is.na(master_50$species)),]

## Standardise dipthongs
source("functions/misspell.R")
master_50$species <- misspell(master_50$species)

## Check for punctuation
any(str_detect(master_50$species, pattern = "[:punct:]"))

## Capitalisation
master_50$species <- str_to_lower(master_50$species)

## Now to combine into a single unique name to update combined name
master_50$combined_name <- apply(master_50, 1, function(x) paste0(x[5], "_", x[9], "_", x[10]))

## Remove duplicate occurrences from each grid cell-time bin combination
## Get time bins
timeBins <- sort(unique(master_50$stage))
## 50km
uniq_master_50 <- master_50[NULL,]
for(i in timeBins){
  ## Get subset
  uniq_master_50 <- rbind(uniq_master_50,uniqify(master_50[which(master_50$stage==i),], xy = c("cellx_50km","celly_50km"), taxVar = "combined_name"))
}

## Standardise uniqified grid cells
source("functions/standardiseCells.R")
occs.min <- 5

## Run function
uniq_master_50 <- standardiseCells(data = uniq_master_50, stage_cell = "stage_cell", minOccs = occs.min)

#### Export datasets
saveRDS(uniq_master_50, "data/final/master_50_uniq_species.Rds")

#### 100km ####
## Read in data
master_100 <- readRDS("data/final/master_100_2_2.Rds")

## Clean up names
master_100$species <- clean_name(master_100$species)

## Drop NAs
master_100 <- master_100[-which(is.na(master_100$species)),]

## Standardise dipthongs
source("functions/misspell.R")
master_100$species <- misspell(master_100$species)

## Check for punctuation
any(str_detect(master_100$species, pattern = "[:punct:]"))

## Capitalisation
master_100$species <- str_to_lower(master_100$species)

## Now to combine into a single unique name to update combined name
master_100$combined_name <- apply(master_100, 1, function(x) paste0(x[5], "_", x[9], "_", x[10]))

## Remove duplicate occurrences from each grid cell-time bin combination
## Get time bins
timeBins <- sort(unique(master_100$stage))
## 100km
uniq_master_100 <- master_100[NULL,]
for(i in timeBins){
  ## Get subset
  uniq_master_100 <- rbind(uniq_master_100,uniqify(master_100[which(master_100$stage==i),], xy = c("cellx_100km","celly_100km"), taxVar = "combined_name"))
}

## Standardise uniqified grid cells
source("functions/standardiseCells.R")
occs.min <- 5

## Run function
uniq_master_100 <- standardiseCells(data = uniq_master_100, stage_cell = "stage_cell", minOccs = occs.min)

#### Export datasets
saveRDS(uniq_master_100, "data/final/master_100_uniq_species.Rds")

#### 200km ####
## Read in data
master_200 <- readRDS("data/final/master_200_2_2.Rds")

## Clean up names
master_200$species <- clean_name(master_200$species)

## Drop NAs
master_200 <- master_200[-which(is.na(master_200$species)),]

## Standardise dipthongs
source("functions/misspell.R")
master_200$species <- misspell(master_200$species)

## Check for punctuation
any(str_detect(master_200$species, pattern = "[:punct:]"))

## Capitalisation
master_200$species <- str_to_lower(master_200$species)

## Now to combine into a single unique name to update combined name
master_200$combined_name <- apply(master_200, 1, function(x) paste0(x[5], "_", x[9], "_", x[10]))

## Remove duplicate occurrences from each grid cell-time bin combination
## Get time bins
timeBins <- sort(unique(master_200$stage))
## 200km
uniq_master_200 <- master_200[NULL,]
for(i in timeBins){
  ## Get subset
  uniq_master_200 <- rbind(uniq_master_200,uniqify(master_200[which(master_200$stage==i),], xy = c("cellx_200km","celly_200km"), taxVar = "combined_name"))
}

## Standardise uniqified grid cells
source("functions/standardiseCells.R")
occs.min <- 5

## Run function
uniq_master_200 <- standardiseCells(data = uniq_master_200, stage_cell = "stage_cell", minOccs = occs.min)

#### Export datasets
saveRDS(uniq_master_200, "data/final/master_200_uniq_species.Rds")

