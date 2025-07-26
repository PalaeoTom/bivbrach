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

## Read in functions
source("functions/misspell.R")
source("functions/standardiseCells.R")

#### Standardised data with abundances ####






#### Unique species in each cell - setup ###
## Read in data
master_50 <- readRDS("data/final/master_50_2_2.Rds")
master_100 <- readRDS("data/final/master_100_2_2.Rds")
master_200 <- readRDS("data/final/master_200_2_2.Rds")

## Set minimum number of occurrences
occs.min <- 5

#### 50km ####
## Clean up names
master_50$species <- clean_name(master_50$species)

## Drop NAs
master_50 <- master_50[-which(is.na(master_50$species)),]

## Standardise dipthongs
master_50$species <- misspell(master_50$species)

## Check for punctuation
any(str_detect(master_50$species, pattern = "[:punct:]"))

## Capitalisation
master_50$species <- str_to_lower(master_50$species)

## Now to combine into a single unique name to update combined name
master_50$combined_name <- apply(master_50, 1, function(x) paste0(x[5], "_", x[9], "_", x[10]))

## Get time bins
timeBins <- sort(unique(master_50$stage))

## Create container
master_50_U <- master_50[NULL,]

## Populate container
for(i in timeBins){
  ## Get subset
  master_50_U <- rbind(master_50_U,uniqify(master_50[which(master_50$stage==i),], xy = c("cellx_50km","celly_50km"), taxVar = "combined_name"))
}

## Run function
master_50_U <- standardiseCells(data = master_50_U, stage_cell = "stage_cell", minOccs = occs.min)

#### Export datasets
saveRDS(master_50_U, "data/final/master_50_2_3_species_std_U.Rds")

