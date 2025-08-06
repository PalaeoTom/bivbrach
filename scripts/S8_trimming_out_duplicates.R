## S8 - trimming out duplicates
## Started by TJS on 08/01/2024

## If packages aren't installed, install them, then load them
packages <- c("divvy", "stringr", "fossilbrush", "divDyn", "ggplot2", "rnaturalearth", "rnaturalearthdata")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divvy)
library(stringr)
library(fossilbrush)
library(divDyn)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

## Clean directory
rm(list = ls())

#### Remove duplicates for sensitivity test ####
## Read in master_XXX_2_2
master_50 <- readRDS("data/final/master_50_2_2.Rds")
master_100 <- readRDS("data/final/master_100_2_2.Rds")
master_150 <- readRDS("data/final/master_150_2_2.Rds")
master_200 <- readRDS("data/final/master_200_2_2.Rds")

## Read in function
source("functions/standardiseCells.R")

## Set minimum
occs.min <- 5

## 50km
## Get time bins
timeBins <- sort(unique(master_50$stage))
## Remove duplicates from grid cells
uniq_master_50 <- master_50[NULL,]
for(i in timeBins){
  ## Get subset
  uniq_master_50 <- rbind(uniq_master_50,uniqify(master_50[which(master_50$stage==i),], xy = c("cellx_50km","celly_50km"), taxVar = "combined_name"))
}

## Run function
uniq_master_50 <- standardiseCells(data = uniq_master_50, cell = "stage_cell", minOccs = occs.min)

## Export uniqified dataset
saveRDS(uniq_master_50, "data/final/master_50_2_2_std_U.Rds")

## 100km
## Get time bins
timeBins <- sort(unique(master_100$stage))
## Remove duplicates from grid cells
uniq_master_100 <- master_100[NULL,]
for(i in timeBins){
  ## Get subset
  uniq_master_100 <- rbind(uniq_master_100,uniqify(master_100[which(master_100$stage==i),], xy = c("cellx_100km","celly_100km"), taxVar = "combined_name"))
}

## Run function
uniq_master_100 <- standardiseCells(data = uniq_master_100, cell = "stage_cell", minOccs = occs.min)

## Export uniqified dataset
saveRDS(uniq_master_100, "data/final/master_100_2_2_std_U.Rds")

## 150km
## Get time bins
timeBins <- sort(unique(master_150$stage))
## Remove duplicates from grid cells
uniq_master_150 <- master_150[NULL,]
for(i in timeBins){
  ## Get subset
  uniq_master_150 <- rbind(uniq_master_150,uniqify(master_150[which(master_150$stage==i),], xy = c("cellx_150km","celly_150km"), taxVar = "combined_name"))
}

## Run function
uniq_master_150 <- standardiseCells(data = uniq_master_150, cell = "stage_cell", minOccs = occs.min)

## Export uniqified dataset
saveRDS(uniq_master_150, "data/final/master_150_2_2_std_U.Rds")

## 200km
## Get time bins
timeBins <- sort(unique(master_200$stage))
## Remove duplicates from grid cells
uniq_master_200 <- master_200[NULL,]
for(i in timeBins){
  ## Get subset
  uniq_master_200 <- rbind(uniq_master_200,uniqify(master_200[which(master_200$stage==i),], xy = c("cellx_200km","celly_200km"), taxVar = "combined_name"))
}

## Run function
uniq_master_200 <- standardiseCells(data = uniq_master_200, cell = "stage_cell", minOccs = occs.min)

## Export uniqified dataset
saveRDS(uniq_master_200, "data/final/master_200_2_2_std_U.Rds")
