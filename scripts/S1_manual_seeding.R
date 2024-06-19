## S1. Spatial subsampling using modified divvy and specified seeds
## Started by TJS on 17/06/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("divvy", "velociraptr", "dplyr", "plyr", "parallel", "RColorBrewer", "plyr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divvy)
library(velociraptr)
library(dplyr)
library(plyr)
library(parallel)
library(RColorBrewer)
library(plyr)

## Load data
setwd("~/R_packages/R_projects/bivbrach")
stages.g200 <- readRDS("data/stages_g200.Rds")
stages.g100 <- readRDS("data/stages_g100.Rds")
stages.s200 <- readRDS("data/stages_s200.Rds")
stages.s100 <- readRDS("data/stages_s100.Rds")

#### Identify viable cookies ####
## Load new functions
source("functions/findPool2.R")
source("functions/findSeeds2.R")
source("functions/getOverlap.R")
source("functions/cookie.R")
source("functions/biscuits.R")
source("functions/rasterOccData.R")

## Define variables
radii <- as.integer(c(200000, 500000, 1000000))
siteQuotas <- c(2, 3, 4, 5)
overlapThresholds <- 0
overlapTypes <- "sites"
weightStandardisation_1 <- F

#### Generate spatial subsamples ####
## Generate output vectors
output.vector <- c("stages_g200",
                   "stages_g100",
                   "stages_s200",
                   "stages_s100")

data.strings <- c("stages.g200",
                  "stages.g100",
                  "stages.s200",
                  "stages.s100")

## Isolate test data frames
lotsOData <- stages.s200[["449"]]
noData <- stages.s200[["534"]]
littleData <- stages.s200[["167"]]

## Define seeds
## Going to pick some random occurrence to serve as seeds
seedOccs <- sample(unique(stages.s200[["449"]][,"collection_no"]), 10, replace = F)
paleolng <- unique(stages.s200[["449"]][which(stages.s200[["449"]][,"collection_no"] %in% seedOccs),"paleolng"])
paleolat <- unique(stages.s200[["449"]][which(stages.s200[["449"]][,"collection_no"] %in% seedOccs),"paleolat"])
seeds <- rasteriseOccData(occData = stages.s200[["449"]], res = 200000, xyCoords1 = paleolng, xyCoords2 = paleolat, xyCoords = c('paleolng','paleolat'))

## Now to run it as part of the biscuits function
test <- biscuits(dataMat = lotsOData, xy = c("cellX", "cellY"), r = radii[3], seeding = NULL, standardiseSiteN = F, rep = 100, nSite = 2, oThreshold = 0,
         oType = "sites", oPruningMode = "maxOccs", weight = F, returnSeeds = F, crs = "EPSG:8857", output = "full")

