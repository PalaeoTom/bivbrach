## 3. Spatial subsampling using modified divvy
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("divvy", "velociraptr", "dplyr", "plyr", "iNEXT", "parallel")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divvy)
library(velociraptr)
library(dplyr)
library(plyr)
library(iNEXT)
library(parallel)

## Load data
setwd("~/R_packages/R_projects/bivbrach")
genera.10ma <- readRDS("data/BB_genera_10maBins.Rds")
species.10ma <- readRDS("data/BB_species_10maBins.Rds")
genera.stages <- readRDS("data/BB_genera_stageBins.Rds")
species.stages <- readRDS("data/BB_species_stageBins.Rds")

#### Identify viable cookies ####
## Load new functions
source("functions/findPool2.R")
source("functions/findSeeds2.R")
source("functions/getOverlap.R")
source("functions/cookie.R")
source("functions/biscuits.R")

## Load wrapper function for running all analyses
source("functions/cut.biscuits.R")

## Get midpoints of bins and stages
stage.midpoints <- names(genera.stages)
bin.midpoints <- names(genera.10ma)

data = genera.stages
siteQuota = c(3, 6, 9, 12, 15)
r = c(100, 500, 1000, 2000)
b.crs = 'EPSG:8857'
b.xy = c("cellX", "cellY")
overlapThreshold = c(0, 0.25, 0.5, 0.75, 1)
overlapType = c("area", "sites")
reps = 100
biscuitWeight = F
standardiseSiteNumber = T
taxa = NULL
taxa.level = NULL
name.output <- "BB"

## Define variables
radii <- c(100, 500, 1000, 2000)
siteQuotas <- c(3, 6, 9, 12, 15)
overlapThresholds <- c(0, 0.25, 0.5, 0.75, 1)
overlapTypes <- c("area", "sites")
weightedStandardisation <- F
standardiseSiteNumber <- F

#### Controlling for proportion of overlapping area ####
gen.st.ar <- cut.biscuits(data = stages.genera,
                                        biscuitThreshold = 0.5, overlapType = "area", siteQuota = 3, r = 1000000, biscuitWeight = F, standardiseSiteNumber = F,
                                        b.crs = 'EPSG:8857', taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))

stages.spec.regions.area <- cut.biscuits(data = stages.species,
                                         biscuitThreshold = 0.5, overlapType = "area", siteQuota = 3, r = 1000000, biscuitWeight = F, standardiseSiteNumber = F,
                                         b.crs = 'EPSG:8857', taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))

bin10.gen.regions.area <- cut.biscuits(data = bin10.genera,
                                       biscuitThreshold = 0.5, overlapType = "area", siteQuota = 3, r = 1000000, biscuitWeight = F, standardiseSiteNumber = F,
                                       b.crs = 'EPSG:8857', taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))

bin10.spec.regions.area <- cut.biscuits(data = bin10.species,
                                        biscuitThreshold = 0.5, overlapType = "area", siteQuota = 3, r = 1000000, biscuitWeight = F, standardiseSiteNumber = F,
                                        b.crs = 'EPSG:8857', taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))

#### Controlling for proportion of sites matching between subsamples ####
stages.gen.regions.sites <- cut.biscuits(data = stages.genera,
                                         biscuitThreshold = 0.5, overlapType = "cells", siteQuota = 3, r = 1000000, biscuitWeight = F, standardiseSiteNumber = F,
                                         b.crs = 'EPSG:8857', taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))

stages.spec.regions.sites <- cut.biscuits(data = stages.species,
                                          biscuitThreshold = 0.5, overlapType = "cells", siteQuota = 3, r = 1000000, biscuitWeight = F, standardiseSiteNumber = F,
                                          b.crs = 'EPSG:8857', taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))

bin10.gen.regions.sites <- cut.biscuits(data = bin10.genera,
                                        biscuitThreshold = 0.5, overlapType = "cells", siteQuota = 3, r = 1000000, biscuitWeight = F, standardiseSiteNumber = F,
                                        b.crs = 'EPSG:8857', taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))

bin10.spec.regions.sites <- cut.biscuits(data = bin10.species,
                                         biscuitThreshold = 0.5, overlapType = "cells", siteQuota = 3, r = 1000000, biscuitWeight = F, standardiseSiteNumber = F,
                                         b.crs = 'EPSG:8857', taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))

#### get midpoints of ages, then remove non-viable bins ####
source("functions/label.and.drop.R")

### First, let's do samples where amount of area overlapping is controlled
## label both stage data objects and drop NAs
stages.gen.regions.area <- label.and.drop(stages.gen.regions.area, stage.midpoints)
stages.spec.regions.area <- label.and.drop(stages.spec.regions.area, stage.midpoints)

## update midpoints vector for later
stage.midpoints <- stage.midpoints[stage.midpoints %in% as.numeric(names(stages.gen.regions.area[[1]]))]

## now to do the same for 10ma time bins
## label and drop
bin10.gen.regions.area <- label.and.drop(bin10.gen.regions.area, bin.midpoints)
bin10.spec.regions.area <- label.and.drop(bin10.spec.regions.area, bin.midpoints)

## update midpoints (for plotting later)
bin.midpoints <- bin.midpoints[bin.midpoints %in% as.numeric(names(bin10.gen.regions.area[[1]]))]

#### Use SQS to derive diversity estimate for each rep ####
source("functions/get.richness.R")
gen.stages <- get.richness(stages.gen.correct)
gen.bins <- get.richness(bin10.gen.correct)

spec.stages <- get.richness(stages.spec.correct, taxVar = "unique_name")
spec.bins <- get.richness(bin10.spec.correct, taxVar = "unique_name")

#### Drop time bins with all NA, then drop reps with NAs ####

#### Get correlation values ####
## Should be left with time bins of cookies (possibly variable number).

