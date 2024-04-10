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
#stage.midpoints <- names(genera.stages)
#bin.midpoints <- names(genera.10ma)

## Define variables
radii <- c(100000, 500000, 1000000, 2000000)
siteQuotas <- c(3, 6, 9, 12, 15)
overlapThresholds <- c(0, 0.25, 0.5, 0.75, 1)
overlapTypes <- c("area", "sites")
weightStandardisation_1 <- F
weightStandardisation_2 <- c(T,F)

#### Generate spatial subsamples ####
## Not standardised for occupancy (will standardise using SQS)
cut.biscuits(data = genera.10ma, siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, standardiseSiteNumber = F, weightedStandardisation = weightStandardisation_1,
             taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), name.output = "BB_gen_10ma_raw", n.cores = 4)

cut.biscuits(data = genera.stages, siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, standardiseSiteNumber = F, weightedStandardisation = weightStandardisation_1,
             taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), name.output = "BB_gen_stag_raw", n.cores = 4)

cut.biscuits(data = species.10ma, siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, standardiseSiteNumber = F, weightedStandardisation = weightStandardisation_1,
             taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), name.output = "BB_spec_10ma_raw", n.cores = 4)

cut.biscuits(data = species.stages, siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, standardiseSiteNumber = F, weightedStandardisation = weightStandardisation_1,
             taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), name.output = "BB_spec_stag_raw", n.cores = 4)

## Standardised for occupancy (weighted and unweighted) <- this needs running on a cluster
cut.biscuits(data = genera.10ma, siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_stSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, standardiseSiteNumber = F, weightedStandardisation = weightStandardisation_2,
             taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), name.output = "BB_gen_10ma_stan", n.cores = 4)

cut.biscuits(data = genera.stages, siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_stSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, standardiseSiteNumber = F, weightedStandardisation = weightStandardisation_2,
             taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), name.output = "BB_gen_stag_stan", n.cores = 4)

cut.biscuits(data = species.10ma, siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_stSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, standardiseSiteNumber = F, weightedStandardisation = weightStandardisation_2,
             taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), name.output = "BB_spec_10ma_stan", n.cores = 4)

cut.biscuits(data = species.stages, siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_stSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, standardiseSiteNumber = F, weightedStandardisation = weightStandardisation_2,
             taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), name.output = "BB_spec_stag_stan", n.cores = 4)

#### Step 1 - get performance data on different settings. Number of viable spatial subsamples under all configurations for all datasets through time.

#### Step 2 - drop non-viable time bins from each dataset ####

#### Step 3 - calculate richness in different ways ####

#### Step 4 - correlation tests ####

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




#### Richness function something for another script ####
#### Use SQS to derive diversity estimate for each rep ####
source("functions/get.richness.R")
gen.stages <- get.richness(stages.gen.correct)
gen.bins <- get.richness(bin10.gen.correct)

spec.stages <- get.richness(stages.spec.correct, taxVar = "unique_name")
spec.bins <- get.richness(bin10.spec.correct, taxVar = "unique_name")

