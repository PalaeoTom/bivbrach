## 3. Spatial subsampling using modified divvy
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("divvy", "velociraptr", "dplyr", "plyr", "iNEXT", "parallel", "RColorbrewer")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divvy)
library(velociraptr)
library(dplyr)
library(plyr)
library(iNEXT)
library(parallel)
library(RColorBrewer)

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

## Define variables
radii <- as.integer(c(100000, 500000, 1000000, 2000000))
siteQuotas <- c(3, 6, 9, 12, 15)
overlapThresholds <- c(0, 0.25, 0.5, 0.75, 1)
overlapTypes <- c("area", "sites")
weightStandardisation_1 <- F

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
## Try subsampling with and without weighting of cells
weightStandardisation_2 <- c(T,F)

cut.biscuits(data = genera.10ma, siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/st_spaSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, standardiseSiteNumber = F, weightedStandardisation = weightStandardisation_2,
             taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), name.output = "BB_gen_10ma_stan", n.cores = 4)

cut.biscuits(data = genera.stages, siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/st_spaSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, standardiseSiteNumber = F, weightedStandardisation = weightStandardisation_2,
             taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), name.output = "BB_gen_stag_stan", n.cores = 4)

cut.biscuits(data = species.10ma, siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/st_spaSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, standardiseSiteNumber = F, weightedStandardisation = weightStandardisation_2,
             taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), name.output = "BB_spec_10ma_stan", n.cores = 4)

cut.biscuits(data = species.stages, siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/st_spaSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, standardiseSiteNumber = F, weightedStandardisation = weightStandardisation_2,
             taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), name.output = "BB_spec_stag_stan", n.cores = 4)

#### Step 1 - get performance data on different settings. Number of viable spatial subsamples under all configurations for all datasets through time.
source("functions/count.viable.samples.R")
dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)), paste0("oTh",seq(1,length(overlapThresholds),1)), paste0("oTy",seq(1,length(overlapTypes),1)))
cores <- 4
taxa.split <- T

count.viable.samples(dir = dir, pre = "BB_gen_10ma_raw", vars = vars, sD = genera.10ma, n.cores = cores, taxa = taxa.split, output.dir = "~/R_packages/R_projects/bivbrach/data", output.name = "BB_gen_10ma_raw")
count.viable.samples(dir = dir, pre = "BB_spec_10ma_raw", vars = vars, sD = species.10ma, n.cores = cores, taxa = taxa.split, output.dir = "~/R_packages/R_projects/bivbrach/data", output.name = "BB_spec_10ma_raw")
count.viable.samples(dir = dir, pre = "BB_gen_stag_raw", vars = vars, sD = genera.stages, n.cores = cores, taxa = taxa.split, output.dir = "~/R_packages/R_projects/bivbrach/data", output.name = "BB_gen_stag_raw")
count.viable.samples(dir = dir, pre = "BB_spec_stag_raw", vars = vars, sD = species.stages, n.cores = cores, taxa = taxa.split, output.dir = "~/R_packages/R_projects/bivbrach/data", output.name = "BB_spec_stag_raw")

## read in result
BB_gen_10ma_raw_VCs <- read.csv("data/BB_gen_10ma_raw_viable_subsamples.csv", header = T, row.names = 1)
BB_gen_stag_raw_VCs <- read.csv("data/BB_gen_stag_raw_viable_subsamples.csv", header = T, row.names = 1)
BB_spec_10ma_raw_VCs <- read.csv("data/BB_spec_10ma_raw_viable_subsamples.csv", header = T, row.names = 1)
BB_spec_stag_raw_VCs <- read.csv("data/BB_spec_stag_raw_viable_subsamples.csv", header = T, row.names = 1)

## Update colnames of stage data to make them more usable
stages <- downloadTime('international ages')
stages$name <- row.names(stages)
stages <- stages[order(stages$b_age, decreasing=TRUE), ]
stages2omit <- c('Stage 2','Stage 3','Stage 4','Wuliuan',
                 'Drumian','Guzhangian','Paibian','Jiangshanian',
                 'Stage 10',
                 'Floian','Darriwilian',
                 'Katian', # otherwise no seed cells for Sandbian
                 'Aeronian', # otherwise no Rhuddanian or Aeronian seed cells
                 'Homerian','Ludfordian','Pragian','Eifelian','Bashkirian','Kasimovian',
                 'Sakmarian','Kungurian', # no Artinskian species records
                 'Olenekian', # otherwise only 1 abundance datum for Olenekian
                 'Sinemurian', # Hettangian is too poorly sampled
                 'Bajocian', # otherwise no Aalenian seed cells
                 'Hauterivian','Barremian', # no seed cells for Haut., Barremian or Valanginian alone
                 'Santonian', # otherwise nothing survives from Coniacian
                 'Thanetian',
                 'Bartonian', # otherwise no environmental data for Bartonian
                 'Aquitanian', # otherwise no seeds here or in Chattian
                 'Serravallian', # otherwise no seed cells for Langhian
                 'Messinian', # otherwise no seed cells for Messinian
                 'Calabrian','Middle Pleistocene','Late Pleistocene', # otherwise weird extinction rates
                 'Northgrippian','Meghalayan') # lump all Holocene records so they're easy to remove later
stages_trunc <- stages[!(stages$name %in% stages2omit),] #remove lumped stages
colnames(BB_gen_stag_raw_VCs) <- rownames(stages_trunc)[-65]
colnames(BB_spec_stag_raw_VCs) <- rownames(stages_trunc)[-65]

## convert to number of time bins exceeded x samples
source("functions/countUsable.R")
source("functions/wrap.countUsable.R")

## get grid of variable combination
varGrid <- expand.grid(siteQuotas, radii/1000, overlapThresholds, overlapTypes, weightStandardisation_1)
threshold.VC <- 15
gen_10ma_UTBs <- wrap.countUsable(VC = BB_gen_10ma_raw_VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))
spec_10ma_UTBs <- wrap.countUsable(VC = BB_gen_10ma_raw_VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))
gen_stag_UTBs <- wrap.countUsable(VC = BB_gen_10ma_raw_VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))
spec_stag_UTBs <- wrap.countUsable(VC = BB_gen_10ma_raw_VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))

## get combination of siteQuotas and radii
split.vars <- expand.grid(radii/1000, siteQuotas)
colnames(split.vars) <- c("radius", "siteQuota")

## get plotting data
source("functions/get.VC.plot.data.R")
gen_10ma_barData <- get.VC.plot.data(UTB = gen_10ma_UTBs, split.vars = split.vars)
spec_10ma_barData <- get.VC.plot.data(UTB = spec_10ma_UTBs, split.vars = split.vars)
gen_stag_barData <- get.VC.plot.data(UTB = gen_stag_UTBs, split.vars = split.vars)
spec_stag_barData <- get.VC.plot.data(UTB = spec_stag_UTBs, split.vars = split.vars)

## plot bar charts
View(gen_10ma_barData[[1]])

labels <- c(paste0("area_", overlapThresholds),paste0("sites_", overlapThresholds))
palette <- c(brewer.pal(5, "Blues"), brewer.pal(5, "Greens"))

title <- paste0("Radius ", split.vars[1,1], " ", "siteQuota ", split.vars[1,2])

## function for getting plotting data for bar charts for combinations of two variables





#### Step 2 - drop non-viable time bins from each dataset ####

## get time data for stage and 10ma midpoints
## Get midpoints of bins and stages
stage.midpoints <- as.numeric(names(genera.stages))
bin.midpoints <- as.numeric(names(genera.10ma))

## which time bins have no data
gen.10ma.EBs <- bin.midpoints[which(colSums(BB_gen_10ma_raw_VCs) == 0)]
spec.10ma.EBs <- bin.midpoints[which(colSums(BB_spec_10ma_raw_VCs) == 0)]
gen.stag.EBs <- stage.midpoints[which(colSums(BB_gen_stag_raw_VCs) == 0)]
spec.stag.EBs <- stage.midpoints[which(colSums(BB_spec_stag_raw_VCs) == 0)]

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

#### Step 3 - calculate richness in different ways ####

#### Step 4 - correlation tests ####


#### Richness function something for another script ####
#### Use SQS to derive diversity estimate for each rep ####
source("functions/get.richness.R")
gen.stages <- get.richness(stages.gen.correct)
gen.bins <- get.richness(bin10.gen.correct)

spec.stages <- get.richness(stages.spec.correct, taxVar = "unique_name")
spec.bins <- get.richness(bin10.spec.correct, taxVar = "unique_name")

