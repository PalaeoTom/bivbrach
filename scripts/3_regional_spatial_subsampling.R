## 3. Spatial subsampling using modified divvy
## Started by TJS on 08/01/2024

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
source("functions/biscuitsBatch.R")

## Define variables
radii <- as.integer(c(200000, 500000, 1000000))
siteQuotas <- c(2, 3, 4, 5)
overlapThresholds <- 0
overlapTypes <- "sites"

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

## Get arguments for standard run
i = 1
dataList = eval(parse(text=data.strings[i]))
siteQuota = siteQuotas
r = radii
b.crs = 'EPSG:8857'
b.xy = c("cellX", "cellY")
output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
overlapThreshold = overlapThresholds
overlapType = overlapTypes
rarefaction = "sitesThenOccs"
name.output = output.vector[i]
n.cores = 4
reps = 100
home <- getwd()
settings <- expand.grid(siteQuota, r, overlapThreshold, overlapType)
params <- list(siteQuota, r, overlapThreshold, overlapType)
labs <- list(paste0("sQ",seq(1,length(siteQuota),1)), paste0("r",seq(1,length(r),1)), paste0("oTh",seq(1,length(overlapThreshold),1)), paste0("oTy",seq(1,length(overlapType),1)))
vary <- apply(settings, 2, function(x) length(unique(x))) > 1
for (n in 1:length(params)) names(params[[n]]) <- labs[[n]]
x = 16
dataMat = dataList[[x]]
xy = b.xy
seeding = NULL
rarefaction = rarefaction
repSites = reps
nSites = settings[i,1]
oThreshold = settings[i,3]
oType = as.character(settings[i,4])
r = settings[i,2]
crs = b.crs
returnSeeds = F
output = 'full'
oPruningMode = "maxOccs"

## use biscuitsBatch to run all permutations
for(i in 1:length(output.vector)){
biscuitsBatch(dataList = eval(parse(text=data.strings[i])), siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, rarefaction = "sitesThenOccs",
             name.output = output.vector[i], n.cores = 4)
}


#### Code from biscuitsBatch for splitting by Taxa.
taxa = c("Brachiopoda","Bivalvia")
taxa.level = c("phylum","class")

## Partition each pack of cookies into different taxa
if(!is.null(taxa)){
  box <- lapply(1:length(taxa), function(t){
    part <- lapply(1:length(box), function(b){
      if(!any(is.na(box[[b]]))){
        pack <- lapply(1:length(box[[b]]), function(p){
          cookie <- box[[b]][[p]][which(box[[b]][[p]][,taxa.level[t]] %in% taxa[t]),]
        })
      } else {
        pack <- NA
      }
    })
  })
  names(box) <- taxa
}


#### Drop non-viable time bins from each dataset ####
#### Only relevant if seeking to do richness/diversification through time.
## Load function and static arguments
source("functions/drop.unusable.bins.R")
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
output.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
taxa <- T
threshold = threshold.VC = 1
prefix.vector <- c("stages_g200", "stages_g100","stages_s200","stages_s100")
out.pre.vector <- c("stages_g200_viaTimBin","stages_g100_viaTimBin","stages_s200_viaTimBin","stages_s100_viaTimBin")
data.strings <- c("stages.g200",
                  "stages.g100",
                  "stages.s200",
                  "stages.s100")

## Run function
for(i in 1:length(out.pre.vector)){
  drop.unusable.bins(input.dir = input.dir, input.pre = prefix.vector[i], output.dir = output.dir, output.pre = out.pre.vector[i],
                     vars = vars, sD = eval(parse(text=data.strings[i])), threshold = threshold.VC, taxa = T)
}
