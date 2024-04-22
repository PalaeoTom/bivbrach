## 3. Spatial subsampling using modified divvy
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("divvy", "velociraptr", "dplyr", "plyr", "iNEXT", "parallel", "RColorBrewer", "plyr")
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
library(plyr)

## Load data
setwd("~/R_packages/R_projects/bivbrach")
bin10.g200 <- readRDS("data/bin10_g200.Rds")
bin10.g100 <- readRDS("data/bin10_g100.Rds")
bin10.g50 <- readRDS("data/bin10_g50.Rds")
bin10.g25 <- readRDS("data/bin10_g25.Rds")

bin10.s200 <- readRDS("data/bin10_s200.Rds")
bin10.s100 <- readRDS("data/bin10_s100.Rds")
bin10.s50 <- readRDS("data/bin10_s50.Rds")
bin10.s25 <- readRDS("data/bin10_s25.Rds")

stages.g200 <- readRDS("data/stages_g200.Rds")
stages.g100 <- readRDS("data/stages_g100.Rds")
stages.g50 <- readRDS("data/stages_g50.Rds")
stages.g25 <- readRDS("data/stages_g25.Rds")

stages.s200 <- readRDS("data/stages_s200.Rds")
stages.s100 <- readRDS("data/stages_s100.Rds")
stages.s50 <- readRDS("data/stages_s50.Rds")
stages.s25 <- readRDS("data/stages_s25.Rds")

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
radii <- as.integer(c(100000, 200000, 250000, 500000))
siteQuotas <- c(3, 6, 9, 12, 15)
overlapThresholds <- c(0, 0.25, 0.5, 0.75, 1)
overlapTypes <- c("area", "sites")
weightStandardisation_1 <- F

#### Generate spatial subsamples ####
## Generate output vectors
output.vector <- c("bin10_g200_raw",
                   "bin10_g100_raw",
                   "bin10_g50_raw",
                   "bin10_g25_raw",
                   "bin10_s200_raw",
                   "bin10_s100_raw",
                   "bin10_s50_raw",
                   "bin10_s25_raw",
                   "stages_g200_raw",
                   "stages_g100_raw",
                   "stages_g50_raw",
                   "stages_g25_raw",
                   "stages_s200_raw",
                   "stages_s100_raw",
                   "stages_s50_raw",
                   "stages_s25_raw")

data.strings <- c("bin10.g200",
                  "bin10.g100",
                  "bin10.g50",
                  "bin10.g25",
                  "bin10.s200",
                  "bin10.s100",
                  "bin10.s50",
                  "bin10.s25",
                  "stages.g200",
                  "stages.g100",
                  "stages.g50",
                  "stages.g25",
                  "stages.s200",
                  "stages.s100",
                  "stages.s50",
                  "stages.s25")

## Not standardised for occupancy (will standardise using SQS)
for(i in 1:length(output.vector)){
cut.biscuits(data = eval(parse(text=data.strings[i])), siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, standardiseSiteNumber = F, weightedStandardisation = weightStandardisation_1,
             taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), name.output = output.vector[i], n.cores = 4)
}

## Standardised for occupancy (weighted and unweighted) <- this needs running on a cluster
## Try subsampling with and without weighting of cells
#weightStandardisation_2 <- c(T,F)
#output.vector <- c("bin10_g200_stan",
#                   "bin10_g100_stan",
#                   "bin10_g50_stan",
#                   "bin10_g25_stan",
#                   "bin10_s200_stan",
#                   "bin10_s100_stan",
#                   "bin10_s50_stan",
#                   "bin10_s25_stan",
#                   "stages_g200_stan",
#                   "stages_g100_stan",
#                   "stages_g50_stan",
#                  "stages_g25_stan",
#                  "stages_s200_stan",
#                  "stages_s100_stan",
#                  "stages_s50_stan",
#                  "stages_s25_stan")

#for(i in 1:length(output.vector)){
#cut.biscuits(data = eval(parse(text=data.strings[i])), siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/st_spaSub",
#             overlapThreshold = overlapThresholds, overlapType = overlapTypes, standardiseSiteNumber = F, weightedStandardisation = weightStandardisation_2,
#             taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), name.output = output.vector.stan[i], n.cores = 4)
#}

#### Step 1 - get performance data on different settings. Number of viable spatial subsamples under all configurations for all datasets through time ####
source("functions/count.viable.samples.R")
dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)), paste0("oTh",seq(1,length(overlapThresholds),1)), paste0("oTy",seq(1,length(overlapTypes),1)))
cores <- 4
taxa.split <- T
prefix.vector <- output.vector <- c("bin10_g200_raw","bin10_g100_raw","bin10_g50_raw","bin10_g25_raw","bin10_s200_raw","bin10_s100_raw","bin10_s50_raw","bin10_s25_raw",
                                    "stages_g200_raw", "stages_g100_raw","stages_g50_raw","stages_g25_raw","stages_s200_raw","stages_s100_raw","stages_s50_raw","stages_s25_raw")
for(i in 1:length(prefix.vector)){
  count.viable.samples(dir = dir, pre = prefix.vector[i], vars = vars, sD = eval(parse(text=data.strings[i])), n.cores = cores, taxa = taxa.split, output.dir = "~/R_packages/R_projects/bivbrach/data", output.name = prefix.vector[i])
}

## read in result
bin10.g200.VCs <- read.csv("data/bin10_g200_raw_viable_subsamples.csv", header = T, row.names = 1)
bin10.g100.VCs <- read.csv("data/bin10_g100_raw_viable_subsamples.csv", header = T, row.names = 1)
bin10.g50.VCs <- read.csv("data/bin10_g50_raw_viable_subsamples.csv", header = T, row.names = 1)
bin10.g25.VCs <- read.csv("data/bin10_g25_raw_viable_subsamples.csv", header = T, row.names = 1)

bin10.s200.VCs <- read.csv("data/bin10_s200_raw_viable_subsamples.csv", header = T, row.names = 1)
bin10.s100.VCs <- read.csv("data/bin10_s100_raw_viable_subsamples.csv", header = T, row.names = 1)
bin10.s50.VCs <- read.csv("data/bin10_s50_raw_viable_subsamples.csv", header = T, row.names = 1)
bin10.s25.VCs <- read.csv("data/bin10_s25_raw_viable_subsamples.csv", header = T, row.names = 1)

stages.g200.VCs <- read.csv("data/stages_g200_raw_viable_subsamples.csv", header = T, row.names = 1)
stages.g100.VCs <- read.csv("data/stages_g100_raw_viable_subsamples.csv", header = T, row.names = 1)
stages.g50.VCs <- read.csv("data/stages_g50_raw_viable_subsamples.csv", header = T, row.names = 1)
stages.g25.VCs <- read.csv("data/stages_g25_raw_viable_subsamples.csv", header = T, row.names = 1)

stages.s200.VCs <- read.csv("data/stages_s200_raw_viable_subsamples.csv", header = T, row.names = 1)
stages.s100.VCs <- read.csv("data/stages_s100_raw_viable_subsamples.csv", header = T, row.names = 1)
stages.s50.VCs <- read.csv("data/stages_s50_raw_viable_subsamples.csv", header = T, row.names = 1)
stages.s25.VCs <- read.csv("data/stages_s25_raw_viable_subsamples.csv", header = T, row.names = 1)

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
colnames(stages.g200.VCs) <- rownames(stages_trunc)[-65]
colnames(stages.g100.VCs) <- rownames(stages_trunc)[-65]
colnames(stages.g50.VCs) <- rownames(stages_trunc)[-65]
colnames(stages.g25.VCs) <- rownames(stages_trunc)[-65]

colnames(stages.s200.VCs) <- rownames(stages_trunc)[-65]
colnames(stages.s100.VCs) <- rownames(stages_trunc)[-65]
colnames(stages.s50.VCs) <- rownames(stages_trunc)[-65]
colnames(stages.s25.VCs) <- rownames(stages_trunc)[-65]

## convert to number of time bins exceeded x samples
source("functions/countUsable.R")
source("functions/wrap.countUsable.R")

## get grid of variable combination
varGrid <- expand.grid(siteQuotas, radii/1000, overlapThresholds, overlapTypes, weightStandardisation_1)
threshold.VC <- 15

## count usable bins and format
bin10.g200.UTBs <- wrap.countUsable(VC = bin10.g200.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))
bin10.g100.UTBs <- wrap.countUsable(VC = bin10.g100.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))
bin10.g50.UTBs <- wrap.countUsable(VC = bin10.g50.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))
bin10.g25.UTBs <- wrap.countUsable(VC = bin10.g25.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))

bin10.s200.UTBs <- wrap.countUsable(VC = bin10.s200.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))
bin10.s100.UTBs <- wrap.countUsable(VC = bin10.s100.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))
bin10.s50.UTBs <- wrap.countUsable(VC = bin10.s50.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))
bin10.s25.UTBs <- wrap.countUsable(VC = bin10.s25.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))

stages.g200.UTBs <- wrap.countUsable(VC = stages.g200.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))
stages.g100.UTBs <- wrap.countUsable(VC = stages.g100.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))
stages.g50.UTBs <- wrap.countUsable(VC = stages.g50.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))
stages.g25.UTBs <- wrap.countUsable(VC = stages.g25.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))

stages.s200.UTBs <- wrap.countUsable(VC = stages.s200.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))
stages.s100.UTBs <- wrap.countUsable(VC = stages.s100.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))
stages.s50.UTBs <- wrap.countUsable(VC = stages.s50.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))
stages.s25.UTBs <- wrap.countUsable(VC = stages.s25.VCs, threshold.VC, varGrid, c("siteQuota", "radius", "overlapThreshold", "overlapType", "weightedStandardisation"))

## get combination of siteQuotas and radii
split.vars <- expand.grid(radii/1000, siteQuotas)
colnames(split.vars) <- c("radius", "siteQuota")

## get plotting data
source("functions/get.VC.plot.data.R")
bin10.g200.barData <- get.VC.plot.data(UTB = bin10.g200.UTBs, split.vars = split.vars)
bin10.g100.barData <- get.VC.plot.data(UTB = bin10.g100.UTBs, split.vars = split.vars)
bin10.g50.barData <- get.VC.plot.data(UTB = bin10.g50.UTBs, split.vars = split.vars)
bin10.g25.barData <- get.VC.plot.data(UTB = bin10.g25.UTBs, split.vars = split.vars)

bin10.s200.barData <- get.VC.plot.data(UTB = bin10.s200.UTBs, split.vars = split.vars)
bin10.s100.barData <- get.VC.plot.data(UTB = bin10.s100.UTBs, split.vars = split.vars)
bin10.s50.barData <- get.VC.plot.data(UTB = bin10.s50.UTBs, split.vars = split.vars)
bin10.s25.barData <- get.VC.plot.data(UTB = bin10.s25.UTBs, split.vars = split.vars)

stages.g200.barData <- get.VC.plot.data(UTB = stages.g200.UTBs, split.vars = split.vars)
stages.g100.barData <- get.VC.plot.data(UTB = stages.g100.UTBs, split.vars = split.vars)
stages.g50.barData <- get.VC.plot.data(UTB = stages.g50.UTBs, split.vars = split.vars)
stages.g25.barData <- get.VC.plot.data(UTB = stages.g25.UTBs, split.vars = split.vars)

stages.s200.barData <- get.VC.plot.data(UTB = stages.s200.UTBs, split.vars = split.vars)
stages.s100.barData <- get.VC.plot.data(UTB = stages.s100.UTBs, split.vars = split.vars)
stages.s50.barData <- get.VC.plot.data(UTB = stages.s50.UTBs, split.vars = split.vars)
stages.s25.barData <- get.VC.plot.data(UTB = stages.s25.UTBs, split.vars = split.vars)

## set output directory and read in plotting function
output.dir <- "/Users/tjs/R_packages/R_projects/bivbrach/figures"
source("functions/plot.UTBs.R")

## Plot figures - add labels in post
## Set input strings and output names
stages.output.name <- c("stages_g200", "stages_g100", "stages_g50", "stages_g25",
                        "stages_s200", "stages_s100", "stages_s50", "stages_s25")
bin10.output.name <- c("bin10_g200", "bin10_g100", "bin10_g50", "bin10_g25",
                        "bin10_s200", "bin10_s100", "bin10_s50", "bin10_s25")
stages.input.strings <- c("stages.g200.barData", "stages.g100.barData", "stages.g50.barData", "stages.g25.barData",
                          "stages.s200.barData", "stages.s100.barData", "stages.s50.barData", "stages.s25.barData")
bin10.input.strings <- c("bin10.g200.barData", "bin10.g100.barData", "bin10.g50.barData", "bin10.g25.barData",
                          "bin10.s200.barData", "bin10.s100.barData", "bin10.s50.barData", "bin10.s25.barData")

## Plot stages data
for(i in 1:length(stages.input.strings)){
  plot.UTBs(data = eval(parse(text=stages.input.strings[i])), output.dir = output.dir, output.name = stages.output.name[i], xlab.inc = 10)
}

## Plot bin10 data
for(i in 1:length(bin10.input.strings)){
  plot.UTBs(data = eval(parse(text=bin10.input.strings[i])), output.dir = output.dir, output.name = bin10.output.name[i])
}


#### Carry on working from here!!! ####



#### Step 2 - drop non-viable time bins from each dataset ####
## Load function and static arguments
source("functions/drop.unusable.bins.R")
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
output.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)), paste0("oTh",seq(1,length(overlapThresholds),1)), paste0("oTy",seq(1,length(overlapTypes),1)))
taxa <- T
threshold = threshold.VC = 15

## Run functions
drop.unusable.bins(input.dir = input.dir, input.pre = "BB_gen_stag_raw", output.dir = output.dir, output.pre = "BB_gen_stag_raw_viableTimeBins",
                   vars = vars, sD = genera.stages, threshold = threshold.VC, taxa = T)
drop.unusable.bins(input.dir = input.dir, input.pre = "BB_gen_10ma_raw", output.dir = output.dir, output.pre = "BB_gen_10ma_raw_viableTimeBins",
                   vars = vars, sD = genera.10ma, threshold = threshold.VC, taxa = T)
drop.unusable.bins(input.dir = input.dir, input.pre = "BB_spec_stag_raw", output.dir = output.dir, output.pre = "BB_spec_stag_raw_viableTimeBins",
                   vars = vars, sD = species.stages, threshold = threshold.VC, taxa = T)
drop.unusable.bins(input.dir = input.dir, input.pre = "BB_spec_10ma_raw", output.dir = output.dir, output.pre = "BB_spec_10ma_raw_viableTimeBins",
                   vars = vars, sD = species.10ma, threshold = threshold.VC, taxa = T)
