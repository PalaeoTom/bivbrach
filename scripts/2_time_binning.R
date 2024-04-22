## 2. Time binning
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("velociraptr", "dplyr", "plyr", "parallel")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(velociraptr)
library(dplyr)
library(plyr)
library(parallel)

## Load data
setwd("~/R_packages/R_projects/bivbrach")
genera_200 <- readRDS("data/genera_200.Rds")
species_200 <- readRDS("data/species_200.Rds")
genera_100 <- readRDS("data/genera_100.Rds")
species_100 <- readRDS("data/species_100.Rds")
genera_50 <- readRDS("data/genera_50.Rds")
species_50 <- readRDS("data/species_50.Rds")
genera_25 <- readRDS("data/genera_25.Rds")
species_25 <- readRDS("data/species_25.Rds")

#### Get stage time bins ####
# Function samples occurrences that fit within bins (won't include those that exist before or after)
source("functions/extract.time.bin.R")

## First, derive Antell binning scheme
## Download raw stage data and create names column
stages <- downloadTime('international ages')
stages$name <- row.names(stages)

## Re-order stages by age, oldest first
stages <- stages[order(stages$b_age, decreasing=TRUE), ]

## Create columns for new rounded ages
stages$b_round <- stages$t_round <- 0

## Define stages to omit (for various reasons)
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

## Create stages object with listed stages omitted
stages_trunc <- stages[!(stages$name %in% stages2omit),] #remove lumped stages

## Read in function for defining rounded stage ages
source("functions/round.age.R")

## Define groupings for rounding
groupings <- list(u10 <- which(stages_trunc$b_age < 10),
                  u150 <- which(stages_trunc$b_age < 150 & stages_trunc$b_age > 10),
                  old <- which(stages_trunc$b_age > 150))

## Round ages
for (group in 1:length(groupings)){
  bins <- groupings[[group]]
  digits <- c(2, 1, 0)[group]

  # round down younger boundary (terminus) and up older boundary (beginning) per stage
  for (i in bins){
    b <- stages_trunc$b_age[i]
    t <- stages_trunc$b_age[i+1]
    stages_trunc$b_round[i] <- round.age(b, digits=digits, round_up=TRUE)
    stages_trunc$t_round[i] <- round.age(t, digits=digits, round_up=FALSE)
  }
}

source("functions/extract.stage.bin.R")
source("functions/extract.time.bin.R")
source("functions/get.bins.R")
source("functions/bin.data.R")

## Function uniqifies data by default
stages.g200 <- bin.data(occs = genera_200, trunc.stages = stages_trunc, complete.stages = stages, uniqify.data = F)
stages.s200 <- bin.data(occs = species_200, trunc.stages = stages_trunc, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "unique_name")
stages.g100 <- bin.data(occs = genera_100, trunc.stages = stages_trunc, complete.stages = stages, uniqify.data = F)
stages.s100 <- bin.data(occs = species_100, trunc.stages = stages_trunc, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "unique_name")
stages.g50 <- bin.data(occs = genera_50, trunc.stages = stages_trunc, complete.stages = stages, uniqify.data = F)
stages.s50 <- bin.data(occs = species_50, trunc.stages = stages_trunc, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "unique_name")
stages.g25 <- bin.data(occs = genera_25, trunc.stages = stages_trunc, complete.stages = stages, uniqify.data = F)
stages.s25 <- bin.data(occs = species_25, trunc.stages = stages_trunc, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "unique_name")

#### Get 10ma time bins ####
## Get min/max for datasets
source("functions/get.min.max.R")
g200.mm <- get.min.max(data = genera_200)
g100.mm <- get.min.max(data = genera_100)
g50.mm <- get.min.max(data = genera_50)
g25.mm <- get.min.max(data = genera_25)

s200.mm <- get.min.max(data = species_200)
s100.mm <- get.min.max(data = species_100)
s50.mm <- get.min.max(data = species_50)
s25.mm <- get.min.max(data = species_25)

## Get 10Ma time bins
bin10.g200 <- bin.data(occs = genera_200, max_time = g200.mm[1], min_time = g200.mm[2], bin_size = 10, uniqify.data = F)
bin10.g100 <- bin.data(occs = genera_100, max_time = g100.mm[1], min_time = g100.mm[2], bin_size = 10, uniqify.data = F)
bin10.g50 <- bin.data(occs = genera_50, max_time = g50.mm[1], min_time = g50.mm[2], bin_size = 10, uniqify.data = F)
bin10.g25 <- bin.data(occs = genera_25, max_time = g25.mm[1], min_time = g25.mm[2], bin_size = 10, uniqify.data = F)

bin10.s200 <- bin.data(occs = species_200, max_time = s200.mm[1], min_time = s200.mm[2], bin_size = 10, uniqify.data = F)
bin10.s100 <- bin.data(occs = species_100, max_time = s100.mm[1], min_time = s100.mm[2], bin_size = 10, uniqify.data = F)
bin10.s50 <- bin.data(occs = species_50, max_time = s50.mm[1], min_time = s50.mm[2], bin_size = 10, uniqify.data = F)
bin10.s25 <- bin.data(occs = species_25, max_time = s25.mm[1], min_time = s25.mm[2], bin_size = 10, uniqify.data = F)

#### Label time bins ####
## Get midpoints for each time bin to use as labels
source("functions/get.midpoints.R")

## Truncated stages vector
stage.times <- stages_trunc[,c("t_round","b_round")]

## last entry has an NA. Change to 0
stage.times[66,1] <- 0

## get midpoints
stage.midpoints <- get.midpoints(stage.times[-66,])

## different time bins for genera and species
genera.bin.times <- t(data.frame(get.bins(g200.mm[1], g200.mm[2], 10)))
species.bin.times <- t(data.frame(get.bins(s200.mm[1], s200.mm[2], 10)))

## get midpoints
genera.bin.midpoints <- get.midpoints(genera.bin.times)
species.bin.midpoints <- get.midpoints(species.bin.times)

## Label time bins
names(stages.g200) <- stage.midpoints
names(stages.g100) <- stage.midpoints
names(stages.g50) <- stage.midpoints
names(stages.g25) <- stage.midpoints
names(stages.s200) <- stage.midpoints
names(stages.s100) <- stage.midpoints
names(stages.s50) <- stage.midpoints
names(stages.s25) <- stage.midpoints

names(bin10.g200) <- genera.bin.midpoints
names(bin10.g100) <- genera.bin.midpoints
names(bin10.g50) <- genera.bin.midpoints
names(bin10.g25) <- genera.bin.midpoints

names(bin10.s200) <- species.bin.midpoints
names(bin10.s100) <- species.bin.midpoints
names(bin10.s50) <- species.bin.midpoints
names(bin10.s25) <- species.bin.midpoints

#### Standardise each time bin to ensure good data quality ####
source("functions/standardiseCells.R")
coll.min <- 10
ref.min <- 5
multiton.min <- 0.3
n.cores <- 4

## Standardise each time bin for collection number, reference number, and multiton ratio ##
bin10.g200.s <- standardiseCells(bin10.g200, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
bin10.g100.s <- standardiseCells(bin10.g100, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
bin10.g50.s <- standardiseCells(bin10.g50, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
bin10.g25.s <- standardiseCells(bin10.g25, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)

bin10.s200.s <- standardiseCells(bin10.s200, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)
bin10.s100.s <- standardiseCells(bin10.s100, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)
bin10.s50.s <- standardiseCells(bin10.s50, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)
bin10.s25.s <- standardiseCells(bin10.s25, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)

stages.g200.s <- standardiseCells(stages.g200, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
stages.g100.s <- standardiseCells(stages.g100, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
stages.g50.s <- standardiseCells(stages.g50, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
stages.g25.s <- standardiseCells(stages.g25, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)

stages.s200.s <- standardiseCells(stages.s200, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)
stages.s100.s <- standardiseCells(stages.s100, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)
stages.s50.s <- standardiseCells(stages.s50, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)
stages.s25.s <- standardiseCells(stages.s25, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)

## Export time binned data
saveRDS(bin10.g200.s, file = "data/bin10_g200.Rds")
saveRDS(bin10.g100.s, file = "data/bin10_g100.Rds")
saveRDS(bin10.g50.s, file = "data/bin10_g50.Rds")
saveRDS(bin10.g25.s, file = "data/bin10_g25.Rds")

saveRDS(bin10.s200.s, file = "data/bin10_s200.Rds")
saveRDS(bin10.s100.s, file = "data/bin10_s100.Rds")
saveRDS(bin10.s50.s, file = "data/bin10_s50.Rds")
saveRDS(bin10.s25.s, file = "data/bin10_s25.Rds")

saveRDS(stages.g200.s, file = "data/stages_g200.Rds")
saveRDS(stages.g100.s, file = "data/stages_g100.Rds")
saveRDS(stages.g50.s, file = "data/stages_g50.Rds")
saveRDS(stages.g25.s, file = "data/stages_g25.Rds")

saveRDS(stages.s200.s, file = "data/stages_s200.Rds")
saveRDS(stages.s100.s, file = "data/stages_s100.Rds")
saveRDS(stages.s50.s, file = "data/stages_s50.Rds")
saveRDS(stages.s25.s, file = "data/stages_s25.Rds")

