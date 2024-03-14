## 2. Initial exploratory analysis
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("divvy", "velociraptr", "dplyr", "plyr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divvy)
library(velociraptr)
library(dplyr)
library(plyr)

## Load data
setwd("~/R_packages/R_projects/bivbrach")
genera <- readRDS("data/PBDB_BB_genera.Rds")
species <- readRDS("data/PBDB_BB_species.Rds")

#### Get time bins ####
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

## Not uniqifying by default
stages.genera <- bin.data(occs = genera, trunc.stages = stages_trunc, complete.stages = stages, uniqify.data = F)
stages.species <- bin.data(occs = species, trunc.stages = stages_trunc, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "species")

## Get min/max for datasets
source("functions/get.min.max.R")
genera.mm <- get.min.max(data = genera)
species.mm <- get.min.max(data = species)

## Get 10Ma time bins
bin10.genera <- bin.data(occs = genera, max_time = genera.mm[1], min_time = genera.mm[2], bin_size = 10, uniqify.data = F)
bin10.species <- bin.data(occs = species, max_time = species.mm[1], min_time = species.mm[2], bin_size = 10, uniqify.data = F, uniqify.taxVar = "species")

#### Correct for spatial sampling biases ####
source("functions/findPool2.R")
source("functions/findSeeds2.R")
source("functions/getOverlap.R")
source("functions/cookie.R")
source("functions/biscuits.R")
source("functions/cut.biscuits.R")

#### Correct for each time bin
stages.gen.correct <- cut.biscuits(data = stages.genera,
                              biscuitThreshold = 0.5,
                              reps = 10, siteQuota = 3, r = 1000, biscuitWeight = F,
                              b.crs = 'EPSG:8857', taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))

stages.spec.correct <- cut.biscuits(data = stages.species,
                                   biscuitThreshold = 0.5,
                                   reps = 10, siteQuota = 3, r = 1000, biscuitWeight = F,
                                   b.crs = 'EPSG:8857', taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))

bin10.gen.correct <- cut.biscuits(data = bin10.genera,
                                   biscuitThreshold = 0.5,
                                   reps = 10, siteQuota = 3, r = 1000, biscuitWeight = F,
                                   b.crs = 'EPSG:8857', taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))

bin10.spec.correct <- cut.biscuits(data = bin10.species,
                                    biscuitThreshold = 0.5,
                                    reps = 10, siteQuota = 3, r = 1000, biscuitWeight = F,
                                    b.crs = 'EPSG:8857', taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))

#### Use SQS to derive diversity estimate for each rep ####
## Test impact of double rarefaction ##




