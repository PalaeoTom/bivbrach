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
setwd("~/R_packages/bivbrach")
genera_200 <- readRDS("data/genera_200.Rds")
species_200 <- readRDS("data/species_200.Rds")
genera_100 <- readRDS("data/genera_100.Rds")
species_100 <- readRDS("data/species_100.Rds")

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

## Read in function for defining rounded stage ages
source("functions/round.age.R")

## Define groupings for rounding - taken from Antell et al. (2020)
groupings <- list(u10 <- which(stages$b_age < 10),
                  u150 <- which(stages$b_age < 150 & stages$b_age > 10),
                  old <- which(stages$b_age > 150))

## Round all to two digits
for (group in 1:length(groupings)){
  bins <- groupings[[group]]
  digits <- c(3, 3, 3)[group]

  # round all up (so no 0-0 bins, and no overlap)
  for (i in bins){
    b <- stages$b_age[i]
    t <- stages$b_age[i+1]
    stages$b_round[i] <- round.age(b, digits=digits, round_up=T)
    stages$t_round[i] <- round.age(t, digits=digits, round_up=T)
  }
}

## Inspect time bins for overlap
## First bin is always fine
checker <- matrix(NA, nrow = length(2:nrow(stages)), ncol = 2)
checker[,1] <- stages[,"name"][-1]
for(i in 2:nrow(stages)){
  if(stages$t_round[i-1] == stages$b_round[i]){
    checker[i-1,2] <- "equal to previous"
  } else {
    checker[i-1,2] <- "unequal"
  }
}
View(checker)

## No overlap between bins. Can continue. First we explort
write.csv(stages, file = "data/cleaned_stages.csv")

source("functions/extract.stage.bin.R")
source("functions/extract.time.bin.R")
source("functions/get.bins.R")
source("functions/bin.data.R")

## Function uniqifies data by default
stages.g200 <- bin.data(occs = genera_200, trunc.stages = stages, complete.stages = stages, uniqify.data = F)
stages.s200 <- bin.data(occs = species_200, trunc.stages = stages, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "unique_name")
stages.g100 <- bin.data(occs = genera_100, trunc.stages = stages, complete.stages = stages, uniqify.data = F)
stages.s100 <- bin.data(occs = species_100, trunc.stages = stages, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "unique_name")

#### Label time bins ####
## Get midpoints for each time bin to use as labels
source("functions/get.midpoints.R")

## Truncated stages vector
stage.times <- stages[,c("t_round","b_round")]

## last entry has an NA. Change to 0
stage.times[102,1] <- 0

## get midpoints
stage.midpoints <- get.midpoints(stage.times[-102,])

## Label time bins
names(stages.g200) <- stage.midpoints
names(stages.g100) <- stage.midpoints
names(stages.s200) <- stage.midpoints
names(stages.s100) <- stage.midpoints

#### Standardise each time bin to ensure good data quality ####
source("functions/standardiseCells.R")
coll.min <- 10
ref.min <- 5
multiton.min <- 0.2
n.cores <- 4

## Standardise each time bin for collection number, reference number, and multiton ratio ##
stages.g200.s <- standardiseCells(stages.g200, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
stages.g100.s <- standardiseCells(stages.g100, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)

stages.s200.s <- standardiseCells(stages.s200, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)
stages.s100.s <- standardiseCells(stages.s100, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)

## Export time binned data
saveRDS(stages.g200.s, file = "data/stages_g200.Rds")
saveRDS(stages.g100.s, file = "data/stages_g100.Rds")

saveRDS(stages.s200.s, file = "data/stages_s200.Rds")
saveRDS(stages.s100.s, file = "data/stages_s100.Rds")

