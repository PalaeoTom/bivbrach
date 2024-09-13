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
genera <- readRDS("data/genera_200.Rds")
species <- readRDS("data/species_200.Rds")

genera_eco <- readRDS("data/genera_eco_200.Rds")
species_eco <- readRDS("data/species_eco_200.Rds")

genera_refRef <- readRDS("data/genera_RefRef_200.Rds")
species_refRef <- readRDS("data/species_RefRef_200.Rds")

## Isolate epifaunal
genera_epif <- genera_eco[which(genera_eco[,"ecological_cat"] == "epifaunal"),]
species_epif <- species_eco[which(species_eco[,"ecological_cat"] == "epifaunal"),]

#### Get stage time bins ####
# Function samples occurrences that fit within bins (won't include those that exist before or after)
#source("functions/extract.time.bin.R")

## First, derive Antell binning scheme
## Download raw stage data and create names column
#stages <- downloadTime('international ages')
#stages$name <- row.names(stages)

## Re-order stages by age, oldest first
#stages <- stages[order(stages$b_age, decreasing=TRUE), ]

## Create columns for new rounded ages
#stages$b_round <- stages$t_round <- 0

## Read in function for defining rounded stage ages
#source("functions/round.age.R")

## Define groupings for rounding - taken from Antell et al. (2020)
#groupings <- list(u10 <- which(stages$b_age < 10),
#                  u150 <- which(stages$b_age < 150 & stages$b_age > 10),
#                  old <- which(stages$b_age > 150))

## Round all to two digits
#for (group in 1:length(groupings)){
#  bins <- groupings[[group]]
#  digits <- c(3, 3, 3)[group]

#  # round all up (so no 0-0 bins, and no overlap)
#  for (i in bins){
#    b <- stages$b_age[i]
#    t <- stages$b_age[i+1]
#    stages$b_round[i] <- round.age(b, digits=digits, round_up=T)
#    stages$t_round[i] <- round.age(t, digits=digits, round_up=T)
#  }
#}

## Inspect time bins for overlap
## First bin is always fine
#checker <- matrix(NA, nrow = length(2:nrow(stages)), ncol = 2)
#checker[,1] <- stages[,"name"][-1]
#for(i in 2:nrow(stages)){
#  if(stages$t_round[i-1] == stages$b_round[i]){
#    checker[i-1,2] <- "equal to previous"
#  } else {
#    checker[i-1,2] <- "unequal"
#  }
#}
#View(checker)

## No overlap between bins. Can continue. First we explort
#write.csv(stages, file = "data/cleaned_stages.csv")

## Read in time binning scheme
stages <- read.csv("data/cleaned_stages.csv", row.names = 1)

source("functions/extract.stage.bin.R")
source("functions/extract.time.bin.R")
source("functions/get.bins.R")
source("functions/bin.data.R")

## Function uniqifies data by default
stages.g200 <- bin.data(occs = genera, trunc.stages = stages, complete.stages = stages, uniqify.data = F)
stages.s200 <- bin.data(occs = species, trunc.stages = stages, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "unique_name")

stages.g200.epif <- bin.data(occs = genera_epif, trunc.stages = stages, complete.stages = stages, uniqify.data = F)
stages.s200.epif <- bin.data(occs = species_epif, trunc.stages = stages, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "unique_name")

stages.g200.ref <- bin.data(occs = genera_refRef, trunc.stages = stages, complete.stages = stages, uniqify.data = F)
stages.s200.ref <- bin.data(occs = species_refRef, trunc.stages = stages, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "unique_name")

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
names(stages.s200) <- stage.midpoints
names(stages.g200.epif) <- stage.midpoints
names(stages.s200.epif) <- stage.midpoints
names(stages.g200.ref) <- stage.midpoints
names(stages.s200.ref) <- stage.midpoints

#### Derive covariate data - see Antell et al., 2020 ####
## Determine cell lithology, environment, reef status, and latitudinal centroid from combined dataset, paste on to subsets
source("functions/add.cell.covariate.R")

## cellY in data frame is latitudinal centroid
stages.g200 <- add.cell.covariate(stages.g200, name = "cellLith", ref = "occLith", values = c("carb", "sili"), threshold = 0.8, n.cores = 4)
stages.g200 <- add.cell.covariate(stages.g200, name = "cellEnv", ref = "occEnv", values = c("shal", "deep"), threshold = 0.8, n.cores = 4)
stages.g200 <- add.cell.covariate(stages.g200, name = "cellReef", ref = "occReef", values = c("reef", "noRf"), threshold = 0.8, n.cores = 4)

stages.s200 <- add.cell.covariate(stages.s200, name = "cellLith", ref = "occLith", values = c("carb", "sili"), threshold = 0.8, n.cores = 4)
stages.s200 <- add.cell.covariate(stages.s200, name = "cellEnv", ref = "occEnv", values = c("shal", "deep"), threshold = 0.8, n.cores = 4)
stages.s200 <- add.cell.covariate(stages.s200, name = "cellReef", ref = "occReef", values = c("reef", "noRf"), threshold = 0.8, n.cores = 4)

## Now to extract data for each grid cell and attach it to subsets
## Load function
source("functions/transfer.cell.covariate.R")

## Run the function
stages.g200.epif <- transfer.cell.covariate(source.data = stages.g200, export.data = stages.g200.epif, cov.cols = c("cellLith", "cellEnv", "cellReef"), cell.col = "cell", n.cores = 4)
stages.s200.epif <- transfer.cell.covariate(source.data = stages.s200, export.data = stages.s200.epif, cov.cols = c("cellLith", "cellEnv", "cellReef"), cell.col = "cell", n.cores = 4)

stages.g200.ref <- transfer.cell.covariate(source.data = stages.g200, export.data = stages.g200.ref, cov.cols = c("cellLith", "cellEnv", "cellReef"), cell.col = "cell", n.cores = 4)
stages.s200.ref <- transfer.cell.covariate(source.data = stages.s200, export.data = stages.s200.ref, cov.cols = c("cellLith", "cellEnv", "cellReef"), cell.col = "cell", n.cores = 4)

#### Standardise each time bin to ensure good data quality ####
source("functions/standardiseCells.R")
coll.min <- 10
ref.min <- 5
multiton.min <- 0.2
n.cores <- 4

## Standardise each time bin for collection number, reference number, and multiton ratio ##
stages.g200.s <- standardiseCells(stages.g200, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
stages.s200.s <- standardiseCells(stages.s200, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)

stages.g200.epif.s <- standardiseCells(stages.g200.epif, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
stages.s200.epif.s <- standardiseCells(stages.s200.epif, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)

stages.g200.ref.s <- standardiseCells(stages.g200.ref, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
stages.s200.ref.s <- standardiseCells(stages.s200.ref, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)

## Label cell-cell combination compositions
source("functions/add.reference.IDs.R")
source("functions/label.cell.reference.combo.comps.R")
stages.g200.s <- label.cell.reference.combo.comps(stages.g200.s, n.cores = 4)
stages.s200.s <- label.cell.reference.combo.comps(stages.s200.s, n.cores = 4)

stages.g200.epif.s <- label.cell.reference.combo.comps(stages.g200.epif.s, n.cores = 4)
stages.s200.epif.s <- label.cell.reference.combo.comps(stages.s200.epif.s, n.cores = 4)

stages.g200.ref.s <- label.cell.reference.combo.comps(stages.g200.ref.s, n.cores = 4)
stages.s200.ref.s <- label.cell.reference.combo.comps(stages.s200.ref.s, n.cores = 4)

## Export time binned data
saveRDS(stages.g200.s, file = "data/stages_g200.Rds")
saveRDS(stages.s200.s, file = "data/stages_s200.Rds")

saveRDS(stages.g200.epif.s, file = "data/stages_g200_epif.Rds")
saveRDS(stages.s200.epif.s, file = "data/stages_s200_epif.Rds")

saveRDS(stages.g200.ref.s, file = "data/stages_g200_ref.Rds")
saveRDS(stages.s200.ref.s, file = "data/stages_s200_ref.Rds")

## Note - there is a function that can prune grid cells with fewer than 3 references for bivalves and brachiopods
## Can be used
#source("functions/applyRefCollThresh.R")
