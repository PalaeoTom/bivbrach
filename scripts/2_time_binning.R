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

genera_200_eco <- readRDS("data/genera_eco_200.Rds")
genera_100_eco <- readRDS("data/genera_eco_100.Rds")
species_200_eco <- readRDS("data/species_eco_200.Rds")
species_100_eco <- readRDS("data/species_eco_100.Rds")

## Split eco datasets into infaunal and epifaunal
genera_200_epif <- genera_200_eco[which(genera_200_eco[,"ecological_cat"] == "epifaunal"),]
genera_100_epif <- genera_100_eco[which(genera_100_eco[,"ecological_cat"] == "epifaunal"),]
species_200_epif <- species_200_eco[which(species_200_eco[,"ecological_cat"] == "epifaunal"),]
species_100_epif <- species_100_eco[which(species_100_eco[,"ecological_cat"] == "epifaunal"),]

genera_200_inf <- genera_200_eco[which(genera_200_eco[,"ecological_cat"] == "infaunal"),]
genera_100_inf <- genera_100_eco[which(genera_100_eco[,"ecological_cat"] == "infaunal"),]
species_200_inf <- species_200_eco[which(species_200_eco[,"ecological_cat"] == "infaunal"),]
species_100_inf <- species_100_eco[which(species_100_eco[,"ecological_cat"] == "infaunal"),]

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
stages.g200 <- bin.data(occs = genera_200, trunc.stages = stages, complete.stages = stages, uniqify.data = F)
stages.s200 <- bin.data(occs = species_200, trunc.stages = stages, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "unique_name")
stages.g100 <- bin.data(occs = genera_100, trunc.stages = stages, complete.stages = stages, uniqify.data = F)
stages.s100 <- bin.data(occs = species_100, trunc.stages = stages, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "unique_name")

stages.g200.epif <- bin.data(occs = genera_200_epif, trunc.stages = stages, complete.stages = stages, uniqify.data = F)
stages.s200.epif <- bin.data(occs = species_200_epif, trunc.stages = stages, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "unique_name")
stages.g100.epif <- bin.data(occs = genera_100_epif, trunc.stages = stages, complete.stages = stages, uniqify.data = F)
stages.s100.epif <- bin.data(occs = species_100_epif, trunc.stages = stages, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "unique_name")

stages.g200.inf <- bin.data(occs = genera_200_inf, trunc.stages = stages, complete.stages = stages, uniqify.data = F)
stages.s200.inf <- bin.data(occs = species_200_inf, trunc.stages = stages, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "unique_name")
stages.g100.inf <- bin.data(occs = genera_100_inf, trunc.stages = stages, complete.stages = stages, uniqify.data = F)
stages.s100.inf <- bin.data(occs = species_100_inf, trunc.stages = stages, complete.stages = stages, uniqify.data = F, uniqify.taxVar = "unique_name")

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
names(stages.g200.epif) <- stage.midpoints
names(stages.g100.epif) <- stage.midpoints
names(stages.s200.epif) <- stage.midpoints
names(stages.s100.epif) <- stage.midpoints
names(stages.g200.inf) <- stage.midpoints
names(stages.g100.inf) <- stage.midpoints
names(stages.s200.inf) <- stage.midpoints
names(stages.s100.inf) <- stage.midpoints

#### Derive covariate data - see Antell et al., 2020 ####
## Determine cell lithology, environment, reef status, and latitudinal centroid from combined dataset, paste on to subsets
source("functions/add.cell.covariate.R")

## cellY in data frame is latitudinal centroid
stages.g200 <- add.cell.covariate(stages.g200, name = "cellLith", ref = "occLith", values = c("carb", "sili"), threshold = 0.8, n.cores = 4)
stages.g200 <- add.cell.covariate(stages.g200, name = "cellEnv", ref = "occEnv", values = c("shal", "deep"), threshold = 0.8, n.cores = 4)
stages.g200 <- add.cell.covariate(stages.g200, name = "cellReef", ref = "occReef", values = c("reef", "noRf"), threshold = 0.8, n.cores = 4)

stages.g100 <- add.cell.covariate(stages.g100, name = "cellLith", ref = "occLith", values = c("carb", "sili"), threshold = 0.8, n.cores = 4)
stages.g100 <- add.cell.covariate(stages.g100, name = "cellEnv", ref = "occEnv", values = c("shal", "deep"), threshold = 0.8, n.cores = 4)
stages.g100 <- add.cell.covariate(stages.g100, name = "cellReef", ref = "occReef", values = c("reef", "noRf"), threshold = 0.8, n.cores = 4)

stages.s200 <- add.cell.covariate(stages.s200, name = "cellLith", ref = "occLith", values = c("carb", "sili"), threshold = 0.8, n.cores = 4)
stages.s200 <- add.cell.covariate(stages.s200, name = "cellEnv", ref = "occEnv", values = c("shal", "deep"), threshold = 0.8, n.cores = 4)
stages.s200 <- add.cell.covariate(stages.s200, name = "cellReef", ref = "occReef", values = c("reef", "noRf"), threshold = 0.8, n.cores = 4)

stages.s100 <- add.cell.covariate(stages.s100, name = "cellLith", ref = "occLith", values = c("carb", "sili"), threshold = 0.8, n.cores = 4)
stages.s100 <- add.cell.covariate(stages.s100, name = "cellEnv", ref = "occEnv", values = c("shal", "deep"), threshold = 0.8, n.cores = 4)
stages.s100 <- add.cell.covariate(stages.s100, name = "cellReef", ref = "occReef", values = c("reef", "noRf"), threshold = 0.8, n.cores = 4)

## Now to extract data for each grid cell and attach it to subsets
## Load function
source("functions/transfer.cell.covariate.R")

## Run the function
stages.g200.epif <- transfer.cell.covariate(source.data = stages.g200, export.data = stages.g200.epif, cov.cols = c("cellLith", "cellEnv", "cellReef"), cell.col = "cell", n.cores = 4)
stages.g200.inf <- transfer.cell.covariate(source.data = stages.g200, export.data = stages.g200.inf, cov.cols = c("cellLith", "cellEnv", "cellReef"), cell.col = "cell", n.cores = 4)

stages.g100.epif <- transfer.cell.covariate(source.data = stages.g100, export.data = stages.g100.epif, cov.cols = c("cellLith", "cellEnv", "cellReef"), cell.col = "cell", n.cores = 4)
stages.g100.inf <- transfer.cell.covariate(source.data = stages.g100, export.data = stages.g100.inf, cov.cols = c("cellLith", "cellEnv", "cellReef"), cell.col = "cell", n.cores = 4)

stages.s200.epif <- transfer.cell.covariate(source.data = stages.s200, export.data = stages.s200.epif, cov.cols = c("cellLith", "cellEnv", "cellReef"), cell.col = "cell", n.cores = 4)
stages.s200.inf <- transfer.cell.covariate(source.data = stages.s200, export.data = stages.s200.inf, cov.cols = c("cellLith", "cellEnv", "cellReef"), cell.col = "cell", n.cores = 4)

stages.s100.epif <- transfer.cell.covariate(source.data = stages.s100, export.data = stages.s100.epif, cov.cols = c("cellLith", "cellEnv", "cellReef"), cell.col = "cell", n.cores = 4)
stages.s100.inf <- transfer.cell.covariate(source.data = stages.s100, export.data = stages.s100.inf, cov.cols = c("cellLith", "cellEnv", "cellReef"), cell.col = "cell", n.cores = 4)

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

stages.g200.epif.s <- standardiseCells(stages.g200.epif, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
stages.g100.epif.s <- standardiseCells(stages.g100.epif, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
stages.s200.epif.s <- standardiseCells(stages.s200.epif, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)
stages.s100.epif.s <- standardiseCells(stages.s100.epif, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)

stages.g200.inf.s <- standardiseCells(stages.g200.inf, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
stages.g100.inf.s <- standardiseCells(stages.g100.inf, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
stages.s200.inf.s <- standardiseCells(stages.s200.inf, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)
stages.s100.inf.s <- standardiseCells(stages.s100.inf, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)

## Export time binned data
saveRDS(stages.g200.s, file = "data/stages_g200.Rds")
saveRDS(stages.g100.s, file = "data/stages_g100.Rds")
saveRDS(stages.s200.s, file = "data/stages_s200.Rds")
saveRDS(stages.s100.s, file = "data/stages_s100.Rds")

saveRDS(stages.g200.inf.s, file = "data/stages_g200_inf.Rds")
saveRDS(stages.g100.inf.s, file = "data/stages_g100_inf.Rds")
saveRDS(stages.s200.inf.s, file = "data/stages_s200_inf.Rds")
saveRDS(stages.s100.inf.s, file = "data/stages_s100_inf.Rds")

saveRDS(stages.g200.epif.s, file = "data/stages_g200_epif.Rds")
saveRDS(stages.g100.epif.s, file = "data/stages_g100_epif.Rds")
saveRDS(stages.s200.epif.s, file = "data/stages_s200_epif.Rds")
saveRDS(stages.s100.epif.s, file = "data/stages_s100_epif.Rds")

#### Sensitivity testing - additional quality criterion ####
## Only retain grid cells with 3+ references with bivalves and brachiopods
## Read in functions
source("functions/standardiseCells.R")
source("functions/add.reference.IDs.R")
source("functions/applyRefCollThresh.R")
source("functions/label.cell.reference.combo.comps.R")

## Set new quality critera
multiton.min <- 0.2
n.cores <- 4
coll.min <- 10
ref.min <- 3

## For minimum three references for both brachiopods and bivalves, initial threshold of just 3
stages.g200.ref <- standardiseCells(stages.g200, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
stages.g100.ref <- standardiseCells(stages.g100, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera", n.cores = n.cores)
stages.s200.ref <- standardiseCells(stages.s200, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)
stages.s100.ref <- standardiseCells(stages.s100, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species", n.cores = n.cores)

## Label cell-reference combination compositions
stages.g200.ref <- label.cell.reference.combo.comps(stages.g200.ref, n.cores = 4)
stages.g100.ref <- label.cell.reference.combo.comps(stages.g100.ref, n.cores = 4)
stages.s200.ref <- label.cell.reference.combo.comps(stages.s200.ref, n.cores = 4)
stages.s100.ref <- label.cell.reference.combo.comps(stages.s100.ref, n.cores = 4)

## Now drop cells with fewer than 3 references containing bivalves and/or brachiopods
stages.g200.ref <- applyRefCollThresh(stages.g200.ref)
stages.g100.ref <- applyRefCollThresh(stages.g100.ref)
stages.s200.ref <- applyRefCollThresh(stages.s200.ref)
stages.s100.ref <- applyRefCollThresh(stages.s100.ref)

## Save
saveRDS(stages.g200.ref, file = "data/stages_g200_ref3.Rds")
saveRDS(stages.g100.ref, file = "data/stages_g100_ref3.Rds")
saveRDS(stages.s200.ref, file = "data/stages_s200_ref3.Rds")
saveRDS(stages.s100.ref, file = "data/stages_s100_ref3.Rds")
