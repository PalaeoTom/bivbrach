## 2.2 Time binning and filtering
## Started by TJS on 08/01/2024

## If packages aren't installed, install them, then load them
packages <- c("divvy", "stringr", "fossilbrush")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divvy)
library(stringr)
library(fossilbrush)

## Clean directory
rm(list = ls())

#### Load data ####
master <- readRDS("data/final/master_2_1.Rds")

## Read in final stages for time binning
bins <- read.csv("data/metadata/binning_timescale.csv", row.names = 1)

#### Time bin data ####
## Number stages
bins$number <- seq(1,nrow(bins),1)

## Create empty container for stage numbers
master$stage <- rep(NA,nrow(master))

## Assign occurrences to time bins
for(b in 1:nrow(bins)){
  ## Get max
  max <- bins[b,"FAD"]
  min <- bins[b,"LAD"]
  ## Get all occurrences with a FAD equal to or less than max
  x <- which(master$max_ma <= max)
  ## Get all occurrences with a LAD equal to or greater than min
  y <- which(master$min_ma >= min)
  ## Get intersect
  if(length(x)>0 && length(y)>0){
    z <- intersect(x,y)
  } else {
    z <- NA
  }
  ## Mark those as being in bin b
  if(!all(is.na(z))){
    master[z,"stage"] <- b
  }
}

## Drop occurrences appearing in more than one stage
droppers <- which(is.na(master$stage))
master_multiStage <- master[droppers,]
master_1stage <- master[-droppers,]

## Nothing to be done for occurrences that cross three boundaries
## For those that cross 2, how many occurrences would be retained if they were assigned to time bins the majority of their temporal range overlaps with?
# Get boundaries
bounds <- c(bins$FAD,bins$LAD[nrow(bins)])
## Now assign occurrences which only cross a single boundary to stages they are more likely to occur within.
for(n in 1:nrow(master_multiStage)){
  ## boundaries crossed
  cross <- which(data.table::between(bounds, lower = master_multiStage[n,"min_ma"], upper = master_multiStage[n,"max_ma"]))
  ## skip if no boundaries crossed (occurrence that needs dropping because it is beyond range)
  if(length(cross)==0){
    next
  }
  ## get values
  cross.v <- bounds[cross]
  ## If any of cross.v = max_ma or min_ma, that means range of occurrence starts and/or ends at boundary
  ## If max_ma is not present, lets add previous boundary to beginning of vector
  if(!any(cross.v==master_multiStage[n,"max_ma"])){
    cross.v <- c(bounds[cross[1]-1],cross.v)
  }
  ## If min_ma is not present in cross.v, lets add the next boundary to end of vector
  if(!any(cross.v==master_multiStage[n,"min_ma"])){
    cross.v <- c(cross.v,bounds[cross[length(cross)]+1])
  }
  ## Now, if length of cross.v is over 3, skip. Otherwise, work out how much of each
  if(length(cross.v)==3){
    ## get range of occurrences
    range <- master_multiStage[n,"max_ma"]-master_multiStage[n,"min_ma"]
    ## get int1 prop
    int1.prop <- (master_multiStage[n,"max_ma"]-cross.v[2])/range
    ## get int2 prop
    int2.prop <- (cross.v[2]-master_multiStage[n,"min_ma"])/range
    ## If int1.prop is greater than 50%, assign first interval as time bin
    if(int1.prop > 0.5){
      master_multiStage[n,"stage"] <- bins[which(bins$FAD == cross.v[1]),"number"]
    }
    ## If int2.prop is greater than 50%, assign second nterval as time bin
    if(int2.prop > 0.5){
      master_multiStage[n,"stage"] <- bins[which(bins$FAD == cross.v[2]),"number"]
    }
    ## Don't assign bin if both equal
  }
}

## How many can we keep?
master <- rbind(master_1stage,master_multiStage[which(!is.na(master_multiStage$stage)),])

#### Derive covariate data for cells from occurrences ####
## Combine grid cell number and stage number into single spacetime number
master$stage_cell <- apply(master, 1, function(x) str_flatten(c(x[43],x[40]),collapse = "_"))
master$stage_cell <- str_replace(master$stage_cell, " ", "")

## Determine cell lithology, environment, reef status, and latitudinal centroid from combined dataset
# Load function
source("functions/add.cell.covariate.R")

# Run function
master <- add.cell.covariate(master, stage_cell = "stage_cell", name = "cellLith", unknown = "unknown", ref = "lith_category", values = c("carbonate", "siliciclastic"), threshold = 0.75)
master <- add.cell.covariate(master, stage_cell = "stage_cell", name = "cellBath", unknown = "unknown", ref = "bath_category", values = c("shallow", "deep"), threshold = 0.75)
master <- add.cell.covariate(master, stage_cell = "stage_cell", name = "cellReef", unknown = "unknown", ref = "reef_category", values = c("reef", "non-reef"), threshold = 0.75)

#### Export datasets
saveRDS(master, "data/final/master_2_2.Rds")

#### Standardise each time bin to ensure good data quality ####
rm(list=ls())

## Read in data
master <- readRDS("data/final/master_2_2.Rds")

## Remove duplicate occurrences from each grid cell-time bin combination
## Get time bins
timeBins <- sort(unique(master$stage))
## 50km
uniq_master <- master[NULL,]
for(i in timeBins){
  ## Get subset
  uniq_master <- rbind(uniq_master,uniqify(master[which(master$stage==i),], xy = c("cellx_50km","celly_50km"), taxVar = "combined_name"))
}

#### Standardise uniqified grid cells ####
source("functions/standardiseCells.R")
occs.min <- 5

## Run function
uniq_master <- standardiseCells(data = uniq_master, stage_cell = "stage_cell", minOccs = occs.min)

#### Export datasets
saveRDS(uniq_master, "data/final/master_2_2_uniq.Rds")

#### Building species version of dataset ####
## First, let's quickly clean up species names
rm(list=ls())

## Read in data
master <- readRDS("data/final/master_2_2.Rds")

## Clean up names
master$species <- clean_name(master$species)

## Drop NAs
master <- master[-which(is.na(master$species)),]

## Standardise dipthongs
source("functions/misspell.R")
master$species <- misspell(master$species)

## Check for punctuation
any(str_detect(master$species, pattern = "[:punct:]"))

## Capitalisation
master$species <- str_to_lower(master$species)

## Now to combine into a single unique name to update combined name
master$combined_name <- apply(master, 1, function(x) paste0(x[5], "_", x[9], "_", x[10]))

## Remove duplicate occurrences from each grid cell-time bin combination
## Get time bins
timeBins <- sort(unique(master$stage))
## 50km
uniq_master <- master[NULL,]
for(i in timeBins){
  ## Get subset
  uniq_master <- rbind(uniq_master,uniqify(master[which(master$stage==i),], xy = c("cellx_50km","celly_50km"), taxVar = "combined_name"))
}

#### Standardise uniqified grid cells ####
source("functions/standardiseCells.R")
occs.min <- 5

## Run function
uniq_master <- standardiseCells(data = uniq_master, stage_cell = "stage_cell", minOccs = occs.min)

#### Export datasets
saveRDS(uniq_master, "data/final/master_2_2_uniq_species.Rds")

