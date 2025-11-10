## 2.2 Time binning and filtering
## Started by TJS on 08/01/2024

## If packages aren't installed, install them, then load them
packages <- c("divvy", "stringr", "fossilbrush", "divDyn", "ggplot2", "rnaturalearth", "rnaturalearthdata", "stringr", "terra", "parallel")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divvy)
library(stringr)
library(fossilbrush)
library(divDyn)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(terra)
library(parallel)

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

#### Derive covariate data for cells from occurrences and remove duplicates ####
## Separate into 50km, 100km, and 200km grid cells
master_50 <- master[,c(-43:-51)]
master_100 <- master[,c(-40:-42,-46:-51)]
master_150 <- master[,c(-40:-45,-49:-51)]
master_200 <- master[,c(-40:-48)]

##### Start with 50km #####
## Combine grid cell number and stage number into single spacetime number
master_50$stage_cell <- apply(master_50, 1, function(x) str_flatten(c(x[43],x[40]),collapse = "_"))
master_50$stage_cell <- str_replace(master_50$stage_cell, " ", "")

## Determine cell lithology, environment, reef status, and latitudinal centroid from combined dataset
# Load function
source("functions/get.cell.covariate.R")
master_50[,"cellLith"] <- get.cell.covariate(master_50, cell = "stage_cell", unknown = "unknown", ref = "lith_category", value = "carbonate")
master_50[,"cellBath"] <- get.cell.covariate(master_50, cell = "stage_cell", unknown = "unknown", ref = "bath_category", value = "deep")
master_50[,"cellReef"] <- get.cell.covariate(master_50, cell = "stage_cell", unknown = "unknown", ref = "reef_category", value = "reef")

## Export dataset with duplicates
saveRDS(master_50, "data/final/master_50_2_2.Rds")

##### 100km grid cells #####
## Combine grid cell number and stage number into single spacetime number
master_100$stage_cell <- apply(master_100, 1, function(x) str_flatten(c(x[43],x[40]),collapse = "_"))
master_100$stage_cell <- str_replace(master_100$stage_cell, " ", "")

## Determine cell lithology, environment, reef status, and latitudinal centroid from combined dataset
# Load function
source("functions/add.cell.covariate.R")

# Run function
# 0 = all siliciclastic, 1 = all carbonate
master_100 <- add.cell.covariate(master_100, cell = "stage_cell", name = "cellLith", unknown = "unknown", ref = "lith_category", value = "carbonate")
# 0 = all shallow, 1 = all deep
master_100 <- add.cell.covariate(master_100, cell = "stage_cell", name = "cellBath", unknown = "unknown", ref = "bath_category", value = "deep")
# 0 = no reef, 1 = all reef
master_100 <- add.cell.covariate(master_100, cell = "stage_cell", name = "cellReef", unknown = "unknown", ref = "reef_category", value = "reef")

## Export dataset with duplicates
saveRDS(master_100, "data/final/master_100_2_2.Rds")

##### 150km grid cells #####
## Combine grid cell number and stage number into single spacetime number
master_150$stage_cell <- apply(master_150, 1, function(x) str_flatten(c(x[43],x[40]),collapse = "_"))
master_150$stage_cell <- str_replace(master_150$stage_cell, " ", "")

## Determine cell lithology, environment, reef status, and latitudinal centroid from combined dataset
# Load function
source("functions/add.cell.covariate.R")

# Run function
# 0 = all siliciclastic, 1 = all carbonate
master_150 <- add.cell.covariate(master_150, cell = "stage_cell", name = "cellLith", unknown = "unknown", ref = "lith_category", value = "carbonate")
# 0 = all shallow, 1 = all deep
master_150 <- add.cell.covariate(master_150, cell = "stage_cell", name = "cellBath", unknown = "unknown", ref = "bath_category", value = "deep")
# 0 = no reef, 1 = all reef
master_150 <- add.cell.covariate(master_150, cell = "stage_cell", name = "cellReef", unknown = "unknown", ref = "reef_category", value = "reef")

## Export dataset with duplicates
saveRDS(master_150, "data/final/master_150_2_2.Rds")

##### 200km grid cells #####
## Combine grid cell number and stage number into single spacetime number
master_200$stage_cell <- apply(master_200, 1, function(x) str_flatten(c(x[43],x[40]),collapse = "_"))
master_200$stage_cell <- str_replace(master_200$stage_cell, " ", "")

## Determine cell lithology, environment, reef status, and latitudinal centroid from combined dataset
# Load function
source("functions/add.cell.covariate.R")

# Run function
# 0 = all siliciclastic, 1 = all carbonate
master_200 <- add.cell.covariate(master_200, cell = "stage_cell", name = "cellLith", unknown = "unknown", ref = "lith_category", value = "carbonate")
# 0 = all shallow, 1 = all deep
master_200 <- add.cell.covariate(master_200, cell = "stage_cell", name = "cellBath", unknown = "unknown", ref = "bath_category", value = "deep")
# 0 = no reef, 1 = all reef
master_200 <- add.cell.covariate(master_200, cell = "stage_cell", name = "cellReef", unknown = "unknown", ref = "reef_category", value = "reef")

## Export dataset with duplicates
saveRDS(master_200, "data/final/master_200_2_2.Rds")

#### Standardising occurrence data for main analysis ####
## 1. Drop grid cells with fewer than 3 unique genera
source("functions/standardiseCells.R")

## Set minimum
gen.min <- 3

## Get standardised data - minimum 3 unique taxa per cell
master_50 <- standardiseCells(master_50, cell = "stage_cell", type = "taxa", minOccs = gen.min)
master_100 <- standardiseCells(master_100, cell = "stage_cell", type = "taxa", minOccs = gen.min)
master_150 <- standardiseCells(master_150, cell = "stage_cell", type = "taxa", minOccs = gen.min)
master_200 <- standardiseCells(master_200, cell = "stage_cell", type = "taxa", minOccs = gen.min)

## Export
saveRDS(master_50, "data/final/master_50_2_2_min3gen.Rds")
saveRDS(master_100, "data/final/master_100_2_2_min3gen.Rds")
saveRDS(master_150, "data/final/master_150_2_2_min3gen.Rds")
saveRDS(master_200, "data/final/master_200_2_2_min3gen.Rds")

## 2. Drop grid cells with fewer than 20 occurrences.
rm(list=ls())

## Read in standardised data
master_50 <- readRDS("data/final/master_50_2_2_min3gen.Rds")
master_100 <- readRDS("data/final/master_100_2_2_min3gen.Rds")
master_150 <- readRDS("data/final/master_150_2_2_min3gen.Rds")
master_200 <- readRDS("data/final/master_200_2_2_min3gen.Rds")

## Standardise number of occurrences - minimum 20. Minimum 3 genera already applied.
source("functions/standardiseCells.R")
occs.min <- 20
master_50 <- standardiseCells(data = master_50, cell = "stage_cell", minOccs = occs.min, type = "occs")
master_100 <- standardiseCells(data = master_100, cell = "stage_cell", minOccs = occs.min, type = "occs")
master_150 <- standardiseCells(data = master_150, cell = "stage_cell", minOccs = occs.min, type = "occs")
master_200 <- standardiseCells(data = master_200, cell = "stage_cell", minOccs = occs.min, type = "occs")

## Save standardised data
saveRDS(master_50, file = "data/final/master_50_2_2_min3gen_min20occs.Rds")
saveRDS(master_100, file = "data/final/master_100_2_2_min3gen_min20occs.Rds")
saveRDS(master_150, file = "data/final/master_150_2_2_min3gen_min20occs.Rds")
saveRDS(master_200, file = "data/final/master_200_2_2_min3gen_min20occs.Rds")

#### Get rarefaction curves to assess sampling ####
## 3. Calculate slopes of final 5 occurrences of each grid cell. Drop all cells with slope of 0.25 or greater (i.e., equivalent to a change of 1 genera )
rm(list=ls())

## Read it in if starting from scratch
master_50 <- readRDS("data/final/master_50_2_2_min3gen_min20occs.Rds")
master_100 <- readRDS("data/final/master_100_2_2_min3gen_min20occs.Rds")
master_150 <- readRDS("data/final/master_150_2_2_min3gen_min20occs.Rds")
master_200 <- readRDS("data/final/master_200_2_2_min3gen_min20occs.Rds")

## Draw and save rarefaction curves - note, iter = 10000 takes a long while! However, needed to limit variance.
source("functions/rarefaction_curve.R")
source("functions/rarefaction_curve_all_cells.R")

master_50_RCs <- rarefaction_curve_all_cells(data = master_50, cell = "stage_cell", taxVar = "combined_name", iter = 10000, n.cores = 8)
saveRDS(master_50_RCs, file = "data/sensitivity_testing//master_50_2_2_min3gen_min20occs_RCs.Rds")

master_100_RCs <- rarefaction_curve_all_cells(data = master_100, cell = "stage_cell", taxVar = "combined_name", iter = 10000, n.cores = 8)
saveRDS(master_100_RCs, file = "data/sensitivity_testing/master_100_2_2_min3gen_min20occs_RCs.Rds")

master_150_RCs <- rarefaction_curve_all_cells(data = master_150, cell = "stage_cell", taxVar = "combined_name", iter = 10000, n.cores = 8)
saveRDS(master_150_RCs, file = "data/sensitivity_testing/master_150_2_2_min3gen_min20occs_RCs.Rds")

master_200_RCs <- rarefaction_curve_all_cells(data = master_200, cell = "stage_cell", taxVar = "combined_name", iter = 10000, n.cores = 8)
saveRDS(master_200_RCs, file = "data/sensitivity_testing/master_200_2_2_min3gen_min20occs_RCs.Rds")

#### Filter out cells with a RC tail slope less than 0.25 across final five occurrences ####
rm(list=ls())

## Read it in if starting from scratch
master_50 <- readRDS("data/final/master_50_2_2_min3gen_min20occs.Rds")
master_100 <- readRDS("data/final/master_100_2_2_min3gen_min20occs.Rds")
master_150 <- readRDS("data/final/master_150_2_2_min3gen_min20occs.Rds")
master_200 <- readRDS("data/final/master_200_2_2_min3gen_min20occs.Rds")

## Read in rarefaction curves
master_50_RCs <- readRDS("data/sensitivity_testing/master_50_2_2_min3gen_min20occs_RCs.Rds")
master_100_RCs <- readRDS("data/sensitivity_testing/master_100_2_2_min3gen_min20occs_RCs.Rds")
master_150_RCs <- readRDS("data/sensitivity_testing/master_150_2_2_min3gen_min20occs_RCs.Rds")
master_200_RCs <- readRDS("data/sensitivity_testing/master_200_2_2_min3gen_min20occs_RCs.Rds")

## Wittle down to grid cells that meet asymptote criteria
asymptote.occs <- 5
slope.threshold <- 0.25
source("functions/test_RC_tail_asymptote.R")
master_50_RCs_bool <- test_RC_tail_asymptote(RCs = master_50_RCs[,-1], n = asymptote.occs, threshold = slope.threshold)
master_100_RCs_bool <- test_RC_tail_asymptote(RCs = master_100_RCs[,-1], n = asymptote.occs, threshold = slope.threshold)
master_150_RCs_bool <- test_RC_tail_asymptote(RCs = master_150_RCs[,-1], n = asymptote.occs, threshold = slope.threshold)
master_200_RCs_bool <- test_RC_tail_asymptote(RCs = master_200_RCs[,-1], n = asymptote.occs, threshold = slope.threshold)

## Filter out grid cells that don't have asymptote
master_50_final <- master_50[which(master_50$stage_cell %in% names(which(master_50_RCs_bool))),]
master_100_final <- master_100[which(master_100$stage_cell %in% names(which(master_100_RCs_bool))),]
master_150_final <- master_150[which(master_150$stage_cell %in% names(which(master_150_RCs_bool))),]
master_200_final <- master_200[which(master_200$stage_cell %in% names(which(master_200_RCs_bool))),]

## Export
saveRDS(master_50_final, "data/final/master_50_2_2_min3gen_min20occs_slope0.25.Rds")
saveRDS(master_100_final, "data/final/master_100_2_2_min3gen_min20occs_slope0.25.Rds")
saveRDS(master_150_final, "data/final/master_150_2_2_min3gen_min20occs_slope0.25.Rds")
saveRDS(master_200_final, "data/final/master_200_2_2_min3gen_min20occs_slope0.25.Rds")

#### Pruning out occurrences with missing covariates and sensitivity testing interpolation ####
## Plan is to run two analyses:
## 1) Drop all grid cells lacking covariate data
## 2) Interpolate where possible using grid cells in concentric rings, starting with closest and working out to 5 cells.
## Clean up
rm(list=ls())

## Read in data
master_50 <- readRDS("data/final/master_50_2_2_min3gen_min20occs_slope0.25.Rds")
master_100 <- readRDS("data/final/master_100_2_2_min3gen_min20occs_slope0.25.Rds")
master_150 <- readRDS("data/final/master_150_2_2_min3gen_min20occs_slope0.25.Rds")
master_200 <- readRDS("data/final/master_200_2_2_min3gen_min20occs_slope0.25.Rds")

## Read in function for checking and pruning cells lacking covariates
source("functions/cov.check.R")
source("functions/cov.prune.R")

## Prune
master_50_pruned <- cov.prune(data = master_50, stage_cell = "stage_cell", coords = c("cellx_50km","celly_50km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_100_pruned <- cov.prune(data = master_100, stage_cell = "stage_cell", coords = c("cellx_100km","celly_100km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_150_pruned <- cov.prune(data = master_150, stage_cell = "stage_cell", coords = c("cellx_150km","celly_150km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_200_pruned <- cov.prune(data = master_200, stage_cell = "stage_cell", coords = c("cellx_200km","celly_200km"), covariates = c("cellLith", "cellBath", "cellReef"))

## Export
saveRDS(master_50_pruned, file = "data/final/master_50_2_2_min3gen_min20occs_slope0.25_covsPruned.Rds")
saveRDS(master_100_pruned, file = "data/final/master_100_2_2_min3gen_min20occs_slope0.25_covsPruned.Rds")
saveRDS(master_150_pruned, file = "data/final/master_150_2_2_min3gen_min20occs_slope0.25_covsPruned.Rds")
saveRDS(master_200_pruned, file = "data/final/master_200_2_2_min3gen_min20occs_slope0.25_covsPruned.Rds")

## Reconstruct original rasters used to rasterise data
rast50 <- terra::project(x = terra::rast(), y = "EPSG:8857", res = 50000)
terra::values(rast50) <- 1:terra::ncell(rast50)
rast100 <- terra::project(x = terra::rast(), y = "EPSG:8857", res = 100000)
terra::values(rast100) <- 1:terra::ncell(rast100)
rast150 <- terra::project(x = terra::rast(), y = "EPSG:8857", res = 150000)
terra::values(rast150) <- 1:terra::ncell(rast150)
rast200 <- terra::project(x = terra::rast(), y = "EPSG:8857", res = 200000)
terra::values(rast200) <- 1:terra::ncell(rast200)

## Read in functions
source("functions/get_cell_rings.R")
source("functions/interpolate_covariates.R")

## 50km
## Try with 1 ring first
rings = 1
master_50_intCov1 <- interpolate_covariates(master_50, raster = rast50, covariate = "cellBath", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_50_intCov1 <- interpolate_covariates(master_50_intCov1, raster = rast50, covariate = "cellLith", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_50_intCov1 <- interpolate_covariates(master_50_intCov1, raster = rast50, covariate = "cellReef", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 2 rings
rings = 2
master_50_intCov2 <- interpolate_covariates(master_50, raster = rast50, covariate = "cellBath", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_50_intCov2 <- interpolate_covariates(master_50_intCov2, raster = rast50, covariate = "cellLith", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_50_intCov2 <- interpolate_covariates(master_50_intCov2, raster = rast50, covariate = "cellReef", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 3 rings
rings = 3
master_50_intCov3 <- interpolate_covariates(master_50, raster = rast50, covariate = "cellBath", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_50_intCov3 <- interpolate_covariates(master_50_intCov3, raster = rast50, covariate = "cellLith", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_50_intCov3 <- interpolate_covariates(master_50_intCov3, raster = rast50, covariate = "cellReef", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 4 rings
rings = 4
master_50_intCov4 <- interpolate_covariates(master_50, raster = rast50, covariate = "cellBath", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_50_intCov4 <- interpolate_covariates(master_50_intCov4, raster = rast50, covariate = "cellLith", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_50_intCov4 <- interpolate_covariates(master_50_intCov4, raster = rast50, covariate = "cellReef", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 5 rings
rings = 5
master_50_intCov5 <- interpolate_covariates(master_50, raster = rast50, covariate = "cellBath", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_50_intCov5 <- interpolate_covariates(master_50_intCov5, raster = rast50, covariate = "cellLith", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_50_intCov5 <- interpolate_covariates(master_50_intCov5, raster = rast50, covariate = "cellReef", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## 100km
## Try with 1 ring first
rings = 1
master_100_intCov1 <- interpolate_covariates(master_100, raster = rast100, covariate = "cellBath", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_100_intCov1 <- interpolate_covariates(master_100_intCov1, raster = rast100, covariate = "cellLith", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_100_intCov1 <- interpolate_covariates(master_100_intCov1, raster = rast100, covariate = "cellReef", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 2 rings
rings = 2
master_100_intCov2 <- interpolate_covariates(master_100, raster = rast100, covariate = "cellBath", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_100_intCov2 <- interpolate_covariates(master_100_intCov2, raster = rast100, covariate = "cellLith", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_100_intCov2 <- interpolate_covariates(master_100_intCov2, raster = rast100, covariate = "cellReef", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 3 rings
rings = 3
master_100_intCov3 <- interpolate_covariates(master_100, raster = rast100, covariate = "cellBath", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_100_intCov3 <- interpolate_covariates(master_100_intCov3, raster = rast100, covariate = "cellLith", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_100_intCov3 <- interpolate_covariates(master_100_intCov3, raster = rast100, covariate = "cellReef", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 4 rings
rings = 4
master_100_intCov4 <- interpolate_covariates(master_100, raster = rast100, covariate = "cellBath", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_100_intCov4 <- interpolate_covariates(master_100_intCov4, raster = rast100, covariate = "cellLith", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_100_intCov4 <- interpolate_covariates(master_100_intCov4, raster = rast100, covariate = "cellReef", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 5 rings
rings = 5
master_100_intCov5 <- interpolate_covariates(master_100, raster = rast100, covariate = "cellBath", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_100_intCov5 <- interpolate_covariates(master_100_intCov5, raster = rast100, covariate = "cellLith", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_100_intCov5 <- interpolate_covariates(master_100_intCov5, raster = rast100, covariate = "cellReef", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## 150km
## Try with 1 ring first
rings = 1
master_150_intCov1 <- interpolate_covariates(master_150, raster = rast150, covariate = "cellBath", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_150_intCov1 <- interpolate_covariates(master_150_intCov1, raster = rast150, covariate = "cellLith", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_150_intCov1 <- interpolate_covariates(master_150_intCov1, raster = rast150, covariate = "cellReef", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 2 rings
rings = 2
master_150_intCov2 <- interpolate_covariates(master_150, raster = rast150, covariate = "cellBath", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_150_intCov2 <- interpolate_covariates(master_150_intCov2, raster = rast150, covariate = "cellLith", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_150_intCov2 <- interpolate_covariates(master_150_intCov2, raster = rast150, covariate = "cellReef", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 3 rings
rings = 3
master_150_intCov3 <- interpolate_covariates(master_150, raster = rast150, covariate = "cellBath", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_150_intCov3 <- interpolate_covariates(master_150_intCov3, raster = rast150, covariate = "cellLith", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_150_intCov3 <- interpolate_covariates(master_150_intCov3, raster = rast150, covariate = "cellReef", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 4 rings
rings = 4
master_150_intCov4 <- interpolate_covariates(master_150, raster = rast150, covariate = "cellBath", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_150_intCov4 <- interpolate_covariates(master_150_intCov4, raster = rast150, covariate = "cellLith", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_150_intCov4 <- interpolate_covariates(master_150_intCov4, raster = rast150, covariate = "cellReef", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 5 rings
rings = 5
master_150_intCov5 <- interpolate_covariates(master_150, raster = rast150, covariate = "cellBath", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_150_intCov5 <- interpolate_covariates(master_150_intCov5, raster = rast150, covariate = "cellLith", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_150_intCov5 <- interpolate_covariates(master_150_intCov5, raster = rast150, covariate = "cellReef", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## 200km
## Try with 1 ring first
rings = 1
master_200_intCov1 <- interpolate_covariates(master_200, raster = rast200, covariate = "cellBath", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_200_intCov1 <- interpolate_covariates(master_200_intCov1, raster = rast200, covariate = "cellLith", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_200_intCov1 <- interpolate_covariates(master_200_intCov1, raster = rast200, covariate = "cellReef", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 2 rings
rings = 2
master_200_intCov2 <- interpolate_covariates(master_200, raster = rast200, covariate = "cellBath", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_200_intCov2 <- interpolate_covariates(master_200_intCov2, raster = rast200, covariate = "cellLith", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_200_intCov2 <- interpolate_covariates(master_200_intCov2, raster = rast200, covariate = "cellReef", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 3 rings
rings = 3
master_200_intCov3 <- interpolate_covariates(master_200, raster = rast200, covariate = "cellBath", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_200_intCov3 <- interpolate_covariates(master_200_intCov3, raster = rast200, covariate = "cellLith", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_200_intCov3 <- interpolate_covariates(master_200_intCov3, raster = rast200, covariate = "cellReef", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 4 rings
rings = 4
master_200_intCov4 <- interpolate_covariates(master_200, raster = rast200, covariate = "cellBath", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_200_intCov4 <- interpolate_covariates(master_200_intCov4, raster = rast200, covariate = "cellLith", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_200_intCov4 <- interpolate_covariates(master_200_intCov4, raster = rast200, covariate = "cellReef", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Then 5 rings
rings = 5
master_200_intCov5 <- interpolate_covariates(master_200, raster = rast200, covariate = "cellBath", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_200_intCov5 <- interpolate_covariates(master_200_intCov5, raster = rast200, covariate = "cellLith", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_200_intCov5 <- interpolate_covariates(master_200_intCov5, raster = rast200, covariate = "cellReef", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Now prune
master_50_intCov1 <- cov.prune(data = master_50_intCov1, stage_cell = "stage_cell", coords = c("cellx_50km","celly_50km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_100_intCov1 <- cov.prune(data = master_100_intCov1, stage_cell = "stage_cell", coords = c("cellx_100km","celly_100km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_150_intCov1 <- cov.prune(data = master_150_intCov1, stage_cell = "stage_cell", coords = c("cellx_150km","celly_150km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_200_intCov1 <- cov.prune(data = master_200_intCov1, stage_cell = "stage_cell", coords = c("cellx_200km","celly_200km"), covariates = c("cellLith", "cellBath", "cellReef"))

master_50_intCov2 <- cov.prune(data = master_50_intCov2, stage_cell = "stage_cell", coords = c("cellx_50km","celly_50km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_100_intCov2 <- cov.prune(data = master_100_intCov2, stage_cell = "stage_cell", coords = c("cellx_100km","celly_100km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_150_intCov2 <- cov.prune(data = master_150_intCov2, stage_cell = "stage_cell", coords = c("cellx_150km","celly_150km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_200_intCov2 <- cov.prune(data = master_200_intCov2, stage_cell = "stage_cell", coords = c("cellx_200km","celly_200km"), covariates = c("cellLith", "cellBath", "cellReef"))

master_50_intCov3 <- cov.prune(data = master_50_intCov3, stage_cell = "stage_cell", coords = c("cellx_50km","celly_50km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_100_intCov3 <- cov.prune(data = master_100_intCov3, stage_cell = "stage_cell", coords = c("cellx_100km","celly_100km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_150_intCov3 <- cov.prune(data = master_150_intCov3, stage_cell = "stage_cell", coords = c("cellx_150km","celly_150km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_200_intCov3 <- cov.prune(data = master_200_intCov3, stage_cell = "stage_cell", coords = c("cellx_200km","celly_200km"), covariates = c("cellLith", "cellBath", "cellReef"))

master_50_intCov4 <- cov.prune(data = master_50_intCov4, stage_cell = "stage_cell", coords = c("cellx_50km","celly_50km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_100_intCov4 <- cov.prune(data = master_100_intCov4, stage_cell = "stage_cell", coords = c("cellx_100km","celly_100km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_150_intCov4 <- cov.prune(data = master_150_intCov4, stage_cell = "stage_cell", coords = c("cellx_150km","celly_150km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_200_intCov4 <- cov.prune(data = master_200_intCov4, stage_cell = "stage_cell", coords = c("cellx_200km","celly_200km"), covariates = c("cellLith", "cellBath", "cellReef"))

master_50_intCov5 <- cov.prune(data = master_50_intCov5, stage_cell = "stage_cell", coords = c("cellx_50km","celly_50km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_100_intCov5 <- cov.prune(data = master_100_intCov5, stage_cell = "stage_cell", coords = c("cellx_100km","celly_100km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_150_intCov5 <- cov.prune(data = master_150_intCov5, stage_cell = "stage_cell", coords = c("cellx_150km","celly_150km"), covariates = c("cellLith", "cellBath", "cellReef"))
master_200_intCov5 <- cov.prune(data = master_200_intCov5, stage_cell = "stage_cell", coords = c("cellx_200km","celly_200km"), covariates = c("cellLith", "cellBath", "cellReef"))

## Summarise for comparison
gridCells <- c(rep("50km", 6), rep("100km", 6), rep("150km", 6), rep("200km", 6))
numberOfRings <- c(0,50,100,150,200,250,0,100,200,300,400,500,0,150,300,450,600,750,0,200,400,600,800,1000)
nCells <- rep(NA, length(gridCells))
nOccs <- rep(NA, length(gridCells))
suma <- data.frame(cbind("GridCellSize" = gridCells, "RadiusOfWidestInterpolationRingInKm" = numberOfRings, "NumberOfCells" = nCells, "NumberOfOccurrences" = nOccs))

## Populate cells
suma[1,"NumberOfCells"] <- length(unique(master_50_pruned$stage_cell))
suma[1,"NumberOfOccurrences"] <- nrow(master_50_pruned)
suma[2,"NumberOfCells"] <- length(unique(master_50_intCov1$stage_cell))
suma[2,"NumberOfOccurrences"] <- nrow(master_50_intCov1)
suma[3,"NumberOfCells"] <- length(unique(master_50_intCov2$stage_cell))
suma[3,"NumberOfOccurrences"] <- nrow(master_50_intCov2)
suma[4,"NumberOfCells"] <- length(unique(master_50_intCov3$stage_cell))
suma[4,"NumberOfOccurrences"] <- nrow(master_50_intCov3)
suma[5,"NumberOfCells"] <- length(unique(master_50_intCov4$stage_cell))
suma[5,"NumberOfOccurrences"] <- nrow(master_50_intCov4)
suma[6,"NumberOfCells"] <- length(unique(master_50_intCov5$stage_cell))
suma[6,"NumberOfOccurrences"] <- nrow(master_50_intCov5)

suma[7,"NumberOfCells"] <- length(unique(master_100_pruned$stage_cell))
suma[7,"NumberOfOccurrences"] <- nrow(master_100_pruned)
suma[8,"NumberOfCells"] <- length(unique(master_100_intCov1$stage_cell))
suma[8,"NumberOfOccurrences"] <- nrow(master_100_intCov1)
suma[9,"NumberOfCells"] <- length(unique(master_100_intCov2$stage_cell))
suma[9,"NumberOfOccurrences"] <- nrow(master_100_intCov2)
suma[10,"NumberOfCells"] <- length(unique(master_100_intCov3$stage_cell))
suma[10,"NumberOfOccurrences"] <- nrow(master_100_intCov3)
suma[11,"NumberOfCells"] <- length(unique(master_100_intCov4$stage_cell))
suma[11,"NumberOfOccurrences"] <- nrow(master_100_intCov4)
suma[12,"NumberOfCells"] <- length(unique(master_100_intCov5$stage_cell))
suma[12,"NumberOfOccurrences"] <- nrow(master_100_intCov5)

suma[13,"NumberOfCells"] <- length(unique(master_150_pruned$stage_cell))
suma[13,"NumberOfOccurrences"] <- nrow(master_150_pruned)
suma[14,"NumberOfCells"] <- length(unique(master_150_intCov1$stage_cell))
suma[14,"NumberOfOccurrences"] <- nrow(master_150_intCov1)
suma[15,"NumberOfCells"] <- length(unique(master_150_intCov2$stage_cell))
suma[15,"NumberOfOccurrences"] <- nrow(master_150_intCov2)
suma[16,"NumberOfCells"] <- length(unique(master_150_intCov3$stage_cell))
suma[16,"NumberOfOccurrences"] <- nrow(master_150_intCov3)
suma[17,"NumberOfCells"] <- length(unique(master_150_intCov4$stage_cell))
suma[17,"NumberOfOccurrences"] <- nrow(master_150_intCov4)
suma[18,"NumberOfCells"] <- length(unique(master_150_intCov5$stage_cell))
suma[18,"NumberOfOccurrences"] <- nrow(master_150_intCov5)

suma[19,"NumberOfCells"] <- length(unique(master_200_pruned$stage_cell))
suma[19,"NumberOfOccurrences"] <- nrow(master_200_pruned)
suma[20,"NumberOfCells"] <- length(unique(master_200_intCov1$stage_cell))
suma[20,"NumberOfOccurrences"] <- nrow(master_200_intCov1)
suma[21,"NumberOfCells"] <- length(unique(master_200_intCov2$stage_cell))
suma[21,"NumberOfOccurrences"] <- nrow(master_200_intCov2)
suma[22,"NumberOfCells"] <- length(unique(master_200_intCov3$stage_cell))
suma[22,"NumberOfOccurrences"] <- nrow(master_200_intCov3)
suma[23,"NumberOfCells"] <- length(unique(master_200_intCov4$stage_cell))
suma[23,"NumberOfOccurrences"] <- nrow(master_200_intCov4)
suma[24,"NumberOfCells"] <- length(unique(master_200_intCov5$stage_cell))
suma[24,"NumberOfOccurrences"] <- nrow(master_200_intCov5)
write.csv(suma, "data/sensitivity_testing/genera_covariate_interpolation_testing.csv")

#### Interpolating covariates ####
## Going with a radius of 500km. That's 10 rings for 50km cells, 5 rings for 100km cells, 3 rings for 150km cells, and just 2 rings for 200km cells
rm(list=ls())

## Read in data
master_50 <- readRDS("data/final/master_50_2_2_min3gen_min20occs_slope0.25.Rds")
master_100 <- readRDS("data/final/master_100_2_2_min3gen_min20occs_slope0.25.Rds")
master_150 <- readRDS("data/final/master_150_2_2_min3gen_min20occs_slope0.25.Rds")
master_200 <- readRDS("data/final/master_200_2_2_min3gen_min20occs_slope0.25.Rds")

## Read in function for checking and pruning cells, as well as those for interpolation
source("functions/cov.check.R")
source("functions/cov.prune.R")
source("functions/get_cell_rings.R")
source("functions/interpolate_covariates.R")

## Reconstruct original rasters used to rasterise data
rast50 <- terra::project(x = terra::rast(), y = "EPSG:8857", res = 50000)
terra::values(rast50) <- 1:terra::ncell(rast50)
rast100 <- terra::project(x = terra::rast(), y = "EPSG:8857", res = 100000)
terra::values(rast100) <- 1:terra::ncell(rast100)
rast150 <- terra::project(x = terra::rast(), y = "EPSG:8857", res = 150000)
terra::values(rast150) <- 1:terra::ncell(rast150)
rast200 <- terra::project(x = terra::rast(), y = "EPSG:8857", res = 200000)
terra::values(rast200) <- 1:terra::ncell(rast200)

## 50km - 500km radius = 10 concentric rings of cells
rings = 10
master_50_intCov <- interpolate_covariates(master_50, raster = rast50, covariate = "cellBath", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_50_intCov <- interpolate_covariates(master_50_intCov, raster = rast50, covariate = "cellLith", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_50_intCov <- interpolate_covariates(master_50_intCov, raster = rast50, covariate = "cellReef", coords = c("cellx_50km", "celly_50km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Now prune out those cells missing data
master_50_intCov <- cov.prune(data = master_50_intCov, stage_cell = "stage_cell", coords = c("cellx_50km","celly_50km"), covariates = c("cellLith", "cellBath", "cellReef"))

## Export
saveRDS(master_50_intCov, file = "data/final/master_50_2_2_min3gen_min20occs_slope0.25_covsInt.Rds")

## 100km - 500km radius = 5 concentric rings of cells
rings = 5
master_100_intCov <- interpolate_covariates(master_100, raster = rast100, covariate = "cellBath", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_100_intCov <- interpolate_covariates(master_100_intCov, raster = rast100, covariate = "cellLith", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_100_intCov <- interpolate_covariates(master_100_intCov, raster = rast100, covariate = "cellReef", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Now prune out those cells missing data
master_100_intCov <- cov.prune(data = master_100_intCov, stage_cell = "stage_cell", coords = c("cellx_100km","celly_100km"), covariates = c("cellLith", "cellBath", "cellReef"))

## Export
saveRDS(master_100_intCov, file = "data/final/master_100_2_2_min3gen_min20occs_slope0.25_covsInt.Rds")

## 150km - 500km radius = 3 concentric rings of cells
rings = 3
master_150_intCov <- interpolate_covariates(master_150, raster = rast150, covariate = "cellBath", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_150_intCov <- interpolate_covariates(master_150_intCov, raster = rast150, covariate = "cellLith", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_150_intCov <- interpolate_covariates(master_150_intCov, raster = rast150, covariate = "cellReef", coords = c("cellx_150km", "celly_150km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Now prune out those cells missing data
master_150_intCov <- cov.prune(data = master_150_intCov, stage_cell = "stage_cell", coords = c("cellx_150km","celly_150km"), covariates = c("cellLith", "cellBath", "cellReef"))

## Export
saveRDS(master_150_intCov, file = "data/final/master_150_2_2_min3gen_min20occs_slope0.25_covsInt.Rds")

## 200km - 500km radius = 2 concentric rings of cells
rings = 2
master_200_intCov <- interpolate_covariates(master_200, raster = rast200, covariate = "cellBath", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_200_intCov <- interpolate_covariates(master_200_intCov, raster = rast200, covariate = "cellLith", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_200_intCov <- interpolate_covariates(master_200_intCov, raster = rast200, covariate = "cellReef", coords = c("cellx_200km", "celly_200km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Now prune out those cells missing data
master_200_intCov <- cov.prune(data = master_200_intCov, stage_cell = "stage_cell", coords = c("cellx_200km","celly_200km"), covariates = c("cellLith", "cellBath", "cellReef"))

## Export
saveRDS(master_200_intCov, file = "data/final/master_200_2_2_min3gen_min20occs_slope0.25_covsInt.Rds")

#### Filter out stages with fewer than 5 grid cells ####
## Clean up
rm(list=ls())

## Read in data with pruned and interpolate cell covariates
master_50_covP <- readRDS("data/final/master_50_2_2_min3gen_min20occs_slope0.25_covsPruned.Rds")
master_100_covP <- readRDS("data/final/master_100_2_2_min3gen_min20occs_slope0.25_covsPruned.Rds")
master_150_covP <- readRDS("data/final/master_150_2_2_min3gen_min20occs_slope0.25_covsPruned.Rds")
master_200_covP <- readRDS("data/final/master_200_2_2_min3gen_min20occs_slope0.25_covsPruned.Rds")

master_50_covI <- readRDS("data/final/master_50_2_2_min3gen_min20occs_slope0.25_covsInt.Rds")
master_100_covI <- readRDS("data/final/master_100_2_2_min3gen_min20occs_slope0.25_covsInt.Rds")
master_150_covI <- readRDS("data/final/master_150_2_2_min3gen_min20occs_slope0.25_covsInt.Rds")
master_200_covI <- readRDS("data/final/master_200_2_2_min3gen_min20occs_slope0.25_covsInt.Rds")

## Also, read in data without covariate data filter
master_50_noCov <- readRDS("data/final/master_50_2_2_min3gen_min20occs_slope0.25.Rds")
master_100_noCov <- readRDS("data/final/master_100_2_2_min3gen_min20occs_slope0.25.Rds")
master_150_noCov <- readRDS("data/final/master_150_2_2_min3gen_min20occs_slope0.25.Rds")
master_200_noCov <- readRDS("data/final/master_200_2_2_min3gen_min20occs_slope0.25.Rds")

## Drop covariate information from cells
covsToDrop <- colnames(master_200_noCov)[c(45:47)]
master_50_noCov[,covsToDrop] <- NULL
master_100_noCov[,covsToDrop] <- NULL
master_150_noCov[,covsToDrop] <- NULL
master_200_noCov[,covsToDrop] <- NULL

## Read in stage checker and prune functions
source("functions/stage.check.R")
source("functions/stage.prune.R")

## Run the functions for each dataset
cellMin <- 5
master_50_covP_stageP <- stage.prune(data = master_50_covP, coords = c("cellx_50km", "celly_50km"), cellMin = cellMin)
master_50_covI_stageP <- stage.prune(data = master_50_covI, coords = c("cellx_50km", "celly_50km"), cellMin = cellMin)
master_50_noCov_stageP <- stage.prune(data = master_50_noCov, coords = c("cellx_50km", "celly_50km"), cellMin = cellMin)

master_100_covP_stageP <- stage.prune(data = master_100_covP, coords = c("cellx_100km", "celly_100km"), cellMin = cellMin)
master_100_covI_stageP <- stage.prune(data = master_100_covI, coords = c("cellx_100km", "celly_100km"), cellMin = cellMin)
master_100_noCov_stageP <- stage.prune(data = master_100_noCov, coords = c("cellx_100km", "celly_100km"), cellMin = cellMin)

master_150_covP_stageP <- stage.prune(data = master_150_covP, coords = c("cellx_150km", "celly_150km"), cellMin = cellMin)
master_150_covI_stageP <- stage.prune(data = master_150_covI, coords = c("cellx_150km", "celly_150km"), cellMin = cellMin)
master_150_noCov_stageP <- stage.prune(data = master_150_noCov, coords = c("cellx_150km", "celly_150km"), cellMin = cellMin)

master_200_covP_stageP <- stage.prune(data = master_200_covP, coords = c("cellx_200km", "celly_200km"), cellMin = cellMin)
master_200_covI_stageP <- stage.prune(data = master_200_covI, coords = c("cellx_200km", "celly_200km"), cellMin = cellMin)
master_200_noCov_stageP <- stage.prune(data = master_200_noCov, coords = c("cellx_200km", "celly_200km"), cellMin = cellMin)

## Export each dataset
saveRDS(master_50_covP_stageP, "data/final/final_50_genera_covsPruned.Rds")
saveRDS(master_50_covI_stageP, "data/final/final_50_genera_covsInt.Rds")
saveRDS(master_50_noCov_stageP, "data/final/final_50_genera_noCov.Rds")

saveRDS(master_100_covP_stageP, "data/final/final_100_genera_covsPruned.Rds")
saveRDS(master_100_covI_stageP, "data/final/final_100_genera_covsInt.Rds")
saveRDS(master_100_noCov_stageP, "data/final/final_100_genera_noCov.Rds")

saveRDS(master_150_covP_stageP, "data/final/final_150_genera_covsPruned.Rds")
saveRDS(master_150_covI_stageP, "data/final/final_150_genera_covsInt.Rds")
saveRDS(master_150_noCov_stageP, "data/final/final_150_genera_noCov.Rds")

saveRDS(master_200_covP_stageP, "data/final/final_200_genera_covsPruned.Rds")
saveRDS(master_200_covI_stageP, "data/final/final_200_genera_covsInt.Rds")
saveRDS(master_200_noCov_stageP, "data/final/final_200_genera_noCov.Rds")

## Create summary
## Summarise for comparison
gridCells <- c(rep("50km", 3), rep("100km", 3), rep("150km", 3), rep("200km", 3))
covariateStatus <- rep(c("original_only", "original_plus_interpolated_within_500km_radius", "none"), 4)
nCells <- rep(NA, length(gridCells))
nOccs <- rep(NA, length(gridCells))
nStages <- rep(NA, length(gridCells))
suma <- data.frame(cbind("gridCellSize" = gridCells, "covariateData" = covariateStatus, "NumberOfCells" = nCells, "NumberOfOccurrences" = nOccs, "NumberOfStages" = nStages))

## Populate cells
suma[1,"NumberOfCells"] <- length(unique(master_50_covP_stageP$stage_cell))
suma[1,"NumberOfOccurrences"] <- nrow(master_50_covP_stageP)
suma[1,"NumberOfStages"] <- length(unique(master_50_covP_stageP$stage))
suma[2,"NumberOfCells"] <- length(unique(master_50_covI_stageP$stage_cell))
suma[2,"NumberOfOccurrences"] <- nrow(master_50_covI_stageP)
suma[2,"NumberOfStages"] <- length(unique(master_50_covI_stageP$stage))
suma[3,"NumberOfCells"] <- length(unique(master_50_noCov_stageP$stage_cell))
suma[3,"NumberOfOccurrences"] <- nrow(master_50_noCov_stageP)
suma[3,"NumberOfStages"] <- length(unique(master_50_noCov_stageP$stage))

suma[4,"NumberOfCells"] <- length(unique(master_100_covP_stageP$stage_cell))
suma[4,"NumberOfOccurrences"] <- nrow(master_100_covP_stageP)
suma[4,"NumberOfStages"] <- length(unique(master_100_covP_stageP$stage))
suma[5,"NumberOfCells"] <- length(unique(master_100_covI_stageP$stage_cell))
suma[5,"NumberOfOccurrences"] <- nrow(master_100_covI_stageP)
suma[5,"NumberOfStages"] <- length(unique(master_100_covI_stageP$stage))
suma[6,"NumberOfCells"] <- length(unique(master_100_noCov_stageP$stage_cell))
suma[6,"NumberOfOccurrences"] <- nrow(master_100_noCov_stageP)
suma[6,"NumberOfStages"] <- length(unique(master_100_noCov_stageP$stage))

suma[7,"NumberOfCells"] <- length(unique(master_150_covP_stageP$stage_cell))
suma[7,"NumberOfOccurrences"] <- nrow(master_150_covP_stageP)
suma[7,"NumberOfStages"] <- length(unique(master_150_covP_stageP$stage))
suma[8,"NumberOfCells"] <- length(unique(master_150_covI_stageP$stage_cell))
suma[8,"NumberOfOccurrences"] <- nrow(master_150_covI_stageP)
suma[8,"NumberOfStages"] <- length(unique(master_150_covI_stageP$stage))
suma[9,"NumberOfCells"] <- length(unique(master_150_noCov_stageP$stage_cell))
suma[9,"NumberOfOccurrences"] <- nrow(master_150_noCov_stageP)
suma[9,"NumberOfStages"] <- length(unique(master_150_noCov_stageP$stage))

suma[10,"NumberOfCells"] <- length(unique(master_200_covP_stageP$stage_cell))
suma[10,"NumberOfOccurrences"] <- nrow(master_200_covP_stageP)
suma[10,"NumberOfStages"] <- length(unique(master_200_covP_stageP$stage))
suma[11,"NumberOfCells"] <- length(unique(master_200_covI_stageP$stage_cell))
suma[11,"NumberOfOccurrences"] <- nrow(master_200_covI_stageP)
suma[11,"NumberOfStages"] <- length(unique(master_200_covI_stageP$stage))
suma[12,"NumberOfCells"] <- length(unique(master_200_noCov_stageP$stage_cell))
suma[12,"NumberOfOccurrences"] <- nrow(master_200_noCov_stageP)
suma[12,"NumberOfStages"] <- length(unique(master_200_noCov_stageP$stage))

write.csv(suma, "data/sensitivity_testing/genera_final_numbers.csv")
