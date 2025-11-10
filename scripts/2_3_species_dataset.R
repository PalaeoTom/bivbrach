## 2.3 Generating a species dataset
## Started by TJS on 08/01/2024

## If packages aren't installed, install them, then load them
packages <- c("divvy", "stringr", "fossilbrush")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divvy)
library(stringr)
library(fossilbrush)
library(terra)
library(parallel)

## Clean directory
rm(list = ls())

## Read in functions
source("functions/misspell.R")

#### Rolling with 100km only ####
## Read in data after time binning and covariate data has been calculated but standardisation has not been conducted
master_100 <- readRDS("data/final/master_100_2_2.Rds")

#### Updating combined name to be species based ####
## Clean up names
master_100$species <- clean_name(master_100$species)

## Drop NAs
master_100 <- master_100[-which(is.na(master_100$species)),]

## Standardise dipthongs
master_100$species <- misspell(master_100$species)

## Check for punctuation
any(str_detect(master_100$species, pattern = "[:punct:]"))

## Capitalisation
master_100$species <- str_to_lower(master_100$species)

## Now to combine into a single unique name to update combined name
master_100$combined_name <- apply(master_100, 1, function(x) paste0(x[5], "_", x[9], "_", x[10]))

#### Apply quality criteria ####
source("functions/standardiseCells.R")

## 3+ species
sp.min <- 3
master_100_3sp <- standardiseCells(master_100, cell = "stage_cell", type = "taxa", minOccs = sp.min)

## 20+ occurrences
occs.min <- 20
master_100_3sp_20occs <- standardiseCells(data = master_100_3sp, cell = "stage_cell", minOccs = occs.min, type = "occs")

## Get rarefaction curves for all cells
#source("functions/rarefaction_curve.R")
#source("functions/rarefaction_curve_all_cells.R")
#master_100_RCs <- rarefaction_curve_all_cells(data = master_100_3sp_20occs, cell = "stage_cell", taxVar = "combined_name", iter = 10000)
#saveRDS(master_100_RCs, file = "data/sensitivity_testing/master_100_2_2_min3sp_min20occs_RCs.Rds")

## Wittle down to grid cells that meet asymptote criteria
## Read in Rarefaction curves
master_100_RCs <- readRDS("data/sensitivity_testing/master_100_2_2_min3sp_min20occs_RCs.Rds")
asymptote.occs <- 5
slope.threshold <- 0.25
source("functions/test_RC_tail_asymptote.R")

## Get grid cells to retain
master_100_RCs_bool <- test_RC_tail_asymptote(RCs = master_100_RCs[,-1], n = asymptote.occs, threshold = slope.threshold)
master_100_GCs <- colnames(master_100_RCs)[-1]
master_100_GCs <- master_100_GCs[master_100_RCs_bool]

## Wittle down
master_100_3sp_20occs_RCAsym <- master_100_3sp_20occs[which(master_100_3sp_20occs$stage_cell %in% master_100_GCs),]

## Covariates - prune and interpolate
source("functions/cov.check.R")
source("functions/cov.prune.R")
master_100_3sp_20occs_RCAsym_covPruned <- cov.prune(data = master_100_3sp_20occs_RCAsym, stage_cell = "stage_cell", coords = c("cellx_100km","celly_100km"), covariates = c("cellLith", "cellBath", "cellReef"))

## Get raster for interpolation
rast100 <- terra::project(x = terra::rast(), y = "EPSG:8857", res = 100000)
terra::values(rast100) <- 1:terra::ncell(rast100)

## Interpolate:
source("functions/get_cell_rings.R")
source("functions/interpolate_covariates.R")
rings = 5
master_100_3sp_20occs_RCAsym_covInt <- interpolate_covariates(master_100_3sp_20occs_RCAsym, raster = rast100, covariate = "cellBath", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_100_3sp_20occs_RCAsym_covInt <- interpolate_covariates(master_100_3sp_20occs_RCAsym_covInt, raster = rast100, covariate = "cellLith", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)
master_100_3sp_20occs_RCAsym_covInt <- interpolate_covariates(master_100_3sp_20occs_RCAsym_covInt, raster = rast100, covariate = "cellReef", coords = c("cellx_100km", "celly_100km"), stage_cell = "stage_cell", max_ring = rings, n.cores = 8)

## Now prune out those cells missing data
master_100_3sp_20occs_RCAsym_covInt <- cov.prune(data = master_100_3sp_20occs_RCAsym_covInt, stage_cell = "stage_cell", coords = c("cellx_100km","celly_100km"), covariates = c("cellLith", "cellBath", "cellReef"))

## Create no covariate version
master_100_3sp_20occs_RCAsym_noCov <- master_100_3sp_20occs_RCAsym
master_100_3sp_20occs_RCAsym_noCov[colnames(master_100_3sp_20occs_RCAsym_noCov)[c(45:47)]] <- NULL

## Finally, prune out stages with less than 5 cells
source("functions/stage.check.R")
source("functions/stage.prune.R")

## Run the functions for each dataset
cellMin <- 5
master_100_covP_stageP <- stage.prune(data = master_100_3sp_20occs_RCAsym_covPruned, coords = c("cellx_100km", "celly_100km"), cellMin = cellMin)
master_100_covI_stageP <- stage.prune(data = master_100_3sp_20occs_RCAsym_covInt, coords = c("cellx_100km", "celly_100km"), cellMin = cellMin)
master_100_noCov_stageP <- stage.prune(data = master_100_3sp_20occs_RCAsym_noCov, coords = c("cellx_100km", "celly_100km"), cellMin = cellMin)

## Export final datasets
saveRDS(master_100_covP_stageP, "data/final/final_100_species_covsPruned.Rds")
saveRDS(master_100_covI_stageP, "data/final/final_100_species_covsInt.Rds")
saveRDS(master_100_noCov_stageP, "data/final/final_100_species_noCov.Rds")

## Summarise numbers
gridCells <- rep("100km", 3)
covariateStatus <-c("original_only", "original_plus_interpolated_within_500km_radius", "none")
nCells <- rep(NA, length(gridCells))
nOccs <- rep(NA, length(gridCells))
nStages <- rep(NA, length(gridCells))
suma <- data.frame(cbind("gridCellSize" = gridCells, "covariateData" = covariateStatus, "NumberOfCells" = nCells, "NumberOfOccurrences" = nOccs, "NumberOfStages" = nStages))

## Populate cells
suma[1,"NumberOfCells"] <- length(unique(master_100_covP_stageP$stage_cell))
suma[1,"NumberOfOccurrences"] <- nrow(master_100_covP_stageP)
suma[1,"NumberOfStages"] <- length(unique(master_100_covP_stageP$stage))
suma[2,"NumberOfCells"] <- length(unique(master_100_covI_stageP$stage_cell))
suma[2,"NumberOfOccurrences"] <- nrow(master_100_covI_stageP)
suma[2,"NumberOfStages"] <- length(unique(master_100_covI_stageP$stage))
suma[3,"NumberOfCells"] <- length(unique(master_100_noCov_stageP$stage_cell))
suma[3,"NumberOfOccurrences"] <- nrow(master_100_noCov_stageP)
suma[3,"NumberOfStages"] <- length(unique(master_100_noCov_stageP$stage))

write.csv(suma, "data/sensitivity_testing/species_final_numbers.csv")

