## 3.2 Estimating species richness from grid cells
## Started by TJS on 06/08/2025

#### Set up - run before any subsection ####
## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("stringr", "parallel")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(stringr)
library(parallel)

## Read in data to be analysed.
setwd("~/R_packages/bivbrach")
species_covInt <- readRDS("data/final/final_100_species_covsInt.Rds")
species_covPrun <- readRDS("data/final/final_100_species_covsPruned.Rds")
home <- getwd()

## Read in function for populating cell metadata
source("functions/extract_cell_metadata.R")

#### Create containers for richness values ####
## One template for each dataset, copied across three methods (raw, CR, CR based on slope)
## species, covariates original plus interpolated
sp_covOI_stageCells <- unique(species_covInt$stage_cell)
sp_covOI_stages <- as.numeric(str_split_i(sp_covOI_stageCells, "_", 1))
sp_covOI_cells <- as.numeric(str_split_i(sp_covOI_stageCells, "_", 2))
sp_covOI_template <- data.frame(cbind("stage_cell" = sp_covOI_stageCells, "stage" = sp_covOI_stages, "cell" = sp_covOI_cells))

## Add empty containers to template
sp_covOI_template$bivalve <- NA
sp_covOI_template$brachiopod <- NA
sp_covOI_template$cellPalaeoLng <- extract_cell_metaData(species_covInt, target = "cellx_100km", cell = "stage_cell")
sp_covOI_template$cellPalaeoLat <- extract_cell_metaData(species_covInt, target = "celly_100km", cell = "stage_cell")
sp_covOI_template$cellLith <- extract_cell_metaData(species_covInt, target = "cellLith", cell = "stage_cell")
sp_covOI_template$cellBath <- extract_cell_metaData(species_covInt, target = "cellBath", cell = "stage_cell")
sp_covOI_template$cellReef <- extract_cell_metaData(species_covInt, target = "cellReef", cell = "stage_cell")
sp_covOI_template$cellAbsLat <- abs(extract_cell_metaData(species_covInt, target = "celly_100km", cell = "stage_cell"))
sp_covOI_template$PTME <- ""

## Add categorisation of PostPTME/PrePTME.
## Stages 1-48 are PrePTME, 49-92 are PostPTME
sp_covOI_template[which(sp_covOI_template$stage <49),"PTME"] <- "PrePTME"
sp_covOI_template[which(sp_covOI_template$stage >48),"PTME"] <- "PostPTME"

## Create containers for 3 analyses
sp_COI_raw <- sp_COI_CR20 <- sp_COI_CRV <- sp_covOI_template

## species, covariates original only
sp_covO_stageCells <- unique(species_covPrun$stage_cell)
sp_covO_stages <- as.numeric(str_split_i(sp_covO_stageCells, "_", 1))
sp_covO_cells <- as.numeric(str_split_i(sp_covO_stageCells, "_", 2))
sp_covO_template <- data.frame(cbind("stage_cell" = sp_covO_stageCells, "stage" = sp_covO_stages, "cell" = sp_covO_cells))

## Add empty containers to template
sp_covO_template$bivalve <- NA
sp_covO_template$brachiopod <- NA
sp_covO_template$cellPalaeoLng <- extract_cell_metaData(species_covPrun, target = "cellx_100km", cell = "stage_cell")
sp_covO_template$cellPalaeoLat <- extract_cell_metaData(species_covPrun, target = "celly_100km", cell = "stage_cell")
sp_covO_template$cellLith <- extract_cell_metaData(species_covPrun, target = "cellLith", cell = "stage_cell")
sp_covO_template$cellBath <- extract_cell_metaData(species_covPrun, target = "cellBath", cell = "stage_cell")
sp_covO_template$cellReef <- extract_cell_metaData(species_covPrun, target = "cellReef", cell = "stage_cell")
sp_covO_template$cellAbsLat <- abs(extract_cell_metaData(species_covPrun, target = "celly_100km", cell = "stage_cell"))
sp_covO_template$PTME <- ""

## Add categorisation of PostPTME/PrePTME.
## Stages 1-48 are PrePTME, 49-92 are PostPTME
sp_covO_template[which(sp_covO_template$stage <49),"PTME"] <- "PrePTME"
sp_covO_template[which(sp_covO_template$stage >48),"PTME"] <- "PostPTME"

## Create containers for 3 analyses
sp_CO_raw <- sp_CO_CR20 <- sp_CO_CRV <- sp_covO_template

## Clean up environment
rm(list = setdiff(ls(), c("sp_CO_raw", "sp_CO_CR20", "sp_CO_CRV", "sp_COI_raw", "sp_COI_CR20", "sp_COI_CRV", "species_covInt", "species_covPrun")))

#### Calculating raw richness ####
source("functions/raw_richness.R")

## Run
CO_raw <- raw_richness(data = species_covPrun)
COI_raw <- raw_richness(data = species_covInt)

## Enter into final data tables
sp_CO_raw[,c(4,5)] <- CO_raw[,c(2,3)]
sp_COI_raw[,c(4,5)] <- COI_raw[,c(2,3)]

## Add sample size for each cell as final column
source("functions/raw_sample_size.R")
sample_sizes_CO <- raw_sample_size(species_covPrun)
sample_sizes_COI <- raw_sample_size(species_covInt)

## Match them to grid cells
sp_CO_raw$n <- NA
sp_COI_raw$n <- NA
sp_CO_raw[,"n"] <- as.numeric(sample_sizes_CO[match(sample_sizes_CO[,1],sp_CO_raw[,"stage_cell"]),2])
sp_COI_raw[,"n"] <- as.numeric(sample_sizes_COI[match(sample_sizes_COI[,1],sp_COI_raw[,"stage_cell"]),2])

## Export final data tables
write.csv(sp_CO_raw, file = "data/analysis_data/species_CO_raw.csv")
write.csv(sp_COI_raw, file = "data/analysis_data/species_COI_raw.csv")

#### Calculating richness using classical rarefaction with fixed subsample sizes ####
source("functions/CR_richness.R")

## Define n
nOccs = 20

## Run
CO_CR20 <- CR_richness(data = species_covPrun, n = nOccs, n.cores = 8)
COI_CR20 <- CR_richness(data = species_covInt, n = nOccs, n.cores = 8)

## Enter into final data tables
sp_CO_CR20[,c(4,5)] <- CO_CR20[,c(2,3)]
sp_COI_CR20[,c(4,5)] <- COI_CR20[,c(2,3)]

## Export final data tables
write.csv(sp_CO_CR20, file = "data/analysis_data/species_CO_CR20.csv")
write.csv(sp_COI_CR20, file = "data/analysis_data/species_COI_CR20.csv")

#### Calculating richness using classical rarefaction with variable subsample sizes ####
source("functions/CR_richness.R")

## Define n
## First, need to reload rarefaction curve data
sp_RareC <- readRDS("data/sensitivity_testing/master_100_2_2_min3sp_min20occs_RCs.Rds")[,-1]
RC_GCs <- colnames(sp_RareC)

## Split
sp_COI_RareC <- sp_RareC[,which(RC_GCs %in% unique(species_covInt$stage_cell))]
sp_CO_RareC <- sp_RareC[,which(RC_GCs %in% unique(species_covPrun$stage_cell))]

## Test asymptotes of cells present to make sure all have some.
source("functions/test_RC_tail_asymptote.R")
asymptote.occs <- 5
slope.threshold <- 0.25
sp_COI_RareC_bool <- test_RC_tail_asymptote(RCs = sp_COI_RareC, n = asymptote.occs, threshold = slope.threshold)
sp_CO_RareC_bool <- test_RC_tail_asymptote(RCs = sp_CO_RareC, n = asymptote.occs, threshold = slope.threshold)
names(which(!sp_COI_RareC_bool))
names(which(!sp_CO_RareC_bool))

## None - that means all have a slope of less than 0.25 at some point.
## Now to proceed to find occurrence number capping the first run of 5 occurrences to produce an RC slope below 0.25. Minimum of 20 applied.
source("functions/find_slope.R")
sp_COI_n <- find_slope(sp_COI_RareC, n = 5, min.n = 20, threshold = 0.25, n.cores = 8)
sp_CO_n <- find_slope(sp_CO_RareC, n = 5, min.n = 20, threshold = 0.25, n.cores = 8)

## Run
CO_CRV <- CR_richness(data = species_covPrun, n = sp_CO_n, n.cores = 8)
COI_CRV <- CR_richness(data = species_covInt, n = sp_COI_n, n.cores = 8)

## Enter into final data tables
sp_CO_CRV[,c(4,5)] <- CO_CRV[,c(2,3)]
sp_COI_CRV[,c(4,5)] <- COI_CRV[,c(2,3)]

## Match them to grid cells
sp_CO_CRV$n <- NA
sp_COI_CRV$n <- NA
sp_CO_CRV[,"n"] <- as.numeric(sp_CO_n[match(names(sp_CO_n),sp_CO_CRV[,"stage_cell"])])
sp_COI_CRV[,"n"] <- as.numeric(sp_COI_n[match(names(sp_COI_n),sp_COI_CRV[,"stage_cell"])])

## Export final data tables
write.csv(sp_CO_CRV, file = "data/analysis_data/species_CO_CRV.csv")
write.csv(sp_COI_CRV, file = "data/analysis_data/species_COI_CRV.csv")



