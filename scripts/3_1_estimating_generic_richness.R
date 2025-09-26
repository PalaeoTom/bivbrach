## 3.1 Estimating generic richness from grid cells
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
genera_covInt <- readRDS("data/final/final_100_genera_covsInt.Rds")
genera_covPrun <- readRDS("data/final/final_100_genera_covsPruned.Rds")
home <- getwd()

## Read in function for populating cell metadata
source("functions/extract_cell_metadata.R")

#### Create containers for richness values ####
## One template for each dataset, copied across three methods (raw, CR, CR based on slope)
## Genera, covariates original plus interpolated
gen_covOI_stageCells <- unique(genera_covInt$stage_cell)
gen_covOI_stages <- as.numeric(str_split_i(gen_covOI_stageCells, "_", 1))
gen_covOI_cells <- as.numeric(str_split_i(gen_covOI_stageCells, "_", 2))
gen_covOI_template <- data.frame(cbind("stage_cell" = gen_covOI_stageCells, "stage" = gen_covOI_stages, "cell" = gen_covOI_cells))

## Add empty containers to template
gen_covOI_template$bivalve <- NA
gen_covOI_template$brachiopod <- NA
gen_covOI_template$cellPalaeoLng <- extract_cell_metaData(genera_covInt, target = "cellx_100km", cell = "stage_cell")
gen_covOI_template$cellPalaeoLat <- extract_cell_metaData(genera_covInt, target = "celly_100km", cell = "stage_cell")
gen_covOI_template$cellLith <- extract_cell_metaData(genera_covInt, target = "cellLith", cell = "stage_cell")
gen_covOI_template$cellBath <- extract_cell_metaData(genera_covInt, target = "cellBath", cell = "stage_cell")
gen_covOI_template$cellReef <- extract_cell_metaData(genera_covInt, target = "cellReef", cell = "stage_cell")
gen_covOI_template$cellAbsLat <- abs(extract_cell_metaData(genera_covInt, target = "celly_100km", cell = "stage_cell"))
gen_covOI_template$PTME <- ""

## Add categorisation of PostPTME/PrePTME.
## Stages 1-48 are PrePTME, 49-92 are PostPTME
gen_covOI_template[which(gen_covOI_template$stage <49),"PTME"] <- "PrePTME"
gen_covOI_template[which(gen_covOI_template$stage >48),"PTME"] <- "PostPTME"

## Create containers for 3 analyses
gen_COI_raw <- gen_COI_CR20 <- gen_COI_CRV <- gen_covOI_template

## Genera, covariates original only
gen_covO_stageCells <- unique(genera_covPrun$stage_cell)
gen_covO_stages <- as.numeric(str_split_i(gen_covO_stageCells, "_", 1))
gen_covO_cells <- as.numeric(str_split_i(gen_covO_stageCells, "_", 2))
gen_covO_template <- data.frame(cbind("stage_cell" = gen_covO_stageCells, "stage" = gen_covO_stages, "cell" = gen_covO_cells))

## Add empty containers to template
gen_covO_template$bivalve <- NA
gen_covO_template$brachiopod <- NA
gen_covO_template$cellPalaeoLng <- extract_cell_metaData(genera_covPrun, target = "cellx_100km", cell = "stage_cell")
gen_covO_template$cellPalaeoLat <- extract_cell_metaData(genera_covPrun, target = "celly_100km", cell = "stage_cell")
gen_covO_template$cellLith <- extract_cell_metaData(genera_covPrun, target = "cellLith", cell = "stage_cell")
gen_covO_template$cellBath <- extract_cell_metaData(genera_covPrun, target = "cellBath", cell = "stage_cell")
gen_covO_template$cellReef <- extract_cell_metaData(genera_covPrun, target = "cellReef", cell = "stage_cell")
gen_covO_template$cellAbsLat <- abs(extract_cell_metaData(genera_covPrun, target = "celly_100km", cell = "stage_cell"))
gen_covO_template$PTME <- ""

## Add categorisation of PostPTME/PrePTME.
## Stages 1-48 are PrePTME, 49-92 are PostPTME
gen_covO_template[which(gen_covO_template$stage <49),"PTME"] <- "PrePTME"
gen_covO_template[which(gen_covO_template$stage >48),"PTME"] <- "PostPTME"

## Create containers for 3 analyses
gen_CO_raw <- gen_CO_CR20 <- gen_CO_CRV <- gen_covO_template

## Clean up environment
rm(list = setdiff(ls(), c("gen_CO_raw", "gen_CO_CR20", "gen_CO_CRV", "gen_COI_raw", "gen_COI_CR20", "gen_COI_CRV", "genera_covInt", "genera_covPrun")))

#### Calculating raw richness ####
source("functions/raw_richness.R")

## Run
CO_raw <- raw_richness(data = genera_covPrun)
COI_raw <- raw_richness(data = genera_covInt)

## Enter into final data tables
gen_CO_raw[,c(4,5)] <- CO_raw[,c(2,3)]
gen_COI_raw[,c(4,5)] <- COI_raw[,c(2,3)]

## Add sample size for each cell as final column
source("functions/raw_sample_size.R")
sample_sizes_CO <- raw_sample_size(genera_covPrun)
sample_sizes_COI <- raw_sample_size(genera_covInt)

## Match them to grid cells
gen_CO_raw$n <- NA
gen_COI_raw$n <- NA
gen_CO_raw[,"n"] <- as.numeric(sample_sizes_CO[match(sample_sizes_CO[,1],gen_CO_raw[,"stage_cell"]),2])
gen_COI_raw[,"n"] <- as.numeric(sample_sizes_COI[match(sample_sizes_COI[,1],gen_COI_raw[,"stage_cell"]),2])

## Export final data tables
write.csv(gen_CO_raw, file = "data/analysis_data/genera_CO_raw.csv")
write.csv(gen_COI_raw, file = "data/analysis_data/genera_COI_raw.csv")

#### Calculating richness using classical rarefaction with fixed subsample sizes ####
source("functions/CR_richness.R")

## Define n
nOccs = 20

## Run
CO_CR20 <- CR_richness(data = genera_covPrun, n = nOccs, n.cores = 8)
COI_CR20 <- CR_richness(data = genera_covInt, n = nOccs, n.cores = 8)

## Enter into final data tables
gen_CO_CR20[,c(4,5)] <- CO_CR20[,c(2,3)]
gen_COI_CR20[,c(4,5)] <- COI_CR20[,c(2,3)]

## Export final data tables
write.csv(gen_CO_CR20, file = "data/analysis_data/genera_CO_CR20.csv")
write.csv(gen_COI_CR20, file = "data/analysis_data/genera_COI_CR20.csv")

#### Calculating richness using classical rarefaction with variable subsample sizes ####
source("functions/CR_richness.R")

## Define n
## First, need to reload rarefaction curve data
gen_RareC <- readRDS("data/sensitivity_testing/master_100_2_2_min3gen_min20occs_RCs.Rds")[,-1]
RC_GCs <- colnames(gen_RareC)

## Split
gen_COI_RareC <- gen_RareC[,which(RC_GCs %in% unique(genera_covInt$stage_cell))]
gen_CO_RareC <- gen_RareC[,which(RC_GCs %in% unique(genera_covPrun$stage_cell))]

## Test asymptotes of cells present to make sure all have some.
source("functions/test_RC_tail_asymptote.R")
asymptote.occs <- 5
slope.threshold <- 0.25
gen_COI_RareC_bool <- test_RC_tail_asymptote(RCs = gen_COI_RareC, n = asymptote.occs, threshold = slope.threshold)
gen_CO_RareC_bool <- test_RC_tail_asymptote(RCs = gen_CO_RareC, n = asymptote.occs, threshold = slope.threshold)
names(which(!gen_COI_RareC_bool))
names(which(!gen_CO_RareC_bool))

## None - that means all have a slope of less than 0.25 at some point.
## Now to proceed to find occurrence number capping the first run of 5 occurrences to produce an RC slope below 0.25. Minimum of 20 applied.
source("functions/find_slope.R")
gen_COI_n <- find_slope(gen_COI_RareC, n = 5, min.n = 20, threshold = 0.25, n.cores = 8)
gen_CO_n <- find_slope(gen_CO_RareC, n = 5, min.n = 20, threshold = 0.25, n.cores = 8)

## Run
CO_CRV <- CR_richness(data = genera_covPrun, n = gen_CO_n, n.cores = 8)
COI_CRV <- CR_richness(data = genera_covInt, n = gen_COI_n, n.cores = 8)

## Enter into final data tables
gen_CO_CRV[,c(4,5)] <- CO_CRV[,c(2,3)]
gen_COI_CRV[,c(4,5)] <- COI_CRV[,c(2,3)]

## Match them to grid cells
gen_CO_CRV$n <- NA
gen_COI_CRV$n <- NA
gen_CO_CRV[,"n"] <- as.numeric(gen_CO_n[match(names(gen_CO_n),gen_CO_CRV[,"stage_cell"])])
gen_COI_CRV[,"n"] <- as.numeric(gen_COI_n[match(names(gen_COI_n),gen_COI_CRV[,"stage_cell"])])

## Export final data tables
write.csv(gen_CO_CRV, file = "data/analysis_data/genera_CO_CRV.csv")
write.csv(gen_COI_CRV, file = "data/analysis_data/genera_COI_CRV.csv")



