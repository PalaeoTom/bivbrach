## 4. Species richness estimation of spatial regions
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("iNEXT")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(iNEXT)

## Load variable vectors
radii <- as.integer(c(100000, 500000, 1000000, 2000000))
siteQuotas <- c(3, 6, 9, 12, 15)
overlapThresholds <- c(0, 0.25, 0.5, 0.75, 1)
overlapTypes <- c("area", "sites")
weightStandardisation_1 <- F
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)), paste0("oTh",seq(1,length(overlapThresholds),1)), paste0("oTy",seq(1,length(overlapTypes),1)))

## Get midpoints from original data
setwd("~/R_packages/R_projects/bivbrach")
home <- getwd()
genera.10ma <- readRDS("data/BB_genera_10maBins.Rds")
genera.stages <- readRDS("data/BB_genera_stageBins.Rds")
species.stages <- readRDS("data/BB_species_stageBins.Rds")
times.10ma <- names(genera.10ma)
times.stag <- names(genera.stages)

## Set working directory, input, and output directories
setwd("~/R_packages/R_projects/bivbrach")
home <- getwd()
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
output.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
source("functions/get.regional.richness.R")

## Run function on each of four datasets
get.regional.richness(input.dir = input.dir, input.pre = "BB_gen_stag_raw_viableTimeBins", output.dir = output.dir, output.pre = "BB_gen_stag_raw_SQS",
                      vars = vars, times = times.stag, SQS = T, coverage = 0.9, taxa = T, n.cores = 4, taxVar = "genus", rareVar = "collection_no",
                      min.rareVar = 2, min.taxVar = 2, alt.value = "none")

get.regional.richness(input.dir = input.dir, input.pre = "BB_gen_10ma_raw_viableTimeBins", output.dir = output.dir, output.pre = "BB_gen_10ma_raw_SQS",
                      vars = vars, times = times.stag, SQS = T, coverage = 0.9, taxa = T, n.cores = 4, taxVar = "genus", rareVar = "collection_no",
                      min.rareVar = 2, min.taxVar = 2, alt.value = "none")

get.regional.richness(input.dir = input.dir, input.pre = "BB_spec_stag_raw_viableTimeBins", output.dir = output.dir, output.pre = "BB_spec_stag_raw_SQS",
                      vars = vars, times = times.stag, SQS = T, coverage = 0.9, taxa = T, n.cores = 4, taxVar = "unique_name", rareVar = "collection_no",
                      min.rareVar = 2, min.taxVar = 2, alt.value = "none")

get.regional.richness(input.dir = input.dir, input.pre = "BB_spec_10ma_raw_viableTimeBins", output.dir = output.dir, output.pre = "BB_spec_10ma_raw_SQS",
                      vars = vars, times = times.stag, SQS = T, coverage = 0.9, taxa = T, n.cores = 4, taxVar = "unique_name", rareVar = "collection_no",
                      min.rareVar = 2, min.taxVar = 2, alt.value = "none")





