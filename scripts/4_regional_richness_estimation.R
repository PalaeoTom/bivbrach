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

## Set working directory
setwd("~/R_packages/R_projects/bivbrach")
home <- getwd()

## Load variable vectors
radii <- as.integer(c(100000, 200000, 250000, 500000))
siteQuotas <- c(3, 6, 9, 12, 15)
overlapThresholds <- c(0, 0.25, 0.5, 0.75, 1)
overlapTypes <- c("area", "sites")
weightStandardisation_1 <- F
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)), paste0("oTh",seq(1,length(overlapThresholds),1)), paste0("oTy",seq(1,length(overlapTypes),1)))

## Set input strings
input.strings <- c("bin10_g200_raw_viableTimeBins","bin10_g100_raw_viableTimeBins","bin10_g50_raw_viableTimeBins","bin10_g25_raw_viableTimeBins","bin10_s200_raw_viableTimeBins","bin10_s100_raw_viableTimeBins","bin10_s50_raw_viableTimeBins","bin10_s25_raw_viableTimeBins",
                    "stages_g200_raw_viableTimeBins","stages_g100_raw_viableTimeBins","stages_g50_raw_viableTimeBins","stages_g25_raw_viableTimeBins","stages_s200_raw_viableTimeBins","stages_s100_raw_viableTimeBins","stages_s50_raw_viableTimeBins","stages_s25_raw_viableTimeBins")

output.strings <- c("bin10_g200_raw_SQSq0.9",
                    "bin10_g100_raw_SQSq0.9",
                    "bin10_g50_raw_SQSq0.9",
                    "bin10_g25_raw_SQSq0.9",
                    "bin10_s200_raw_SQSq0.9",
                    "bin10_s100_raw_SQSq0.9",
                    "bin10_s50_raw_SQSq0.9",
                    "bin10_s25_raw_SQSq0.9",
                    "stages_g200_raw_SQSq0.9",
                    "stages_g100_raw_SQSq0.9",
                    "stages_g50_raw_SQSq0.9",
                    "stages_g25_raw_SQSq0.9",
                    "stages_s200_raw_SQSq0.9",
                    "stages_s100_raw_SQSq0.9",
                    "stages_s50_raw_SQSq0.9",
                    "stages_s25_raw_SQSq0.9")


## Set input, and output directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
output.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
source("functions/get.regional.richness.R")

## Run function on each of four datasets
for(a in 1:length(input.strings)){
  get.regional.richness(input.dir = input.dir, input.pre = input.strings[a], output.dir = output.dir, output.pre = output.strings[a],
                        vars = vars, SQS = T, coverage = 0.9, taxa = T, n.cores = 4, taxVar = "genus", rareVar = "collection_no",
                        min.rareVar = 2, min.taxVar = 2, min.taxRareVar.alt = "none", noAbsence.alt = "none", exceedExtrap.alt = "none", omit.NAs = T)
}
