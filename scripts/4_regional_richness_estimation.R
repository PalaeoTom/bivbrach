## 4. Species richness estimation of spatial regions
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("iNEXT", "parallel")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(iNEXT)
library(parallel)

## Set working directory
setwd("~/R_packages/R_projects/bivbrach")
home <- getwd()

## Load variable vectors
radii <- as.integer(c(100000, 200000, 250000, 500000))
siteQuotas <- c(3, 6, 9, 12, 15)
overlapThresholds <- c(0, 0.25, 0.5, 0.75, 1)
overlapTypes <- c("sites")
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)), paste0("oTh",seq(1,length(overlapThresholds),1)), "oTy2")

## Set input strings
input.strings <- c("bin10_g200_raw_viableTimeBins","bin10_g100_raw_viableTimeBins","bin10_g50_raw_viableTimeBins","bin10_g25_raw_viableTimeBins","bin10_s200_raw_viableTimeBins","bin10_s100_raw_viableTimeBins","bin10_s50_raw_viableTimeBins","bin10_s25_raw_viableTimeBins",
                    "stages_g200_raw_viableTimeBins","stages_g100_raw_viableTimeBins","stages_g50_raw_viableTimeBins","stages_g25_raw_viableTimeBins","stages_s200_raw_viableTimeBins","stages_s100_raw_viableTimeBins","stages_s50_raw_viableTimeBins","stages_s25_raw_viableTimeBins")

output.strings <- c("bin10_g200_SQS_q0_9",
                    "bin10_g100_SQS_q0_9",
                    "bin10_g50_SQS_q0_9",
                    "bin10_g25_SQS_q0_9",
                    "bin10_s200_SQS_q0_9",
                    "bin10_s100_SQS_q0_9",
                    "bin10_s50_SQS_q0_9",
                    "bin10_s25_SQS_q0_9",
                    "stages_g200_SQS_q0_9",
                    "stages_g100_SQS_q0_9",
                    "stages_g50_SQS_q0_9",
                    "stages_g25_SQS_q0_9",
                    "stages_s200_SQS_q0_9",
                    "stages_s100_SQS_q0_9",
                    "stages_s50_SQS_q0_9",
                    "stages_s25_SQS_q0_9")

taxVar.strings <- c(rep("genus", 4), rep("unique_name", 4),rep("genus", 4), rep("unique_name", 4))

## Set input, and output directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
output.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
source("functions/get.regional.richness.R")

## Run function on each dataset
for(a in 1:length(input.strings)){
  get.regional.richness(input.dir = input.dir, input.pre = input.strings[a], output.dir = output.dir, output.pre = output.strings[a],
                        vars = vars, SQS = T, coverage = 0.9, taxa = T, n.cores = 4, taxVar = taxVar.strings[a], rareVar = "collection_no",
                        min.rareVar = 2, min.taxVar = 2, min.taxRareVar.alt = "none", noAbsence.alt = "none", exceedExtrap.alt = "none", omit.NAs = T)
}

## Now to run analysis and calculate raw richness
output.strings.raw <- c("bin10_g200_raw_richness",
                    "bin10_g100_raw_richness",
                    "bin10_g50_raw_richness",
                    "bin10_g25_raw_richness",
                    "bin10_s200_raw_richness",
                    "bin10_s100_raw_richness",
                    "bin10_s50_raw_richness",
                    "bin10_s25_raw_richness",
                    "stages_g200_raw_richness",
                    "stages_g100_raw_richness",
                    "stages_g50_raw_richness",
                    "stages_g25_raw_richness",
                    "stages_s200_raw_richness",
                    "stages_s100_raw_richness",
                    "stages_s50_raw_richness",
                    "stages_s25_raw_richness")

## Run function again
for(a in 1:length(input.strings)){
  get.regional.richness(input.dir = input.dir, input.pre = input.strings[a], output.dir = output.dir, output.pre = output.strings.raw[a],
                        vars = vars, SQS = F, coverage = 0.9, taxa = T, n.cores = 4, taxVar = taxVar.strings[a], rareVar = "collection_no",
                        min.rareVar = 2, min.taxVar = 2, min.taxRareVar.alt = "none", noAbsence.alt = "none", exceedExtrap.alt = "none", omit.NAs = T)
}


