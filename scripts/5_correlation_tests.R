## 5. Mixed effect modelling
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("lmerTest")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(lmerTest)

## Load variable vectors - just looking at sites moving forward.
radii <- as.integer(c(100000, 200000, 250000, 500000))
siteQuotas <- c(3, 6, 9, 12, 15)
overlapThresholds <- c(0, 0.25, 0.5, 0.75, 1)
overlapTypes <- "sites"
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)), paste0("oTh",seq(1,length(overlapThresholds),1)), "oTy2")
vars.values <- list(siteQuotas, radii, overlapThresholds, overlapTypes)
names(vars.values) <- c("site_quota", "radius", "overlap_threshold", "overlap_type")

## input strings
input.strings <- c("bin10_g200_SQS_q0_9",
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

output.strings <- c("bin10_g200_SQS_q0_9_mlm",
                   "bin10_g100_SQS_q0_9_mlm",
                   "bin10_g50_SQS_q0_9_mlm",
                   "bin10_g25_SQS_q0_9_mlm",
                   "bin10_s200_SQS_q0_9_mlm",
                   "bin10_s100_SQS_q0_9_mlm",
                   "bin10_s50_SQS_q0_9_mlm",
                   "bin10_s25_SQS_q0_9_mlm",
                   "stages_g200_SQS_q0_9_mlm",
                   "stages_g100_SQS_q0_9_mlm",
                   "stages_g50_SQS_q0_9_mlm",
                   "stages_g25_SQS_q0_9_mlm",
                   "stages_s200_SQS_q0_9_mlm",
                   "stages_s100_SQS_q0_9_mlm",
                   "stages_s50_SQS_q0_9_mlm",
                   "stages_s25_SQS_q0_9_mlm")

## Set working directory
setwd("~/R_packages/R_projects/bivbrach")
home <- getwd()

## Set other parameters directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dir <- "~/R_packages/R_projects/bivbrach/data"
source("functions/mass.mlm.R")

## Run for each input
for(i in 1:length(input.strings)){
  mass.mlm(input.dir = input.dir, input.pre = input.strings[i], output.dir = output.dir,
           output.pre = output.strings[i], vars = vars, vars.values = vars.values)
}
