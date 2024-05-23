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
radii <- as.integer(c(200000, 500000, 1000000))
siteQuotas <- c(2, 3, 4, 5)
overlapThresholds <- 0
overlapTypes <- "sites"
weightStandardisation_1 <- F
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))

## Set input strings
input.strings <- c("stages_g200_viaTimBin","stages_g100_viaTimBin","stages_s200_viaTimBin","stages_s100_viaTimBin")

## Set output strings
SQS.output.strings <- c("stages_g200_SQS_q0_9",
                    "stages_g100_SQS_q0_9",
                    "stages_s200_SQS_q0_9",
                    "stages_s100_SQS_q0_9")

raw.output.strings <- c("stages_g200_raw",
                        "stages_g100_raw",
                        "stages_s200_raw",
                        "stages_s100_raw")

SR.output.strings <- c("stages_g200_SR_minSite",
                         "stages_g100_SR_minSite",
                         "stages_s200_SR_minSite",
                         "stages_s100_SR_minSite")

## Set taxonomic variable string
taxVar.strings <- c(rep("genus", 2), rep("unique_name", 2))

## Set input, and output directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
output.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
source("functions/get.regional.richness.R")

## Calculate richness using SQS
for(a in 1:length(input.strings)){
  get.regional.richness(input.dir = input.dir, input.pre = input.strings[a], output.dir = output.dir, output.pre = SQS.output.strings[a],
                        vars = vars, mode = "SQS", SQS.coverage = 0.9, taxa = T, n.cores = 4, taxVar = taxVar.strings[a], SQS.rareVar = "collection_no",
                        SQS.min.rareVar = 2, SQS.min.taxVar = 2, SQS.min.taxRareVar.alt = "none", SQS.noAbsence.alt = "none", SQS.exceedExtrap.alt = "none", omit.NAs = T)
}

## Calculate raw richness
for(a in 1:length(input.strings)){
  get.regional.richness(input.dir = input.dir, input.pre = input.strings[a], output.dir = output.dir, output.pre = raw.output.strings[a],
                        vars = vars, mode = "raw", taxa = T, n.cores = 4, taxVar = taxVar.strings[a], omit.NAs = T)
}

## Calculate richness using spatial subsample site rarefaction
## Define sQKey input object
sQKey <- c(2,3,4,5,2,3,4,5,2,3,4,5)

## Calculate richness
for(a in 1:length(input.strings)){
  get.regional.richness(input.dir = input.dir, input.pre = input.strings[a], output.dir = output.dir, output.pre = SR.output.strings[a],
                        vars = vars, mode = "SR", SR.rep = 100, SR.nSite = sQKey, taxa = T, n.cores = 4, taxVar = taxVar.strings[a], omit.NAs = T)
}

