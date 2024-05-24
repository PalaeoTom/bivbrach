## 4. Species richness estimation of spatial regions
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("iNEXT", "parallel", "velociraptr", "dplyr", "plyr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(iNEXT)
library(parallel)
library(velociraptr)
library(dplyr)
library(plyr)

## Set working directory
setwd("~/R_packages/R_projects/bivbrach")
home <- getwd()

## Load variable vectors
radii <- as.integer(c(200000, 500000, 1000000))
siteQuotas <- c(2, 3, 4, 5)
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

#### Plotting bivalve versus brachiopod richness ####
## Define directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dir <- "/Users/tjs/R_packages/R_projects/bivbrach/figures"

## Define variables and labels
radii <- as.integer(c(200000, 500000, 1000000))
siteQuotas <- c(2, 3, 4, 5)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.label <- list(paste0(seq(2, 5, 1), " site minima"), paste0(c(200, 500, 1000), "km radius"))

## Define colour palette for periods
periods <- downloadTime("international periods")
periods <- periods[order(periods$b_age, decreasing=TRUE), ]
periods <- periods[periods$b_age <= periods[which(rownames(periods) == "Cambrian"),"b_age"],c(2,4,5,8)]

## Set legend position
legend.position = c("topright", "topright", "topright", "topright")

## Define geo.scale object, which specifies colours and point shapes for different time bins
geo.scale <- cbind(periods, "shape" = c(rep(16, 6), rep(15, 3), rep(17, 3)))

## Set names of plotting columns and columns containing time data
plotting.col <- c("Bivalvia", "Brachiopoda")
times.col <- "times"

## Define input and output strings
SQS.input.pre <- c("stages_g200_SQS_q0_9",
               "stages_g100_SQS_q0_9",
               "stages_s200_SQS_q0_9",
               "stages_s100_SQS_q0_9")

SQS.output.pre <- c("stages_g200_SQS_q0_9",
                "stages_g100_SQS_q0_9",
                "stages_s200_SQS_q0_9",
                "stages_s100_SQS_q0_9")

SQS.output.title <- c("Genera, 200km grid cells, SQS (q = 0.9)",
                   "Genera, 100km grid cells, SQS (q = 0.9)",
                   "Species, 200km grid cells, SQS (q = 0.9)",
                   "Species, 100km grid cells, SQS (q = 0.9)")

raw.input.pre <- c("stages_g200_raw",
                   "stages_g100_raw",
                   "stages_s200_raw",
                   "stages_s100_raw")

raw.output.pre <- c("stages_g200_raw",
                    "stages_g100_raw",
                    "stages_s200_raw",
                    "stages_s100_raw")

raw.output.title <- c("Genera, 200km grid cells, raw",
                      "Genera, 100km grid cells, raw",
                      "Species, 200km grid cells, raw",
                      "Species, 100km grid cells, raw")

SR.input.pre <- c("stages_g200_SR",
                   "stages_g100_SR",
                   "stages_s200_SR",
                   "stages_s100_SR")

SR.output.pre <- c("stages_g200_SR",
                    "stages_g100_SR",
                    "stages_s200_SR",
                    "stages_s100_SR")

SR.output.title <- c("Genera, 200km grid cells, SR (minima sampled)",
                      "Genera, 100km grid cells, SR (minima sampled)",
                      "Species, 200km grid cells, SR (minima sampled)",
                      "Species, 100km grid cells, SR (minima sampled)")

## Read in function
source("functions/plot.richness.R")

## Run plotting function for SQS
for(a in 1:length(SQS.input.pre)){
  plot.richness(input.dir, input.pre = SQS.input.pre[a], output.dir, output.pre = SQS.output.pre[a], output.title = SQS.output.title[a],
                vars, vars.label, plotting.col, times.col, geo.scale, legend.position = legend.position[a])
}

## Run plotting function for raw
for(a in 1:length(raw.input.pre)){
  plot.richness(input.dir, input.pre = raw.input.pre[a], output.dir, output.pre = raw.output.pre[a], output.title = raw.output.title[a],
                vars, vars.label, plotting.col, times.col, geo.scale, legend.position = legend.position[a])
}

## Run plotting function for SR
for(a in 1:length(SR.input.pre)){
  plot.richness(input.dir, input.pre = SR.input.pre[a], output.dir, output.pre = SR.output.pre[a], output.title = SR.output.title[a],
                vars, vars.label, plotting.col, times.col, geo.scale, legend.position = legend.position[a])
}
