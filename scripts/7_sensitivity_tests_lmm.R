## 6. Mixed effect modelling  - sensitivity tests
## Started by TJS on 04/09/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("lmerTest", "velociraptr", "sjPlot", "ggplot2", "velociraptr", "dplyr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(lmerTest)
library(velociraptr)
library(sjPlot)
library(ggplot2)
library(velociraptr)
library(dplyr)

## Load variable vectors - just looking at sites moving forward.
radii <- as.integer(c(200000, 500000, 1000000))
siteQuotas <- c(2, 3, 4, 5)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- names(vars) <- c("site_quota", "radius")

#### Sensitivity testing - taking median richness for each radially constrained region and dropping random effect ####
radii <- as.integer(c(200000, 500000, 1000000))
siteQuotas <- c(2, 3, 4, 5)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- c("site_quota", "radius")

## input strings
## Set output strings
input.strings <- c("stages_g200",
                   "stages_g100",
                   "stages_s200",
                   "stages_s100")

output.strings.med.diff <- c("stages_g200_mlm_med_diff",
                             "stages_g100_mlm_med_diff",
                             "stages_s200_mlm_med_diff",
                             "stages_s100_mlm_med_diff")

output.strings.med.brach <- c("stages_g200_mlm_med_brach",
                              "stages_g100_mlm_med_brach",
                              "stages_s200_mlm_med_brach",
                              "stages_s100_mlm_med_brach")

output.strings.med.biv <- c("stages_g200_mlm_med_biv",
                            "stages_g100_mlm_med_biv",
                            "stages_s200_mlm_med_biv",
                            "stages_s100_mlm_med_biv")

output.strings.min.diff <- c("stages_g200_mlm_min_diff",
                             "stages_g100_mlm_min_diff",
                             "stages_s200_mlm_min_diff",
                             "stages_s100_mlm_min_diff")

## Set other parameters directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dir <- "~/R_packages/bivbrach/data/mlm/sensitivity_testing/median_richness/"
source("functions/mass.mlm.on.summary.R")

#m = 1
#input.dir = input.dir
#input.pre = input.strings[m]
#output.dir = output.dir
#output.pre = output.strings.med.diff[m]
#vars = vars
#vars.values = vars.values
#mode = "median"
#unit = "diff"
#data.columns = c("Brachiopoda", "Bivalvia")

## Run for each input under each setting
for(m in 1:length(input.strings)){
  mass.mlm.on.summary(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
                      output.pre = output.strings.med.diff[m], vars = vars, vars.values = vars.values, mode = "median", unit = "diff", data.columns = c("Brachiopoda", "Bivalvia"))
}

for(m in 1:length(input.strings)){
  mass.mlm.on.summary(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
                      output.pre = output.strings.med.brach[m], vars = vars, vars.values = vars.values, mode = "median", unit = "Brachiopoda", data.columns = c("Brachiopoda", "Bivalvia"))
}

for(m in 1:length(input.strings)){
  mass.mlm.on.summary(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
                      output.pre = output.strings.med.biv[m], vars = vars, vars.values = vars.values, mode = "median", unit = "Bivalvia", data.columns = c("Brachiopoda", "Bivalvia"))
}

for(m in 1:length(input.strings)){
  mass.mlm.on.summary(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
                      output.pre = output.strings.min.diff[m], vars = vars, vars.values = vars.values, mode = "min", unit = "diff", data.columns = c("Brachiopoda", "Bivalvia"))
}

#### Sensitivity test - modelling time periods as random effects ####
radii <- as.integer(c(200000, 500000, 1000000))
siteQuotas <- c(2, 3, 4, 5)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- c("site_quota", "radius")

## input strings
## Set output strings
input.strings <- c("stages_g200",
                   "stages_g100",
                   "stages_s200",
                   "stages_s100")

output.strings.PTME <- c("stages_g200_mlm_PTME",
                         "stages_g100_mlm_PTME",
                         "stages_s200_mlm_PTME",
                         "stages_s100_mlm_PTME")

output.strings.eras <- c("stages_g200_mlm_eras",
                         "stages_g100_mlm_eras",
                         "stages_s200_mlm_eras",
                         "stages_s100_mlm_eras")

## Get stage data with times used (columns t_round and b_round)
stages <- read.csv("data/cleaned_stages.csv", row.names = 1)
stages[102,10] <- 0

## get midpoints that are used
source("functions/get.midpoints.R")
midpoints <- get.midpoints(stages[,10:11])

## Get time bins
era.cutoffs <- matrix(c(stages["Fortunian","b_round"],stages["Induan","b_round"],stages["Danian","b_round"],stages["Induan","b_round"],stages["Maastrichtian","t_round"],0), ncol = 2, nrow = 3)
colnames(era.cutoffs) <- c("bottom","top")
rownames(era.cutoffs) <- c("Paleozoic", "Mesozoic", "Cenozoic")

PTME.cutoffs <- matrix(c(stages["Fortunian","b_round"],stages["Induan","b_round"],stages["Induan","b_round"],0), ncol = 2, nrow = 2)
colnames(PTME.cutoffs) <- c("bottom","top")
rownames(PTME.cutoffs) <- c("Pre-PTME", "Post-PTME")

## Assign time values interval categories
source("functions/get.interval.mat.R")
era.interval <- get.interval.mat(as.numeric(midpoints), era.cutoffs)
PTME.interval <- get.interval.mat(as.numeric(midpoints), PTME.cutoffs)

## Set other parameters directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dir <- "~/R_packages/bivbrach/data/mlm/sensitivity_testing/time_interval_random_effect/"

## Read in function
source("functions/mass.mlm.REinterval.R")

## Run for each input
for(m in 1:length(input.strings)){
  mass.mlm.REinterval(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
                      output.pre = output.strings.PTME[m], vars = vars, vars.values = vars.values, interval = PTME.interval)
}

for(m in 1:length(input.strings)){
  mass.mlm.REinterval(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
                      output.pre = output.strings.eras[m], vars = vars, vars.values = vars.values, interval = era.interval)
}

#### Sensitivity testing - breaking up time periods ####
radii <- as.integer(c(200000, 500000, 1000000))
siteQuotas <- c(2, 3, 4, 5)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- c("site_quota", "radius")

## input strings
## Set output strings
input.strings <- c("stages_g200",
                   "stages_g100",
                   "stages_s200",
                   "stages_s100")

output.strings <- c("stages_g200_mlm",
                    "stages_g100_mlm",
                    "stages_s200_mlm",
                    "stages_s100_mlm")

## Get stage data with times used (columns t_round and b_round)
stages <- read.csv("data/cleaned_stages.csv", row.names = 1)

## Get time bins
era.cutoffs <- matrix(c(stages["Fortunian","b_round"],stages["Induan","b_round"],stages["Danian","b_round"],stages["Induan","b_round"],stages["Maastrichtian","t_round"],0), ncol = 2, nrow = 3)
colnames(era.cutoffs) <- c("bottom","top")
rownames(era.cutoffs) <- c("Paleozoic", "Mesozoic", "Cenozoic")

PTME.cutoffs <- matrix(c(stages["Fortunian","b_round"],stages["Induan","b_round"],stages["Induan","b_round"],0), ncol = 2, nrow = 2)
colnames(PTME.cutoffs) <- c("bottom","top")
rownames(PTME.cutoffs) <- c("Pre-PTME", "Post-PTME")

## Set other parameters directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dir <- "~/R_packages/bivbrach/data/mlm/sensitivity_testing/time_interval_separation/"
source("functions/mass.mlm.intervals.R")

#m = 1
#input.dir = input.dir
#input.pre = input.strings[m]
#output.dir = output.dir
#output.pre = output.strings[m]
#vars = vars
#vars.values = vars.values
#time.cutoffs <- era.cutoffs

## Run for each input
for(m in 1:length(input.strings)){
  mass.mlm.intervals(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
                     output.pre = output.strings[m], vars = vars, vars.values = vars.values, time.cutoffs = era.cutoffs)
}

for(m in 1:length(input.strings)){
  mass.mlm.intervals(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
                     output.pre = output.strings[m], vars = vars, vars.values = vars.values, time.cutoffs = PTME.cutoffs)
}


