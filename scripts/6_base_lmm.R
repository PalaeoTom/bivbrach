## 6. Mixed effect modelling
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("lme4", "velociraptr", "sjPlot", "ggplot2", "velociraptr", "dplyr", "cowplot")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(lme4)
library(velociraptr)
library(sjPlot)
library(ggplot2)
library(velociraptr)
library(dplyr)
library(cowplot)

#### Modelling ####
## Load variable vectors - just looking at sites moving forward.
radii <- as.integer(c(1000000, 500000))
siteQuotas <- c(2, 4)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- names(vars) <- c("site_quota", "radius")

## input strings
## Set output strings
input.strings <- c("stages_g200",
                   "stages_s200",
                   "stages_g200_epif",
                   "stages_s200_epif",
                   "stages_g200_ref",
                   "stages_s200_ref")

output.strings.full <- c("stages_g200_full_lmm",
                    "stages_s200_full_lmm",
                    "stages_g200_epif_full_lmm",
                    "stages_s200_epif_full_lmm",
                    "stages_g200_ref_full_lmm",
                    "stages_s200_ref_full_lmm")

output.strings.simple <- c("stages_g200_simple_lmm",
                         "stages_s200_simple_lmm",
                         "stages_g200_epif_simple_lmm",
                         "stages_s200_epif_simple_lmm",
                         "stages_g200_ref_simple_lmm",
                         "stages_s200_ref_simple_lmm")

output.dirs <- c("~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/base",
                      "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/base",
                      "~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/epifaunal",
                      "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/epifaunal",
                      "~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/reference",
                      "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/reference")

## Set working directory
setwd("~/R_packages/bivbrach")
home <- getwd()

## Set other parameters directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
source("functions/mass.lmm.R")

## mass.mlm arguments
#m = 1
#input.dir = input.dir
#input.pre = input.strings[m]
#output.dir = output.dirs[m]
#output.pre = output.strings.full[m]
#vars = vars
#vars.values = vars.values
#type = "full"
#i = 1

## Run for each input
## just bivalve predictor and time/subregion random effects
for(m in 1:length(input.strings)){
  mass.lmm(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dirs[m],
                output.pre = output.strings.simple[m], vars = vars, vars.values = vars.values, type = "simple")
}

## Full model
for(m in 1:length(input.strings)){
  mass.lmm(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dirs[m],
           output.pre = output.strings.full[m], vars = vars, vars.values = vars.values, type = "full")
}

#### Testing model assumptions ####
rm(list = ls())

## Load variable vectors - just looking at sites moving forward.
radii <- as.integer(c(1000000, 500000))
siteQuotas <- c(2, 4)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- names(vars) <- c("site_quota", "radius")

## Load input and output strings
input.strings <- c("stages_g200",
                   "stages_s200",
                   "stages_g200_epif",
                   "stages_s200_epif",
                   "stages_g200_ref",
                   "stages_s200_ref")

input.dirs <- c("~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/base",
                 "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/base",
                 "~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/epifaunal",
                 "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/epifaunal",
                 "~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/reference",
                 "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/reference")

output.dirs.full <- c("~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/base/full",
                      "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/base/full",
                      "~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/epifaunal/full",
                      "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/epifaunal/full",
                      "~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/reference/full",
                      "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/reference/full")

output.dirs.simple <- c("~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/base/simple",
                        "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/base/simple",
                        "~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/epifaunal/simple",
                        "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/epifaunal/simple",
                        "~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/reference/simple",
                        "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/reference/simple")

## Define plot title strings
input.names <- c("sQ1_r1", "sQ2_r2")
plot.names <- c("2 sites, 1000km radius", "4 sites, 500km radius")
title.strings <- cbind(input.names,plot.names)

## Arguments for function
#r = 1
#input.string = input.strings[r]
#model.type = "full"
#argument.strings = title.strings
#input.dir = input.dirs[r]
#output.dir <- output.dirs.full[r]
#i = 1

## Read in function
source("functions/visualise.model.assumption.R")

## Run the function
for(r in 1:length(input.strings)){
  visualise.model.assumption(input.string = input.strings[r], model.type = "full", argument.strings = title.strings, input.dir = input.dirs[r], output.dir = output.dirs.full[r])
}

for(r in 1:length(input.strings)){
  visualise.model.assumption(input.string = input.strings[r], model.type = "simple", argument.strings = title.strings, input.dir = input.dirs[r], output.dir = output.dirs.simple[r])
}

#### Plotting lmm with sjplot ####
## Define plot title strings
input.names <- c("sQ1_r1", "sQ2_r2")
plot.names <- c("2 sites, 1000km radius", "4 sites, 500km radius")
title.strings <- cbind(input.names,plot.names)

## Download period colour palettes
periods <- downloadTime("international periods")
periods <- periods[order(periods$b_age, decreasing=TRUE), ]
periods <- periods[periods$b_age <= periods[which(rownames(periods) == "Cambrian"),"b_age"],c(2,4,5,8)]
periods[,"shape"] <- c(rep(16, 6), rep(15, 3), rep(17, 3))

## Download era shape palette
eras <- downloadTime("international eras")[1:3,]
eras <- eras[order(eras$b_age, decreasing = TRUE),c(2,4,5)]
eras[,"shape"] <- c(16,15,17)

## define input and output strings
input.strings.full <- input.strings.simple <- c("stages_g200",
                   "stages_s200",
                   "stages_g200_epif",
                   "stages_s200_epif",
                   "stages_g200_ref",
                   "stages_s200_ref")

model.input.dirs <- c("~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/base",
                                     "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/base",
                                     "~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/epifaunal",
                                     "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/epifaunal",
                                     "~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/reference",
                                     "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/reference")

output.dirs.full <- c("~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/base/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/base/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/epifaunal/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/epifaunal/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/reference/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/reference/full")

output.dirs.simple <- c("~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/base/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/base/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/epifaunal/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/epifaunal/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/reference/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/reference/simple")

## Function
source("functions/mass.SJplot.R")
## Run function for all
rich.input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"

#r = 1
#input.string = input.strings.full[r]
#model.type = "full"
#argument.strings = title.strings
#model.input.dir = model.input.dirs[r]
#rich.input.dir = rich.input.dir
#output.dir <- output.dirs.full[r]
#times.col = "times"
#period.scale = periods
#era.scale = eras
#xy = c("Bivalvia", "Brachiopoda")
#time.cutoffs = NULL
#min.sample = 20
#s = 1
#i = 1

for(r in 1:length(input.strings.full)){
 mass.SJplot(input.strings.full[r], model.type = "full", title.strings, model.input.dirs[r], rich.input.dir, output.dirs.full[r], times.col = "times", period.scale = periods, era.scale = eras, xy = c("Bivalvia", "Brachiopoda"))
}

for(r in 1:length(input.strings.simple)){
  mass.SJplot(input.strings.simple[r], model.type = "simple", title.strings, model.input.dirs[r], rich.input.dir, output.dirs.simple[r], times.col = "times", period.scale = periods, era.scale = eras, xy = c("Bivalvia", "Brachiopoda"))
}
