## 6. Mixed effect modelling
## Started by TJS on 08/01/2024

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

## input strings
## Set output strings
input.strings <- c("stages_g200",
                   "stages_g100",
                   "stages_s200",
                   "stages_s100",
                   "stages_g200_epif",
                   "stages_g100_epif",
                   "stages_s200_epif",
                   "stages_s100_epif",
                   "stages_g200_inf",
                   "stages_g100_inf",
                   "stages_s200_inf",
                   "stages_s100_inf",
                   "stages_g200_sitesThenRefs",
                   "stages_g100_sitesThenRefs",
                   "stages_s200_sitesThenRefs",
                   "stages_s100_sitesThenRefs")

output.strings.full <- c("stages_g200_full_lmm",
                    "stages_g100_full_lmm",
                    "stages_s200_full_lmm",
                    "stages_s100_full_lmm",
                    "stages_g200_epif_full_lmm",
                    "stages_g100_epif_full_lmm",
                    "stages_s200_epif_full_lmm",
                    "stages_s100_epif_full_lmm",
                    "stages_g200_inf_full_lmm",
                    "stages_g100_inf_full_lmm",
                    "stages_s200_inf_full_lmm",
                    "stages_s100_inf_full_lmm",
                    "stages_g200_sitesThenRefs_full_lmm",
                    "stages_g100_sitesThenRefs_full_lmm",
                    "stages_s200_sitesThenRefs_full_lmm",
                    "stages_s100_sitesThenRefs_full_lmm")

output.strings.simple <- c("stages_g200_simple_lmm",
                         "stages_g100_simple_lmm",
                         "stages_s200_simple_lmm",
                         "stages_s100_simple_lmm",
                         "stages_g200_epif_simple_lmm",
                         "stages_g100_epif_simple_lmm",
                         "stages_s200_epif_simple_lmm",
                         "stages_s100_epif_simple_lmm",
                         "stages_g200_inf_simple_lmm",
                         "stages_g100_inf_simple_lmm",
                         "stages_s200_inf_simple_lmm",
                         "stages_s100_inf_simple_lmm",
                         "stages_g200_sitesThenRefs_simple_lmm",
                         "stages_g100_sitesThenRefs_simple_lmm",
                         "stages_s200_sitesThenRefs_simple_lmm",
                         "stages_s100_sitesThenRefs_simple_lmm")

output.dirs <- c("~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/base/",
                      "~/R_packages/bivbrach/data/lmm/basic/genera_100km_cells/base/",
                      "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/base/",
                      "~/R_packages/bivbrach/data/lmm/basic/species_100km_cells/base/",
                      "~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/epifaunal/",
                      "~/R_packages/bivbrach/data/lmm/basic/genera_100km_cells/epifaunal/",
                      "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/epifaunal/",
                      "~/R_packages/bivbrach/data/lmm/basic/species_100km_cells/epifaunal/",
                      "~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/infaunal/",
                      "~/R_packages/bivbrach/data/lmm/basic/genera_100km_cells/infaunal/",
                      "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/infaunal/",
                      "~/R_packages/bivbrach/data/lmm/basic/species_100km_cells/infaunal/",
                      "~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/sitesThenRefs/",
                      "~/R_packages/bivbrach/data/lmm/basic/genera_100km_cells/sitesThenRefs/",
                      "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/sitesThenRefs/",
                      "~/R_packages/bivbrach/data/lmm/basic/species_100km_cells/sitesThenRefs/")

## Set working directory
setwd("~/R_packages/bivbrach")
home <- getwd()

## Set other parameters directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
source("functions/mass.full.lmm.R")
source("functions/mass.simple.lmm.R")

## mass.mlm arguments
#m = 1
#input.dir = input.dir
#input.pre = input.strings[m]
#output.dir = output.dir
#output.pre = output.strings[m]
#vars = vars
#vars.values = vars.values
#i = 1

## Run for each input
for(m in 1:length(input.strings)){
  mass.simple.lmm(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dirs[m],
                output.pre = output.strings.simple[m], vars = vars, vars.values = vars.values)
}

for(m in 1:length(input.strings)){
  mass.full.lmm(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dirs[m],
           output.pre = output.strings.full[m], vars = vars, vars.values = vars.values)
}

#### Testing model assumptions ####



#### Plotting lmm with sjplot ####
## Define plot title strings
input.names <- c("sQ1_r1", "sQ2_r1", "sQ3_r1", "sQ4_r1",
                 "sQ1_r2", "sQ2_r2", "sQ3_r2", "sQ4_r2",
                 "sQ1_r3", "sQ2_r3", "sQ3_r3", "sQ4_r3")
plot.names <- c("2 sites, 200km radius", "3 sites, 200km radius", "4 sites, 200km radius", "5 sites, 200km radius",
                "2 sites, 500km radius", "3 sites, 500km radius", "4 sites, 500km radius", "5 sites, 500km radius",
                "2 sites, 1000km radius", "3 sites, 1000km radius", "4 sites, 1000km radius", "5 sites, 1000km radius")
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
input.strings.full <- c("stages_g200",
                     "stages_g100",
                     "stages_s200",
                     "stages_s100",
                     "stages_g200_epif",
                     "stages_g100_epif",
                     "stages_s200_epif",
                     "stages_s100_epif",
                     "stages_g200_inf",
                     "stages_g100_inf",
                     "stages_s200_inf",
                     "stages_s100_inf",
                     "stages_g200_sitesThenRefs",
                     "stages_g100_sitesThenRefs",
                     "stages_s200_sitesThenRefs",
                     "stages_s100_sitesThenRefs")

input.strings.simple <- c("stages_g200",
                        "stages_g100",
                        "stages_s200",
                        "stages_s100",
                        "stages_g200_epif",
                        "stages_g100_epif",
                        "stages_s200_epif",
                        "stages_s100_epif",
                        "stages_g200_inf",
                        "stages_g100_inf",
                        "stages_s200_inf",
                        "stages_s100_inf",
                        "stages_g200_sitesThenRefs",
                        "stages_g100_sitesThenRefs",
                        "stages_s200_sitesThenRefs",
                        "stages_s100_sitesThenRefs")

model.input.dirs <- c("~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/base",
                                     "~/R_packages/bivbrach/data/lmm/basic/genera_100km_cells/base",
                                     "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/base",
                                     "~/R_packages/bivbrach/data/lmm/basic/species_100km_cells/base",
                                     "~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/epifaunal",
                                     "~/R_packages/bivbrach/data/lmm/basic/genera_100km_cells/epifaunal",
                                     "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/epifaunal",
                                     "~/R_packages/bivbrach/data/lmm/basic/species_100km_cells/epifaunal",
                                     "~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/infaunal",
                                     "~/R_packages/bivbrach/data/lmm/basic/genera_100km_cells/infaunal",
                                     "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/infaunal",
                                     "~/R_packages/bivbrach/data/lmm/basic/species_100km_cells/infaunal",
                                     "~/R_packages/bivbrach/data/lmm/basic/genera_200km_cells/sitesThenRefs",
                                     "~/R_packages/bivbrach/data/lmm/basic/genera_100km_cells/sitesThenRefs",
                                     "~/R_packages/bivbrach/data/lmm/basic/species_200km_cells/sitesThenRefs",
                                     "~/R_packages/bivbrach/data/lmm/basic/species_100km_cells/sitesThenRefs")

output.dirs.full <- c("~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/base/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/genera_100km_cells/base/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/base/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/species_100km_cells/base/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/epifaunal/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/genera_100km_cells/epifaunal/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/epifaunal/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/species_100km_cells/epifaunal/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/infaunal/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/genera_100km_cells/infaunal/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/infaunal/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/species_100km_cells/infaunal/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/sitesThenRefs/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/genera_100km_cells/sitesThenRefs/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/sitesThenRefs/full",
                 "~/R_packages/bivbrach/figures/richness_lmm/species_100km_cells/sitesThenRefs/full")

output.dirs.simple <- c("~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/base/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/genera_100km_cells/base/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/base/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/species_100km_cells/base/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/epifaunal/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/genera_100km_cells/epifaunal/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/epifaunal/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/species_100km_cells/epifaunal/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/infaunal/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/genera_100km_cells/infaunal/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/infaunal/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/species_100km_cells/infaunal/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/genera_200km_cells/sitesThenRefs/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/genera_100km_cells/sitesThenRefs/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/species_200km_cells/sitesThenRefs/simple",
                      "~/R_packages/bivbrach/figures/richness_lmm/species_100km_cells/sitesThenRefs/simple")

## Function
source("functions/mass.SJplot.R")
## Run function for all
rich.input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"

r = 1
input.string = input.strings.full[r]
model.type = "full"
argument.strings = title.strings
model.input.dir = model.input.dirs[r]
rich.input.dir = rich.input.dir
output.dirs.full[r]
times.col = "times"
period.scale = periods
era.scale = eras
xy = c("Bivalvia", "Brachiopoda")
i = 1

for(r in 1:length(input.strings)){
 mass.SJplot(input.strings.full[r], model.type = "full", title.strings, model.input.dirs[r], rich.input.dir, output.dirs.full[r], times.col = "times", period.scale = periods, era.scale = eras, xy = c("Bivalvia", "Brachiopoda"))
}

for(r in 1:length(input.strings)){
  mass.SJplot(input.strings.simple[r], model.type = "simple", title.strings, model.input.dirs[r], rich.input.dir, output.dirs.simple[r], times.col = "times", period.scale = periods, era.scale = eras, xy = c("Bivalvia", "Brachiopoda"))
}

