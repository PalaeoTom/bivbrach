## 5. Species richness of different categories
## Started by TJS on 04/09/2024

# Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("sjPlot")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(sjPlot)

## Load variable vectors - just looking at sites moving forward.
radii <- as.integer(c(1000000, 500000))
siteQuotas <- c(2, 4)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- names(vars) <- c("site_quota", "radius")

## Set input and output strings
input.strings <- c("stages_g200",
                   "stages_s200",
                   "stages_g200_epif",
                   "stages_s200_epif",
                   "stages_g200_ref",
                   "stages_s200_ref")

output.strings.env <- c("stages_g200_environment",
                        "stages_s200_environment",
                        "stages_g200_epif_environment",
                        "stages_s200_epif_environment",
                        "stages_g200_ref_environment",
                        "stages_s200_ref_environment")

output.strings.lith <- c("stages_g200_lithology",
                         "stages_s200_lithology",
                         "stages_g200_epif_lithology",
                         "stages_s200_epif_lithology",
                         "stages_g200_ref_lithology",
                         "stages_s200_ref_lithology")

output.strings.reef <- c("stages_g200_reefalState",
                         "stages_s200_reefalState",
                         "stages_g200_epif_reefalState",
                         "stages_s200_epif_reefalState",
                         "stages_g200_ref_reefalState",
                         "stages_s200_ref_reefalState")

output.strings.lat <- c("stages_g200_latitude",
                        "stages_s200_latitude",
                        "stages_g200_epif_latitude",
                        "stages_s200_epif_latitude",
                        "stages_g200_ref_latitude",
                        "stages_s200_ref_latitude")

## Set inpout and output directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dirs <- c("~/R_packages/bivbrach/figures/richness_boxplots_comparing_categories/genera_200km_cells/base",
                 "~/R_packages/bivbrach/figures/richness_boxplots_comparing_categories/species_200km_cells/base",
                 "~/R_packages/bivbrach/figures/richness_boxplots_comparing_categories/genera_200km_cells/epifaunal",
                 "~/R_packages/bivbrach/figures/richness_boxplots_comparing_categories/species_200km_cells/epifaunal",
                 "~/R_packages/bivbrach/figures/richness_boxplots_comparing_categories/genera_200km_cells/reference",
                 "~/R_packages/bivbrach/figures/richness_boxplots_comparing_categories/species_200km_cells/reference")


## Arguments
#r = 11
#input.pre <- input.strings[r]
#output.pre <- output.strings.env[r]
#output.dir <- output.dirs[r]
#covariate <- "sampEnv"
#data.strings <- c("Bivalvia", "Brachiopoda")
#i = 1
#d = 2

## Read in function
source("functions/compare.categories.R")

## Run the function!
for(r in 1:length(input.strings)){
  compare.categories(input.pre = input.strings[r], output.pre = output.strings.env[r], output.dir = output.dirs[r], covariate = "sampEnv", data.strings = c("Bivalvia", "Brachiopoda"))
}

for(r in 1:length(input.strings)){
  compare.categories(input.pre = input.strings[r], output.pre = output.strings.lith[r], output.dir = output.dirs[r], covariate = "sampLith", data.strings = c("Bivalvia", "Brachiopoda"))
}

for(r in 1:length(input.strings)){
  compare.categories(input.pre = input.strings[r], output.pre = output.strings.reef[r], output.dir = output.dirs[r], covariate = "sampReef", data.strings = c("Bivalvia", "Brachiopoda"))
}

for(r in 1:length(input.strings)){
  compare.categories(input.pre = input.strings[r], output.pre = output.strings.lat[r], output.dir = output.dirs[r], covariate = "sampLat", data.strings = c("Bivalvia", "Brachiopoda"))
}


