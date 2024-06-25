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

## Set working directory
setwd("~/R_packages/R_projects/bivbrach")
home <- getwd()

## Set other parameters directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dir <- "~/R_packages/R_projects/bivbrach/data"
source("functions/mass.mlm.R")

## mass.mlm arguments
#m = 1
#input.dir = input.dir
#input.pre = input.strings[m]
#output.dir = output.dir
#output.pre = output.strings[m]
#vars = vars
#vars.values = vars.values

## Run for each input
for(m in 1:length(input.strings)){
  mass.mlm(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
           output.pre = output.strings[m], vars = vars, vars.values = vars.values)
}
