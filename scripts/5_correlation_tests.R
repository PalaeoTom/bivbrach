## 5. Mixed effect modelling
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("lmerTest", "velociraptr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(lmerTest)
library(velociraptr)

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
setwd("~/R_packages/bivbrach")
home <- getwd()

## Set other parameters directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dir <- "~/R_packages/bivbrach/data"
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
output.dir <- "~/R_packages/bivbrach/data"
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
stages <- downloadTime('international ages')
stages$name <- row.names(stages)

## Re-order stages by age, oldest first
stages <- stages[order(stages$b_age, decreasing=TRUE), ]

## Create columns for new rounded ages
stages$b_round <- stages$t_round <- 0

## Read in function for defining rounded stage ages
source("functions/round.age.R")

## Define groupings for rounding
groupings <- list(u10 <- which(stages$b_age < 10),
                  u150 <- which(stages$b_age < 150 & stages$b_age > 10),
                  old <- which(stages$b_age > 150))

## Round ages to different digits depending on age classification
for (group in 1:length(groupings)){
  bins <- groupings[[group]]
  digits <- c(2, 1, 0)[group]

  # round down younger boundary (terminus) and up older boundary (beginning) per stage
  for (i in bins){
    b <- stages$b_age[i]
    t <- stages$b_age[i+1]
    stages$b_round[i] <- round.age(b, digits=digits, round_up=TRUE)
    stages$t_round[i] <- round.age(t, digits=digits, round_up=FALSE)
  }
}

## Get time bins
era.cutoffs <- matrix(c(stages["Fortunian","b_round"],stages["Induan","b_round"],stages["Danian","b_round"],stages["Induan","b_round"],stages["Maastrichtian","t_round"],0), ncol = 2, nrow = 3)
colnames(era.cutoffs) <- c("bottom","top")
rownames(era.cutoffs) <- c("Paleozoic", "Mesozoic", "Cenozoic")

PTME.cutoffs <- matrix(c(stages["Fortunian","b_round"],stages["Induan","b_round"],stages["Induan","b_round"],0), ncol = 2, nrow = 2)
colnames(PTME.cutoffs) <- c("bottom","top")
rownames(PTME.cutoffs) <- c("Pre-PTME", "Post-PTME")

## Set other parameters directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dir <- "~/R_packages/bivbrach/data"
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

