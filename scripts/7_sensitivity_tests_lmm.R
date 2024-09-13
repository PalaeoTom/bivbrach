## 6. Mixed effect modelling  - sensitivity tests
## Started by TJS on 04/09/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("lme4", "velociraptr", "sjPlot", "ggplot2", "velociraptr", "dplyr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(lme4)
library(velociraptr)
library(sjPlot)
library(ggplot2)
library(velociraptr)
library(dplyr)

#### Sensitivity testing - taking median richness for each radially constrained region and dropping random effect ####
rm(list = ls())
radii <- as.integer(c(1000000, 500000))
siteQuotas <- c(2, 4)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- names(vars) <- c("site_quota", "radius")

## input strings
## Set output strings
input.strings <- c("stages_g200",
                   "stages_s200")

output.strings.med.diff <- c("stages_g200_lmm_med_diff",
                             "stages_s200_lmm_med_diff")


output.dirs.median <- c("~/R_packages/bivbrach/data/lmm/sensitivity_testing/median/genera_200km_cells",
                 "~/R_packages/bivbrach/data/lmm/sensitivity_testing/median/species_200km_cells")

## Set other parameters directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
source("functions/mass.mlm.on.summary.R")

#m = 1
#input.dir = input.dir
#input.pre = input.strings[m]
#output.dir = output.dirs.median[m]
#output.pre = output.strings.med.diff[m]
#vars = vars
#vars.values = vars.values
#mode = "median"
#unit = "diff"
#data.columns = c("Brachiopoda", "Bivalvia")
#i = 1

## Run for each input under each setting
for(m in 1:length(input.strings)){
  mass.mlm.on.summary(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dirs.median[m],
                      output.pre = output.strings.med.diff[m], vars = vars, vars.values = vars.values, mode = "median", unit = "diff", data.columns = c("Brachiopoda", "Bivalvia"))
}

## Testing model assumptions ##
rm(list = ls())

## Load variable vectors - just looking at sites moving forward.
radii <- as.integer(c(1000000, 500000))
siteQuotas <- c(2, 4)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- names(vars) <- c("site_quota", "radius")

## Load input and output strings
input.strings <- c("stages_g200",
                   "stages_s200")

model.input.dirs <- c("~/R_packages/bivbrach/data/lmm/sensitivity_testing/median/genera_200km_cells",
                      "~/R_packages/bivbrach/data/lmm/sensitivity_testing/median/species_200km_cells")

output.dirs.median <- c("~/R_packages/bivbrach/figures/richness_lmm/median/genera_200km_cells",
                        "~/R_packages/bivbrach/figures/richness_lmm/median/species_200km_cells")

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
  visualise.model.assumption(input.string = input.strings[r], model.type = "median", argument.strings = title.strings, input.dir = model.input.dirs[r], output.dir = output.dirs.median[r])
}

## Now to plot ##
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
input.strings <- c("stages_g200",
                        "stages_s200")

model.input.dirs <- c("~/R_packages/bivbrach/data/lmm/sensitivity_testing/median/genera_200km_cells",
                        "~/R_packages/bivbrach/data/lmm/sensitivity_testing/median/species_200km_cells")

output.dirs.median <- c("~/R_packages/bivbrach/figures/richness_lmm/median/genera_200km_cells",
                      "~/R_packages/bivbrach/figures/richness_lmm/median/species_200km_cells")

## Function
source("functions/mass.SJplot.R")

## Run function for all
#r = 2
#input.string = input.strings[r]
#model.type = "median"
#argument.strings = title.strings
#model.input.dir = model.input.dirs[r]
#rich.input.dir = model.input.dirs[r]
#output.dir <- output.dirs.median[r]
#times.col = "times"
#period.scale = periods
#era.scale = eras
#xy = c("Bivalvia", "Brachiopoda")
#i = 4

for(r in 1:length(input.strings)){
  mass.SJplot(input.strings[r], model.type = "median", title.strings, model.input.dirs[r], model.input.dirs[r], output.dirs.median[r], times.col = "times", period.scale = periods, era.scale = eras, xy = c("Bivalvia", "Brachiopoda"))
}

#### Sensitivity test - modelling time periods as random effects ####
rm(list = ls())
radii <- as.integer(c(1000000, 500000))
siteQuotas <- c(2, 4)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- names(vars) <- c("site_quota", "radius")

## input strings
## Set output strings
input.strings <- c("stages_g200",
                   "stages_s200")

output.dirs.intervals.RE <- c("~/R_packages/bivbrach/data/lmm/sensitivity_testing/intervals_randomEffect/genera_200km_cells",
                        "~/R_packages/bivbrach/data/lmm/sensitivity_testing/intervals_randomEffect/species_200km_cells")

output.strings.PTME <- c("stages_g200_lmm_PTME",
                         "stages_s200_lmm_PTME")

output.strings.eras <- c("stages_g200_lmm_eras",
                         "stages_s200_lmm_eras")

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

## Read in function
source("functions/mass.mlm.REinterval.R")

## Run for each input
for(m in 1:length(input.strings)){
  mass.mlm.REinterval(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dirs.intervals.RE[m],
                      output.pre = output.strings.PTME[m], vars = vars, vars.values = vars.values, interval = PTME.interval)
}

for(m in 1:length(input.strings)){
  mass.mlm.REinterval(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dirs.intervals.RE[m],
                      output.pre = output.strings.eras[m], vars = vars, vars.values = vars.values, interval = era.interval)
}

## Testing model assumptions ##
rm(list = ls())

## Load variable vectors - just looking at sites moving forward.
radii <- as.integer(c(1000000, 500000))
siteQuotas <- c(2, 4)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- names(vars) <- c("site_quota", "radius")

## Load input and output strings
input.strings <- c("stages_g200",
                   "stages_s200")

model.input.dirs <- c("~/R_packages/bivbrach/data/lmm/sensitivity_testing/intervals_randomEffect/genera_200km_cells",
                      "~/R_packages/bivbrach/data/lmm/sensitivity_testing/intervals_randomEffect/species_200km_cells")

output.dirs.intervals.RE <- c("~/R_packages/bivbrach/figures/richness_lmm/intervals_randomEffect/genera_200km_cells",
                              "~/R_packages/bivbrach/figures/richness_lmm/intervals_randomEffect/species_200km_cells")

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
  visualise.model.assumption(input.string = input.strings[r], model.type = "interval_RE_era", argument.strings = title.strings, input.dir = model.input.dirs[r], output.dir = output.dirs.intervals.RE[r])
}

for(r in 1:length(input.strings)){
  visualise.model.assumption(input.string = input.strings[r], model.type = "interval_RE_PTME", argument.strings = title.strings, input.dir = model.input.dirs[r], output.dir = output.dirs.intervals.RE[r])
}

## Plot!
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
input.strings <- c("stages_g200",
                         "stages_s200")

model.input.dirs <- c("~/R_packages/bivbrach/data/lmm/sensitivity_testing/intervals_randomEffect/genera_200km_cells",
                      "~/R_packages/bivbrach/data/lmm/sensitivity_testing/intervals_randomEffect/species_200km_cells")

output.dirs.intervals.RE <- c("~/R_packages/bivbrach/figures/richness_lmm/intervals_randomEffect/genera_200km_cells",
                        "~/R_packages/bivbrach/figures/richness_lmm/intervals_randomEffect/species_200km_cells")

rich.input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"

## Function
source("functions/mass.SJplot.R")

## Run function for all
#r = 2
#input.string = input.strings[r]
#model.type = "interval_RE_era"
#argument.strings = title.strings
#model.input.dir = model.input.dirs[r]
#rich.input.dir = rich.input.dir
#output.dir <- output.dirs.intervals.RE[r]
#times.col = "times"
#period.scale = periods
#era.scale = eras
#xy = c("Bivalvia", "Brachiopoda")
#min.sample = 20
#i = 1

## Run it!
for(r in 1:length(input.strings)){
  mass.SJplot(input.strings[r], model.type = "interval_RE_era", title.strings, model.input.dirs[r], rich.input.dir, output.dirs.intervals.RE[r], times.col = "times", period.scale = periods, era.scale = eras, xy = c("Bivalvia", "Brachiopoda"))
}

for(r in 1:length(input.strings)){
  mass.SJplot(input.strings[r], model.type = "interval_RE_PTME", title.strings, model.input.dirs[r], rich.input.dir, output.dirs.intervals.RE[r], times.col = "times", period.scale = periods, era.scale = eras, xy = c("Bivalvia", "Brachiopoda"))
}


#### Sensitivity testing - breaking up time periods ####
rm(list = ls())
radii <- as.integer(c(1000000, 500000))
siteQuotas <- c(2, 4)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- names(vars) <- c("site_quota", "radius")

## input strings
## Set output strings
input.strings <- c("stages_g200",
                   "stages_s200")

output.dirs.intervals.split <- c("~/R_packages/bivbrach/data/lmm/sensitivity_testing/intervals_splitData/genera_200km_cells",
                              "~/R_packages/bivbrach/data/lmm/sensitivity_testing/intervals_splitData/species_200km_cells")

output.strings.PTME <- c("stages_g200_lmm_PTME",
                         "stages_s200_lmm_PTME")

output.strings.eras <- c("stages_g200_lmm_eras",
                         "stages_s200_lmm_eras")

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
source("functions/mass.mlm.intervals.R")

## Run function for all
#m = 1
#input.dir = input.dir
#input.pre = input.strings[m]
#output.dir = output.dirs.intervals.split[m]
#output.pre = output.strings.eras[m]
#vars = vars
#vars.values = vars.values
#time.cutoffs = era.cutoffs
#i = 1

## Run for each input
for(m in 1:length(input.strings)){
  mass.mlm.intervals(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dirs.intervals.split[m],
                     output.pre = output.strings.eras[m], vars = vars, vars.values = vars.values, time.cutoffs = era.cutoffs)
}

for(m in 1:length(input.strings)){
  mass.mlm.intervals(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dirs.intervals.split[m],
                     output.pre = output.strings.PTME[m], vars = vars, vars.values = vars.values, time.cutoffs = PTME.cutoffs)
}

## Testing model assumptions ##
rm(list = ls())

## Load variable vectors - just looking at sites moving forward.
radii <- as.integer(c(1000000, 500000))
siteQuotas <- c(2, 4)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- names(vars) <- c("site_quota", "radius")

## Load input and output strings
input.strings <- c("stages_g200",
                   "stages_s200")

model.input.dirs <- c("~/R_packages/bivbrach/data/lmm/sensitivity_testing/intervals_splitData/genera_200km_cells",
                      "~/R_packages/bivbrach/data/lmm/sensitivity_testing/intervals_splitData/species_200km_cells")

output.dirs.intervals.split <- c("~/R_packages/bivbrach/figures/richness_lmm/intervals_splitData/genera_200km_cells",
                                 "~/R_packages/bivbrach/figures/richness_lmm/intervals_splitData/species_200km_cells")

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
  visualise.model.assumption(input.string = input.strings[r], model.type = "interval_split_era", argument.strings = title.strings, input.dir = model.input.dirs[r], output.dir = output.dirs.intervals.split[r], intervals = c("Paleozoic", "Mesozoic", "Cenozoic"))
}

for(r in 1:length(input.strings)){
  visualise.model.assumption(input.string = input.strings[r], model.type = "interval_split_PTME", argument.strings = title.strings, input.dir = model.input.dirs[r], output.dir = output.dirs.intervals.split[r], intervals = c("Pre-PTME", "Post-PTME"))
}

## Now to plot!
## Download period colour palettes
periods <- downloadTime("international periods")
periods <- periods[order(periods$b_age, decreasing=TRUE), ]
periods <- periods[periods$b_age <= periods[which(rownames(periods) == "Cambrian"),"b_age"],c(2,4,5,8)]
periods[,"shape"] <- c(rep(16, 6), rep(15, 3), rep(17, 3))

## Download era shape palette
eras <- downloadTime("international eras")[1:3,]
eras <- eras[order(eras$b_age, decreasing = TRUE),c(2,4,5)]
eras[,"shape"] <- c(16,15,17)

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

## define input and output strings
input.strings <- c("stages_g200",
                   "stages_s200")

model.input.dirs <- c("~/R_packages/bivbrach/data/lmm/sensitivity_testing/intervals_splitData/genera_200km_cells",
                      "~/R_packages/bivbrach/data/lmm/sensitivity_testing/intervals_splitData/species_200km_cells")

output.dirs.intervals.split <- c("~/R_packages/bivbrach/figures/richness_lmm/intervals_splitData/genera_200km_cells",
                              "~/R_packages/bivbrach/figures/richness_lmm/intervals_splitData/species_200km_cells")

rich.input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"

## Function
source("functions/mass.SJplot.R")

## Run function for all
#r = 2
#input.string = input.strings[r]
#model.type = "interval_split_PTME"
#argument.strings = title.strings
#model.input.dir = model.input.dirs[r]
#rich.input.dir = rich.input.dir
#output.dir <- output.dirs.intervals.split[r]
#times.col = "times"
#period.scale = periods
#era.scale = eras
#xy = c("Bivalvia", "Brachiopoda")
#min.sample = 20
#time.cutoffs = era.cutoffs
#s = 3
#i = 2

## Run it!
for(r in 1:length(input.strings)){
  mass.SJplot(input.strings[r], model.type = "interval_split_era", argument.strings = title.strings,
              model.input.dir = model.input.dirs[r], rich.input.dir = rich.input.dir,
              output.dir = output.dirs.intervals.split[r], times.col = "times", period.scale = periods,
              era.scale = eras, xy = c("Bivalvia", "Brachiopoda"), min.sample = 20, time.cutoffs = era.cutoffs)
}

for(r in 1:length(input.strings)){
  mass.SJplot(input.strings[r], model.type = "interval_split_PTME", title.strings, model.input.dirs[r], rich.input.dir, output.dirs.intervals.split[r], times.col = "times", period.scale = periods, era.scale = eras, xy = c("Bivalvia", "Brachiopoda"), time.cutoffs = PTME.cutoffs)
}

