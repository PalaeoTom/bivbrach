## 5. Mixed effect modelling
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

#### Plotting main results ####
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

## define input strings
input.strings <- c("stages_g200",
                   "stages_g100",
                   "stages_s200",
                   "stages_s100")

## Function
source("functions/mass.SJplot.R")

## Run function for all
model.input.dir <- "~/R_packages/bivbrach/data"
rich.input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dir <- "~/R_packages/bivbrach/figures/sjPlot"

for(r in 1:length(input.strings)){
 mass.SJplot(input.strings[r], title.strings, model.input.dir, rich.input.dir, output.dir, times.col = "times", period.scale = periods, era.scale = eras, xy = c("Bivalvia", "Brachiopoda"))
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

output.strings <- c("stages_g200_mlm",
                    "stages_g100_mlm",
                    "stages_s200_mlm",
                    "stages_s100_mlm")

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
era.interval <- get.interval.mat(midpoints, era.cutoffs)
PTME.interval <- get.interval.mat(midpoints, PTME.cutoffs)

## Set other parameters directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dir <- "~/R_packages/bivbrach/data/mlms_era_PTME_random_effects/"

a = 1
input.pre = input.strings[a]
output.pre = output.strings[a]
interval = era.interval
i = 1

mass.mlm.REinterval <- function(input.dir, input.pre, output.dir, output.pre, vars, vars.values, interval){
  combin <- expand.grid(vars)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".csv")
  output.dir.mlm <- paste0(output.dir, "/", output.pre, "_models.Rds")
  output.dir <- paste0(output.dir, "/", output.pre, ".csv")
  output.mat <- expand.grid(vars.values)
  ## create output matrix
  data.mat <- matrix(NA, ncol = 15, nrow = nrow(output.mat))
  colnames(data.mat) <- c("n.samples", "n.timeBins", "n.subregions", "avg.obs.per.timeBin", "avg.obs.per.subregion", "intercept", "intercept.std.error", "intercept.df", "intercept.t.value", "intercept.p.value", "slope", "slope.std.error", "slope.df", "slope.t.value", "slope.p.value")
  ## create output list
  output.list <- lapply(1:nrow(output.mat), function(all) NA)
  ## populate output vectors
  for(i in 1:nrow(output.mat)){
    ## Read in data
    data <- suppressWarnings(tryCatch(read.csv(input.dirs[i], row.names = 1), error = function(e){}))
    ## if null, log entries as NA
    if(is.null(data)){
      next
    } else {
      ## use midpoint to assign time period a category
      interval.cat <- data.frame(matrix(NA, nrow = nrow(data), ncol = 1))
      colnames(interval.cat) <- "interval"
      midpoints <- matrix(unique(data[,1]), ncol = 1)
      cats <- rownames(interval)
      sapply(1:nrow(interval), function(x){
        bet <- between(as.vector(midpoints), interval[x,2], interval[x,1])
        if(any(bet)){

        }
      })





      data.mat[i,1] <- nrow(data)
      data.mat[i,2] <- length(unique(data[,"times"]))
      data.mat[i,3] <- length(unique(data[,"source.subregion.ID"]))
      data.mat[i,4] <- nrow(data)/(length(unique(data[,"times"])))
      data.mat[i,5] <- nrow(data)/(length(unique(data[,"source.subregion.ID"])))
      #mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia + (1|times/source.subregion.ID), data = data), error = function(e){}))
      mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia + (Bivalvia|times/source.subregion.ID), data = data), error = function(e){}))
      if(is.null(mlm)){
        next
      } else {
        ## Add MLM to list
        output.list[[i]] <- mlm
        ## get coefficients
        coefficients <- coef(summary(mlm))
        data.mat[i,6] <- coefficients[1,1]
        data.mat[i,7] <- coefficients[1,2]
        data.mat[i,8] <- coefficients[1,3]
        data.mat[i,9] <- coefficients[1,4]
        data.mat[i,10] <- coefficients[1,5]
        data.mat[i,11] <- coefficients[2,1]
        data.mat[i,12] <- coefficients[2,2]
        data.mat[i,13] <- coefficients[2,3]
        data.mat[i,14] <- coefficients[2,4]
        data.mat[i,15] <- coefficients[2,5]
      }
    }
  }
  output.mat <- cbind(output.mat, data.mat)
  output.mat <- output.mat[which(!apply(output.mat, 1, function(x) any(is.na(x)))),]
  names(output.list) <- varStrings
  output.list <- output.list[!is.na(output.list)]
  write.csv(output.mat, output.dir)
  saveRDS(output.list, output.dir.mlm)
}

