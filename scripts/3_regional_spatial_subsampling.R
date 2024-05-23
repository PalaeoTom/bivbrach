## 3. Spatial subsampling using modified divvy
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("divvy", "velociraptr", "dplyr", "plyr", "parallel", "RColorBrewer", "plyr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divvy)
library(velociraptr)
library(dplyr)
library(plyr)
library(parallel)
library(RColorBrewer)
library(plyr)

## Load data
setwd("~/R_packages/R_projects/bivbrach")
stages.g200 <- readRDS("data/stages_g200.Rds")
stages.g100 <- readRDS("data/stages_g100.Rds")
stages.s200 <- readRDS("data/stages_s200.Rds")
stages.s100 <- readRDS("data/stages_s100.Rds")

#### Identify viable cookies ####
## Load new functions
source("functions/findPool2.R")
source("functions/findSeeds2.R")
source("functions/getOverlap.R")
source("functions/cookie.R")
source("functions/biscuits.R")
source("functions/cut.biscuits.R")

## Define variables
radii <- as.integer(c(200000, 500000, 1000000))
siteQuotas <- c(2, 3, 4, 5)
overlapThresholds <- 0
overlapTypes <- "sites"
weightStandardisation_1 <- F

#### Generate spatial subsamples ####
## Generate output vectors
output.vector <- c("stages_g200",
                   "stages_g100",
                   "stages_s200",
                   "stages_s100")

data.strings <- c("stages.g200",
                  "stages.g100",
                  "stages.s200",
                  "stages.s100")

## Not standardised for occupancy - just returning spatial subsamples
for(i in 1:length(output.vector)){
cut.biscuits(data = eval(parse(text=data.strings[i])), siteQuota = siteQuotas, r = radii, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, standardiseSiteNumber = F, weightedStandardisation = weightStandardisation_1,
             taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), name.output = output.vector[i], n.cores = 4)
}

#### Step 1 - get performance data on different settings. Number of viable spatial subsamples under all configurations for all datasets through time ####
source("functions/count.viable.samples.R")
dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
cores <- 4
taxa.split <- T
prefix.vector <- output.vector <- c("stages_g200", "stages_g100","stages_s200","stages_s100")
for(i in 1:length(prefix.vector)){
  count.viable.samples(dir = dir, pre = prefix.vector[i], vars = vars, sD = eval(parse(text=data.strings[i])), n.cores = cores, taxa = taxa.split, output.dir = "~/R_packages/R_projects/bivbrach/data", output.name = prefix.vector[i])
}

## read in result
stages.g200.VCs <- read.csv("data/stages_g200_viable_subsamples.csv", header = T, row.names = 1)
stages.g100.VCs <- read.csv("data/stages_g100_viable_subsamples.csv", header = T, row.names = 1)
stages.s200.VCs <- read.csv("data/stages_s200_viable_subsamples.csv", header = T, row.names = 1)
stages.s100.VCs <- read.csv("data/stages_s100_viable_subsamples.csv", header = T, row.names = 1)

## Update colnames using stage data to make them more usable
stages <- downloadTime('international ages')
stages <- stages[order(stages$b_age, decreasing=TRUE), ]
colnames(stages.g200.VCs) <- rownames(stages)[-102]
colnames(stages.g100.VCs) <- rownames(stages)[-102]
colnames(stages.s200.VCs) <- rownames(stages)[-102]
colnames(stages.s100.VCs) <- rownames(stages)[-102]

## Get midpoints for plotting
midpoints <- as.numeric(names(stages.g200))

## Get period data for plotting geo scale
periods <- downloadTime("international periods")
periods <- periods[order(periods$b_age, decreasing=TRUE), ]
periods <- periods[periods$b_age <= periods[which(rownames(periods) == "Cambrian"),"b_age"],]

## Main title vector
titles <- c("Genera, 200km grid cells", "Genera, 100km grid cells", "Species, 200km grid cells", "Species, 100km grid cells")

# Set output directory
output.dir <- "~/R_packages/R_projects/bivbrach/figures"

## Output file name vector
output.strings <- c("stages_g200", "stages_g100", "stages_s200", "stages_s100")

## Input strings
input.strings <- c("stages.g200.VCs", "stages.g100.VCs", "stages.s200.VCs", "stages.s100.VCs")

## Set legend position
legend.position = c("topright", "topright", "topright", "topright")

## Set legend labels
legend.labels <- c("Radius 200km, 2 sites", "Radius 200km, 3 sites", "Radius 200km, 4 sites", "Radius 200km, 5 sites",
                   "Radius 500km, 2 sites", "Radius 500km, 3 sites", "Radius 500km, 4 sites", "Radius 500km, 5 sites",
                   "Radius 1000km, 2 sites", "Radius 1000km, 3 sites", "Radius 1000km, 4 sites", "Radius 1000km, 5 sites")

## Set palette for lines
pal.purple <- brewer.pal(n = 9, name = "Purples")[c(4,9)]
pal.purple.func <- colorRampPalette(c(pal.purple[1],pal.purple[2]))
purples <- pal.purple.func(4)

pal.green <- brewer.pal(n = 9, name = "Greens")[c(4,9)]
pal.green.func <- colorRampPalette(c(pal.green[1],pal.green[2]))
greens <- pal.green.func(4)

pal.orange <- brewer.pal(n = 9, name = "Oranges")[c(4,9)]
pal.orange.func <- colorRampPalette(c(pal.orange[1],pal.orange[2]))
oranges <- pal.orange.func(4)

## Set line type palette
line.type.pal <- c(1,6,2,3,1,6,2,3,1,6,2,3)

## read in plotting functions
source("functions/shade.time.R")
source("functions/add.geo.scale.R")
source("functions/plot.spatSubThroughTime.R")

## Set increments
y.ax.inc <- 5
x.ax.inc <- 100

## Run plotting function
plot.spatSubThroughTime(input.strings, output.dir, output.strings, strat.data = periods, time.data = midpoints, titles, legend.position, legend.labels,
                        y.axis.inc = y.ax.inc, x.axis.inc = x.ax.inc, line.pal = c(oranges, greens, purples), line.type.pal)

### Only useful when more data integrated so individual time bins may be compared ####
#source("functions/countUsable.R")
#source("functions/wrap.countUsable.R")

## get grid of variable combination
#varGrid <- expand.grid(siteQuotas, radii/1000)
#threshold.VC <- 5

## count usable bins and format
#stages.g200.UTBs <- wrap.countUsable(VC = stages.g200.VCs, threshold.VC, varGrid, c("siteQuota", "radius"))
#stages.g100.UTBs <- wrap.countUsable(VC = stages.g100.VCs, threshold.VC, varGrid, c("siteQuota", "radius"))
#stages.s200.UTBs <- wrap.countUsable(VC = stages.s200.VCs, threshold.VC, varGrid, c("siteQuota", "radius"))
#stages.s100.UTBs <- wrap.countUsable(VC = stages.s100.VCs, threshold.VC, varGrid, c("siteQuota", "radius"))

## get combination of siteQuotas and radii
#split.vars <- expand.grid(radii/1000, siteQuotas)
#colnames(split.vars) <- c("radius", "siteQuota")

## get plotting data
#source("functions/get.VC.plot.data.R")
#stages.g200.barData <- get.VC.plot.data(UTB = stages.g200.UTBs, split.vars = split.vars)
#stages.g100.barData <- get.VC.plot.data(UTB = stages.g100.UTBs, split.vars = split.vars)
#stages.s200.barData <- get.VC.plot.data(UTB = stages.s200.UTBs, split.vars = split.vars)
#stages.s100.barData <- get.VC.plot.data(UTB = stages.s100.UTBs, split.vars = split.vars)

## set output directory and read in plotting function
#output.dir <- "/Users/tjs/R_packages/R_projects/bivbrach/figures"
#source("functions/plot.UTBs.R")

## Plot figures - add labels in post
## Set input strings and output names
#stages.output.name <- c("stages_g200", "stages_g100",
#                        "stages_s200", "stages_s100")
#stages.input.strings <- c("stages.g200.barData", "stages.g100.barData",
#                          "stages.s200.barData", "stages.s100.barData")

## Plot stages data
#for(i in 1:length(stages.input.strings)){
#  plot.UTBs(data = eval(parse(text=stages.input.strings[i])), output.dir = output.dir, output.name = stages.output.name[i])
#}

#### Step 2 - drop non-viable time bins from each dataset ####
#### Only relevant if seeking to do richness/diversification through time.
## Load function and static arguments
source("functions/drop.unusable.bins.R")
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
output.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
taxa <- T
threshold = threshold.VC = 1
prefix.vector <- c("stages_g200", "stages_g100","stages_s200","stages_s100")
out.pre.vector <- c("stages_g200_viaTimBin","stages_g100_viaTimBin","stages_s200_viaTimBin","stages_s100_viaTimBin")
data.strings <- c("stages.g200",
                  "stages.g100",
                  "stages.s200",
                  "stages.s100")

## Run function
for(i in 1:length(out.pre.vector)){
  drop.unusable.bins(input.dir = input.dir, input.pre = prefix.vector[i], output.dir = output.dir, output.pre = out.pre.vector[i],
                     vars = vars, sD = eval(parse(text=data.strings[i])), threshold = threshold.VC, taxa = T)
}
