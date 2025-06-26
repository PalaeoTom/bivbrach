## 3. Spatial subsampling using modified divvy
## Started by TJS on 08/01/2024

#### Set up - run before any subsection ####
## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("divvy", "remotes")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divvy)
library(remotes)

## install divvyCompanion from github and load
remotes::install_github("PalaeoTom/divvyCompanion")
library(divvyCompanion)

## Load data
setwd("~/R_packages/bivbrach")
genera <- readRDS("data/final/master_2_2_uniq.Rds")
species <- readRDS("data/final/master_2_2_uniq_species.Rds")
home <- getwd()

## Load new functions
source("functions/cookies2Batch.R")

## Define variables
radii <- as.integer(c(1000000, 500000))
siteQuotas <- c(2, 4)
overlapThresholds <- 0
overlapTypes <- "sites"
vars <- list(siteQuotas, radii)
names(vars) <- c("sQ","r")

#### Get distribution of occurrence numbers for each subsample of sites ####
## Generate spatial subsamples, returning all occurrences associate with subsamples of sites
## Only doing this for main dataset and epifaunal - refined reference datasets will not use occurrence rarefaction
## Generate output vectors
output.strings.sites <- c("stages_g200",
                          "stages_s200",
                          "stages_g200_epif",
                          "stages_s200_epif")

data.strings <- c("stages.g200",
                  "stages.s200",
                  "stages.g200.epif",
                  "stages.s200.epif")

## Arguments
#z = 1
#dataList = eval(parse(text=data.strings[z]))
#vars = vars
#b.crs = 'EPSG:8857'
#b.xy = c("cellX", "cellY")
#output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
#overlapThreshold = overlapThresholds
#overlapType = overlapTypes
#overlapPruningMode = "maxOccs"
#rarefaction = "sites"
#name.output = output.strings.sites[z]
#n.cores = 4
#taxa = c("Brachiopoda","Bivalvia")
#taxa.level = c("phylum","class")
#all.var.comb = F
#nOccs = 100
#reps = 100
#i = 1
#x = 16

## use biscuitsBatch to run all permutations
for(z in 1:length(output.strings.sites)){
  cookies2Batch(dataList = eval(parse(text=data.strings[z])), vars = vars, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub",
                overlapThreshold = overlapThresholds, overlapType = overlapTypes, overlapPruningMode = "maxOccs", rarefaction = "sites",
                name.output = output.strings.sites[z], n.cores = 4, taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"), all.var.comb = F)
}

## now convert to viable bins
setwd(home)
source("functions/drop.unusable.bins.R")
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
output.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
taxa <- T
threshold = threshold.VC = 1
prefix.vector <- output.strings.sites
out.pre.vector <- c("stages_g200_viaTimeBin",
                    "stages_s200_viaTimeBin",
                    "stages_g200_epif_viaTimeBin",
                    "stages_s200_epif_viaTimeBin")

## Run function
for(z in 1:length(out.pre.vector)){
  drop.unusable.bins(input.dir = input.dir, input.pre = prefix.vector[z], output.dir = output.dir, output.pre = out.pre.vector[z],
                     vars = vars, sD = eval(parse(text=data.strings[z])), threshold = threshold.VC, taxa = T, all.var.comb = F)
}

## Now to count up occurrences for each one
source("functions/count.occurrences.R")
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
output.dir <- "~/R_packages/bivbrach/data/occurrence_count"
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
names(vars) <- c("siteQuota", "radius")
taxa <- T
input.strings <- out.pre.vector
output.strings <- c("stages_g200_occ_count",
                    "stages_s200_occ_count",
                    "stages_g200_epif_occ_count",
                    "stages_s200_epif_occ_count")

## Run the function
for(z in 1:length(output.strings)){
  count.occurrences(input.dir = input.dir, input.pre = input.strings[z], output.dir = output.dir, output.pre = output.strings[z],
                    vars = vars, method = "sites", n.cores = 4, taxa = T, all.var.comb = F)
}

#### Further testing occurrence numbers - get occurrences by cell for each dataset ####
source("functions/get.occs.per.cell.R")

## Get occurrences per cell
stages.g200.occs <- get.occs.per.cell(stages.g200)
stages.s200.occs <- get.occs.per.cell(stages.s200)
stages.g200.ref.occs <- get.occs.per.cell(stages.g200.ref)
stages.s200.ref.occs <- get.occs.per.cell(stages.s200.ref)
stages.g200.epif.occs <- get.occs.per.cell(stages.g200.epif)
stages.s200.epif.occs <- get.occs.per.cell(stages.s200.epif)

#### Generate spatial subsamples ####
## Generate input and output vectors once again
output.vector <- c("stages_g200",
                   "stages_s200",
                   "stages_g200_ref",
                   "stages_s200_ref",
                   "stages_g200_epif",
                   "stages_s200_epif")

data.strings <- c("stages.g200",
                  "stages.s200",
                  "stages.g200.ref",
                  "stages.s200.ref",
                  "stages.g200.epif",
                  "stages.s200.epif")

rarefaction.strings <- c("sitesThenOccs",
                         "sitesThenOccs",
                         "none",
                         "none",
                         "sitesThenOccs",
                         "sitesThenOccs")

## Get arguments for standard run
## BiscuitBatch arguments
#z = 1
#dataList = eval(parse(text=data.strings[i]))
#vars = vars
#b.crs = 'EPSG:8857'
#b.xy = c("cellX", "cellY")
#output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
#overlapThreshold = overlapThresholds
#overlapType = overlapTypes
#rarefaction = "sitesThenOccs"
#name.output = output.vector[z]
#n.cores = 4
#taxa = c("Brachiopoda","Bivalvia")
#taxa.level = c("phylum","class")
#overlapPruningMode = "maxOccs"
#nOccs = occ.list[[z]]
#reps = 100
#i = 1
#x = 16

## cookies2 arguments
#dat = dataList[[x]]
#xy = b.xy
#uniqID = "cell"
#seeding = NULL
#rarefaction = rarefaction
#iter = reps
#nSite = settings[i,1]
#nOcc = occs.n
#oThreshold = overlapThreshold
#oType = overlapType
#oPruningMode = overlapPruningMode
#r = settings[i,2]
#crs = b.crs
#output = 'full'

## Read in outputs of occurrence counting
stages.s200.occs.count <- read.csv("data/occurrence_count/stages_s200_occ_count.csv", row.names = 1)
stages.g200.occs.count <- read.csv("data/occurrence_count/stages_g200_occ_count.csv", row.names = 1)
stages.s200.epif.occs.count <- read.csv("data/occurrence_count/stages_s200_epif_occ_count.csv", row.names = 1)
stages.g200.epif.occs.count <- read.csv("data/occurrence_count/stages_g200_epif_occ_count.csv", row.names = 1)

## Get minima for different configurations
stages.g200.occ.n <- stages.g200.occs.count[,"minimum"]
stages.s200.occ.n <- stages.s200.occs.count[,"minimum"]
stages.g200.epif.occ.n <- stages.g200.epif.occs.count[,"minimum"]
stages.s200.epif.occ.n <- stages.s200.epif.occs.count[,"minimum"]

occ.list <- list(stages.g200.occ.n,
                 stages.s200.occ.n,
                 100,
                 100,
                 stages.g200.epif.occ.n,
                 stages.s200.epif.occ.n)

## get vars
vars <- list(siteQuotas, radii)
names(vars) <- c("sQ","r")

## use biscuitsBatch to run all permutations, using minimum occurrence number for each run as occs number
for(z in 1:length(output.vector)){
cookies2Batch(dataList = eval(parse(text=data.strings[z])), vars = vars, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, overlapPruningMode = "maxOccs", rarefaction = rarefaction.strings[z], nOccs = occ.list[[z]],
             name.output = output.vector[z], n.cores = 4, taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))
}

#### Drop time bins with no data from each dataset ####
## Load function and static arguments
source("functions/drop.unusable.bins.R")
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
output.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
taxa <- T
threshold = threshold.VC = 1
prefix.vector <- output.vector
out.pre.vector <- c("stages_g200_viaTimeBin",
                    "stages_s200_viaTimeBin",
                    "stages_g200_ref_viaTimeBin",
                    "stages_s200_ref_viaTimeBin",
                    "stages_g200_epif_viaTimeBin",
                    "stages_s200_epif_viaTimeBin")

## drop unusable bins arguments
#i = 1
#input.dir = input.dir
#input.pre = prefix.vector[i]
#output.dir = output.dir
#output.pre = out.pre.vector[i]
#vars = vars
#sD = eval(parse(text=data.strings[i]))
#threshold = threshold.VC
#taxa = T

## Run function
for(z in 1:length(out.pre.vector)){
  drop.unusable.bins(input.dir = input.dir, input.pre = prefix.vector[z], output.dir = output.dir, output.pre = out.pre.vector[z],
                     vars = vars, sD = eval(parse(text=data.strings[z])), threshold = threshold.VC, taxa = T)
}

#### Count number of radially constrained regions in each time bin ####
source("functions/count.viable.samples.R")
dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
cores <- 4
taxa.split <- T
prefix.vector <- output.vector

## count viable samples arguments
#z = 1
#dir = dir
#pre = prefix.vector[z]
#vars = vars
#sD = eval(parse(text=data.strings[z]))
#n.cores = cores
#taxa = taxa.split
#output.dir = "~/R_packages/R_projects/bivbrach/data"
#output.name = prefix.vector[z]

for(z in 1:length(prefix.vector)){
  count.viable.samples(dir = dir, pre = prefix.vector[z], vars = vars, sD = eval(parse(text=data.strings[z])), n.cores = cores, taxa = taxa.split, output.dir = "~/R_packages/bivbrach/data/viable_subsample_count/", output.name = prefix.vector[z])
}

## read in cookie counts
stages.g200.VCs <- read.csv("data/viable_subsample_count/stages_g200_viable_subsamples.csv", header = T, row.names = 1)
stages.s200.VCs <- read.csv("data/viable_subsample_count/stages_s200_viable_subsamples.csv", header = T, row.names = 1)
stages.g200.ref.VCs <- read.csv("data/viable_subsample_count/stages_g200_ref_viable_subsamples.csv", header = T, row.names = 1)
stages.s200.ref.VCs <- read.csv("data/viable_subsample_count/stages_s200_ref_viable_subsamples.csv", header = T, row.names = 1)
stages.g200.epif.VCs <- read.csv("data/viable_subsample_count/stages_g200_epif_viable_subsamples.csv", header = T, row.names = 1)
stages.s200.epif.VCs <- read.csv("data/viable_subsample_count/stages_s200_epif_viable_subsamples.csv", header = T, row.names = 1)

## Update colnames using stage data to make them more usable
stages <- read.csv("data/cleaned_stages.csv", row.names = 1)
stages <- stages[order(stages$b_age, decreasing=TRUE), ]
colnames(stages.g200.VCs) <- rownames(stages)[-102]
colnames(stages.s200.VCs) <- rownames(stages)[-102]
colnames(stages.g200.ref.VCs) <- rownames(stages)[-102]
colnames(stages.s200.ref.VCs) <- rownames(stages)[-102]
colnames(stages.g200.epif.VCs) <- rownames(stages)[-102]
colnames(stages.s200.epif.VCs) <- rownames(stages)[-102]

## Get midpoints for plotting
midpoints <- as.numeric(names(stages.g200))

## Get period data for plotting geo scale
periods <- downloadTime("international periods")
periods <- periods[order(periods$b_age, decreasing=TRUE), ]
periods <- periods[periods$b_age <= periods[which(rownames(periods) == "Cambrian"),"b_age"],]

## Main title vector
titles <- c("Genera, 200km grid cells",
            "Species, 200km grid cells",
            "Genera, 200km grid cells, references refined",
            "Species, 200km grid cells, references refined",
            "Genera, 200km grid cells, epifaunal",
            "Species, 200km grid cells, epifaunal")

# Set output directory
output.dir <- "~/R_packages/bivbrach/figures/viable_RCR_through_time"

## Output file name vector
output.strings <- output.vector

## Input strings
input.strings <- c("stages.g200.VCs",
                   "stages.s200.VCs",
                   "stages.g200.ref.VCs",
                   "stages.s200.ref.VCs",
                   "stages.g200.epif.VCs",
                   "stages.s200.epif.VCs")

## Set legend position
legend.position = c("topright", "topright", "topright", "topright",
                    "topright", "topright")

## Set legend labels
legend.labels <- c("Radius 1000km, 2+ cells", "Radius 500km, 4+ cells")

## Set palette for lines
purples <- brewer.pal(n = 9, name = "Purples")[c(4,9)]

## Set line type palette
line.type.pal <- c(1,6)

## read in plotting functions
source("functions/shade.time.R")
source("functions/add.geo.scale.R")
source("functions/plot.spatSubThroughTime.R")

## Set increments
y.ax.inc <- 3
x.ax.inc <- 100

## Run plotting function
plot.spatSubThroughTime(input.strings, output.dir, output.strings, strat.data = periods, time.data = midpoints, titles, legend.position, legend.labels,
                        y.axis.inc = y.ax.inc, x.axis.inc = x.ax.inc, line.pal = purples, line.type.pal)

#### Subsampling references ####
## First, need to drop cookies with fewer than 3/5 references for bivalves and brachiopods
input.strings <- output.strings <- c("stages_g200_ref_viaTimeBin",
                                     "stages_s200_ref_viaTimeBin")

## Set input and output directory
input.dir <- output.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))

## Drop cookies that don't have minimum 3/5 references for bivalves and brachiopods
source("functions/applyRefThresh.R")

## Arguments
#t = 1
#input.dir = input.dir
#input.pre = input.strings[t]
#output.dir = output.dir
#output.pre = output.strings[t]
#vars = vars
#categories = list("Bivalvia" = c("biv", "both"), "Brachiopoda" = c("brach","both"))
#all.var.comb = F
#taxa = T
#n.cores = 4
#threshold = 3
#i = 1
#x = 1
#y = 1
#z = 1

## Run the function
for(t in 1:length(input.strings)){
  applyRefThresh(input.dir = input.dir, input.pre = input.strings[t], output.dir = output.dir, output.pre = output.strings[t], vars = vars, categories = list("Bivalvia" = c("biv", "both"), "Brachiopoda" = c("brach","both")), taxa = T, all.var.comb = F, threshold = 3, n.cores = 4)
}

## Then subsample 3/5 references for bivalves and brachiopods
## Load function
source("functions/subsample.references.R")

## Arguments
#t = 1
#input.dir = input.dir
#input.pre = input.strings[t]
#output.dir = output.dir
#output.pre = output.strings[t]
#vars = vars
#nRefs = 3
#reps = 100
#taxa = T
#n.cores = 4
#all.var.comb = F
#d = 1

## Run function
for(t in 1:length(input.strings)){
  subsample.references(input.dir = input.dir, input.pre = input.strings[t], output.dir = output.dir, output.pre = output.strings[t],
                     vars = vars, nRefs = 3, reps = 100, taxa = T, n.cores = 4)
}
