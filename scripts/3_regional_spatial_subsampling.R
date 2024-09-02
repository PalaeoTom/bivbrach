## 3. Spatial subsampling using modified divvy
## Started by TJS on 08/01/2024

#### Set up - run before any subsection ####
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

## install divvyCompanion from github and load
library(remotes)
remotes::install_github("PalaeoTom/divvyCompanion")
library(divvyCompanion)

## Load data
setwd("~/R_packages/bivbrach")
stages.g200 <- readRDS("data/stages_g200.Rds")
stages.g100 <- readRDS("data/stages_g100.Rds")
stages.s200 <- readRDS("data/stages_s200.Rds")
stages.s100 <- readRDS("data/stages_s100.Rds")

stages.g200.ref <- readRDS("data/stages_g200_ref3.Rds")
stages.g100.ref <- readRDS("data/stages_g100_ref3.Rds")
stages.s200.ref <- readRDS("data/stages_s200_ref3.Rds")
stages.s100.ref <- readRDS("data/stages_s100_ref3.Rds")

stages.g200.inf <- readRDS("data/stages_g200_inf.Rds")
stages.g100.inf <- readRDS("data/stages_g100_inf.Rds")
stages.s200.inf <- readRDS("data/stages_s200_inf.Rds")
stages.s100.inf <- readRDS("data/stages_s100_inf.Rds")

stages.g200.epif <- readRDS("data/stages_g200_epif.Rds")
stages.g100.epif <- readRDS("data/stages_g100_epif.Rds")
stages.s200.epif <- readRDS("data/stages_s200_epif.Rds")
stages.s100.epif <- readRDS("data/stages_s100_epif.Rds")
home <- getwd()

## Load new functions
source("functions/cookies2Batch.R")

## Define variables
radii <- as.integer(c(200000, 500000, 1000000))
siteQuotas <- c(2, 3, 4, 5)
overlapThresholds <- 0
overlapTypes <- "sites"
vars <- list(siteQuotas, radii)
names(vars) <- c("sQ","r")

#### Get distribution of occurrence numbers for each subsample of sites ####
## Generate spatial subsamples, returning all occurrences associate with subsamples of sites
## Generate output vectors
output.strings.sites <- c("stages_g200",
                          "stages_g100",
                          "stages_s200",
                          "stages_s100",
                          "stages_g200_ref",
                          "stages_g100_ref",
                          "stages_s200_ref",
                          "stages_s100_ref",
                          "stages_g200_epif",
                          "stages_g100_epif",
                          "stages_s200_epif",
                          "stages_s100_epif",
                          "stages_g200_inf",
                          "stages_g100_inf",
                          "stages_s200_inf",
                          "stages_s100_inf")

data.strings <- c("stages.g200",
                  "stages.g100",
                  "stages.s200",
                  "stages.s100",
                  "stages.g200.ref",
                  "stages.g100.ref",
                  "stages.s200.ref",
                  "stages.s100.ref",
                  "stages.g200.epif",
                  "stages.g100.epif",
                  "stages.s200.epif",
                  "stages.s100.epif",
                  "stages.g200.inf",
                  "stages.g100.inf",
                  "stages.s200.inf",
                  "stages.s100.inf")

## use biscuitsBatch to run all permutations
for(z in 1:length(output.strings.sites)){
  cookies2Batch(dataList = eval(parse(text=data.strings[z])), vars = vars, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub",
                overlapThreshold = overlapThresholds, overlapType = overlapTypes, overlapPruningMode = "maxOccs", rarefaction = "sites",
                name.output = output.strings.sites[z], n.cores = 4, taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))
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
                    "stages_g100_viaTimeBin",
                    "stages_s200_viaTimeBin",
                    "stages_s100_viaTimeBin",
                    "stages_g200_ref_viaTimeBin",
                    "stages_g100_ref_viaTimeBin",
                    "stages_s200_ref_viaTimeBin",
                    "stages_s100_ref_viaTimeBin",
                    "stages_g200_epif_viaTimeBin",
                    "stages_g100_epif_viaTimeBin",
                    "stages_s200_epif_viaTimeBin",
                    "stages_s100_epif_viaTimeBin",
                    "stages_g200_inf_viaTimeBin",
                    "stages_g100_inf_viaTimeBin",
                    "stages_s200_inf_viaTimeBin",
                    "stages_s100_inf_viaTimeBin")

## Run function
for(z in 1:length(out.pre.vector)){
  drop.unusable.bins(input.dir = input.dir, input.pre = prefix.vector[z], output.dir = output.dir, output.pre = out.pre.vector[z],
                     vars = vars, sD = eval(parse(text=data.strings[z])), threshold = threshold.VC, taxa = T)
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
                    "stages_g100_occ_count",
                    "stages_s200_occ_count",
                    "stages_s100_occ_count",
                    "stages_g200_ref_occ_count",
                    "stages_g100_ref_occ_count",
                    "stages_s200_ref_occ_count",
                    "stages_s100_ref_occ_count",
                    "stages_g200_epif_occ_count",
                    "stages_g100_epif_occ_count",
                    "stages_s200_epif_occ_count",
                    "stages_s100_epif_occ_count",
                    "stages_g200_inf_occ_count",
                    "stages_g100_inf_occ_count",
                    "stages_s200_inf_occ_count",
                    "stages_s100_inf_occ_count")
## Run the function
for(z in 1:length(output.strings)){
  count.occurrences(input.dir = input.dir, input.pre = input.strings[z], output.dir = output.dir, output.pre = output.strings[z],
                    vars = vars, method = "sites", n.cores = 4, taxa = T)
}

#### Further testing occurrence numbers - get occurrences by cell for each dataset ####
source("functions/get.occs.per.cell.R")

## Get occurrences per cell
stages.g200.occs <- get.occs.per.cell(stages.g200)
stages.g100.occs <- get.occs.per.cell(stages.g100)
stages.s200.occs <- get.occs.per.cell(stages.s200)
stages.s100.occs <- get.occs.per.cell(stages.s100)
stages.g200.ref.occs <- get.occs.per.cell(stages.g200.ref)
stages.g100.ref.occs <- get.occs.per.cell(stages.g100.ref)
stages.s200.ref.occs <- get.occs.per.cell(stages.s200.ref)
stages.s100.ref.occs <- get.occs.per.cell(stages.s100.ref)
stages.g200.epif.occs <- get.occs.per.cell(stages.g200.epif)
stages.g100.epif.occs <- get.occs.per.cell(stages.g100.epif)
stages.s200.epif.occs <- get.occs.per.cell(stages.s200.epif)
stages.s100.epif.occs <- get.occs.per.cell(stages.s100.epif)
stages.g200.inf.occs <- get.occs.per.cell(stages.g200.inf)
stages.g100.inf.occs <- get.occs.per.cell(stages.g100.inf)
stages.s200.inf.occs <- get.occs.per.cell(stages.s200.inf)
stages.s100.inf.occs <- get.occs.per.cell(stages.s100.inf)

#### Generate spatial subsamples ####
## Generate input and output vectors once again
output.vector <- c("stages_g200",
                   "stages_g100",
                   "stages_s200",
                   "stages_s100",
                   "stages_g200_ref",
                   "stages_g100_ref",
                   "stages_s200_ref",
                   "stages_s100_ref",
                   "stages_g200_epif",
                   "stages_g100_epif",
                   "stages_s200_epif",
                   "stages_s100_epif",
                   "stages_g200_inf",
                   "stages_g100_inf",
                   "stages_s200_inf",
                   "stages_s100_inf")

data.strings <- c("stages.g200",
                  "stages.g100",
                  "stages.s200",
                  "stages.s100",
                  "stages.g200.ref",
                  "stages.g100.ref",
                  "stages.s200.ref",
                  "stages.s100.ref",
                  "stages.g200.epif",
                  "stages.g100.epif",
                  "stages.s200.epif",
                  "stages.s100.epif",
                  "stages.g200.inf",
                  "stages.g100.inf",
                  "stages.s200.inf",
                  "stages.s100.inf")

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
stages.s100.occs.count <- read.csv("data/occurrence_count/stages_s100_occ_count.csv", row.names = 1)
stages.s200.occs.count <- read.csv("data/occurrence_count/stages_s200_occ_count.csv", row.names = 1)
stages.g100.occs.count <- read.csv("data/occurrence_count/stages_g100_occ_count.csv", row.names = 1)
stages.g200.occs.count <- read.csv("data/occurrence_count/stages_g200_occ_count.csv", row.names = 1)
stages.s100.ref.occs.count <- read.csv("data/occurrence_count/stages_s100_ref_occ_count.csv", row.names = 1)
stages.s200.ref.occs.count <- read.csv("data/occurrence_count/stages_s200_ref_occ_count.csv", row.names = 1)
stages.g100.ref.occs.count <- read.csv("data/occurrence_count/stages_g100_ref_occ_count.csv", row.names = 1)
stages.g200.ref.occs.count <- read.csv("data/occurrence_count/stages_g200_ref_occ_count.csv", row.names = 1)
stages.s100.epif.occs.count <- read.csv("data/occurrence_count/stages_s100_epif_occ_count.csv", row.names = 1)
stages.s200.epif.occs.count <- read.csv("data/occurrence_count/stages_s200_epif_occ_count.csv", row.names = 1)
stages.g100.epif.occs.count <- read.csv("data/occurrence_count/stages_g100_epif_occ_count.csv", row.names = 1)
stages.g200.epif.occs.count <- read.csv("data/occurrence_count/stages_g200_epif_occ_count.csv", row.names = 1)
stages.s100.inf.occs.count <- read.csv("data/occurrence_count/stages_s100_inf_occ_count.csv", row.names = 1)
stages.s200.inf.occs.count <- read.csv("data/occurrence_count/stages_s200_inf_occ_count.csv", row.names = 1)
stages.g100.inf.occs.count <- read.csv("data/occurrence_count/stages_g100_inf_occ_count.csv", row.names = 1)
stages.g200.inf.occs.count <- read.csv("data/occurrence_count/stages_g200_inf_occ_count.csv", row.names = 1)

## Get minima for different configurations
stages.g200.occ.n <- stages.g200.occs.count[,"minimum"]
stages.g100.occ.n <- stages.g100.occs.count[,"minimum"]
stages.s200.occ.n <- stages.s200.occs.count[,"minimum"]
stages.s100.occ.n <- stages.s100.occs.count[,"minimum"]
stages.g200.ref.occ.n <- stages.g200.ref.occs.count[,"minimum"]
stages.g100.ref.occ.n <- stages.g100.ref.occs.count[,"minimum"]
stages.s200.ref.occ.n <- stages.s200.ref.occs.count[,"minimum"]
stages.s100.ref.occ.n <- stages.s100.ref.occs.count[,"minimum"]
stages.g200.epif.occ.n <- stages.g200.epif.occs.count[,"minimum"]
stages.g100.epif.occ.n <- stages.g100.epif.occs.count[,"minimum"]
stages.s200.epif.occ.n <- stages.s200.epif.occs.count[,"minimum"]
stages.s100.epif.occ.n <- stages.s100.epif.occs.count[,"minimum"]
stages.g200.inf.occ.n <- stages.g200.inf.occs.count[,"minimum"]
stages.g100.inf.occ.n <- stages.g100.inf.occs.count[,"minimum"]
stages.s200.inf.occ.n <- stages.s200.inf.occs.count[,"minimum"]
stages.s100.inf.occ.n <- stages.s100.inf.occs.count[,"minimum"]

occ.list <- list(stages.g200.occ.n,
                 stages.g100.occ.n,
                 stages.s200.occ.n,
                 stages.s100.occ.n,
                 stages.g200.ref.occ.n,
                 stages.g100.ref.occ.n,
                 stages.s200.ref.occ.n,
                 stages.s100.ref.occ.n,
                 stages.g200.epif.occ.n,
                 stages.g100.epif.occ.n,
                 stages.s200.epif.occ.n,
                 stages.s100.epif.occ.n,
                 stages.g200.inf.occ.n,
                 stages.g100.inf.occ.n,
                 stages.s200.inf.occ.n,
                 stages.s100.inf.occ.n)

## get vars
vars <- list(siteQuotas, radii)
names(vars) <- c("sQ","r")

## use biscuitsBatch to run all permutations, using minimum occurrence number for each run as occs number
for(z in 1:length(output.vector)){
cookies2Batch(dataList = eval(parse(text=data.strings[z])), vars = vars, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub",
             overlapThreshold = overlapThresholds, overlapType = overlapTypes, overlapPruningMode = "maxOccs", rarefaction = "sitesThenOccs", nOccs = occ.list[[z]],
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
                    "stages_g100_viaTimeBin",
                    "stages_s200_viaTimeBin",
                    "stages_s100_viaTimeBin",
                    "stages_g200_ref_viaTimeBin",
                    "stages_g100_ref_viaTimeBin",
                    "stages_s200_ref_viaTimeBin",
                    "stages_s100_ref_viaTimeBin",
                    "stages_g200_epif_viaTimeBin",
                    "stages_g100_epif_viaTimeBin",
                    "stages_s200_epif_viaTimeBin",
                    "stages_s100_epif_viaTimeBin",
                    "stages_g200_inf_viaTimeBin",
                    "stages_g100_inf_viaTimeBin",
                    "stages_s200_inf_viaTimeBin",
                    "stages_s100_inf_viaTimeBin")

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
stages.g100.VCs <- read.csv("data/viable_subsample_count/stages_g100_viable_subsamples.csv", header = T, row.names = 1)
stages.s200.VCs <- read.csv("data/viable_subsample_count/stages_s200_viable_subsamples.csv", header = T, row.names = 1)
stages.s100.VCs <- read.csv("data/viable_subsample_count/stages_s100_viable_subsamples.csv", header = T, row.names = 1)
stages.g200.ref.VCs <- read.csv("data/viable_subsample_count/stages_g200_ref_viable_subsamples.csv", header = T, row.names = 1)
stages.g100.ref.VCs <- read.csv("data/viable_subsample_count/stages_g100_ref_viable_subsamples.csv", header = T, row.names = 1)
stages.s200.ref.VCs <- read.csv("data/viable_subsample_count/stages_s200_ref_viable_subsamples.csv", header = T, row.names = 1)
stages.s100.ref.VCs <- read.csv("data/viable_subsample_count/stages_s100_ref_viable_subsamples.csv", header = T, row.names = 1)
stages.g200.epif.VCs <- read.csv("data/viable_subsample_count/stages_g200_epif_viable_subsamples.csv", header = T, row.names = 1)
stages.g100.epif.VCs <- read.csv("data/viable_subsample_count/stages_g100_epif_viable_subsamples.csv", header = T, row.names = 1)
stages.s200.epif.VCs <- read.csv("data/viable_subsample_count/stages_s200_epif_viable_subsamples.csv", header = T, row.names = 1)
stages.s100.epif.VCs <- read.csv("data/viable_subsample_count/stages_s100_epif_viable_subsamples.csv", header = T, row.names = 1)
stages.g200.inf.VCs <- read.csv("data/viable_subsample_count/stages_g200_inf_viable_subsamples.csv", header = T, row.names = 1)
stages.g100.inf.VCs <- read.csv("data/viable_subsample_count/stages_g100_inf_viable_subsamples.csv", header = T, row.names = 1)
stages.s200.inf.VCs <- read.csv("data/viable_subsample_count/stages_s200_inf_viable_subsamples.csv", header = T, row.names = 1)
stages.s100.inf.VCs <- read.csv("data/viable_subsample_count/stages_s100_inf_viable_subsamples.csv", header = T, row.names = 1)

## Update colnames using stage data to make them more usable
stages <- read.csv("data/cleaned_stages.csv", row.names = 1)
stages <- stages[order(stages$b_age, decreasing=TRUE), ]
colnames(stages.g200.VCs) <- rownames(stages)[-102]
colnames(stages.g100.VCs) <- rownames(stages)[-102]
colnames(stages.s200.VCs) <- rownames(stages)[-102]
colnames(stages.s100.VCs) <- rownames(stages)[-102]
colnames(stages.g200.ref.VCs) <- rownames(stages)[-102]
colnames(stages.g100.ref.VCs) <- rownames(stages)[-102]
colnames(stages.s200.ref.VCs) <- rownames(stages)[-102]
colnames(stages.s100.ref.VCs) <- rownames(stages)[-102]
colnames(stages.g200.epif.VCs) <- rownames(stages)[-102]
colnames(stages.g100.epif.VCs) <- rownames(stages)[-102]
colnames(stages.s200.epif.VCs) <- rownames(stages)[-102]
colnames(stages.s100.epif.VCs) <- rownames(stages)[-102]
colnames(stages.g200.inf.VCs) <- rownames(stages)[-102]
colnames(stages.g100.inf.VCs) <- rownames(stages)[-102]
colnames(stages.s200.inf.VCs) <- rownames(stages)[-102]
colnames(stages.s100.inf.VCs) <- rownames(stages)[-102]

## Get midpoints for plotting
midpoints <- as.numeric(names(stages.g200))

## Get period data for plotting geo scale
periods <- downloadTime("international periods")
periods <- periods[order(periods$b_age, decreasing=TRUE), ]
periods <- periods[periods$b_age <= periods[which(rownames(periods) == "Cambrian"),"b_age"],]

## Main title vector
titles <- c("Genera, 200km grid cells", "Genera, 100km grid cells", "Species, 200km grid cells", "Species, 100km grid cells",
            "Genera, 200km grid cells, 3+ references for both", "Genera, 100km grid cells, 3+ references for both", "Species, 200km grid cells, 3+ references for both", "Species, 100km grid cells, 3+ references for both",
            "Genera, 200km grid cells, epifaunal", "Genera, 100km grid cells, epifaunal", "Species, 200km grid cells, epifaunal", "Species, 100km grid cells, epifaunal",
            "Genera, 200km grid cells, infaunal", "Genera, 100km grid cells, infaunal", "Species, 200km grid cells, infaunal", "Species, 100km grid cells, infaunal")

# Set output directory
output.dir <- "~/R_packages/bivbrach/figures/viable_RCR_through_time"

## Output file name vector
output.strings <- output.vector

## Input strings
input.strings <- c("stages.g200.VCs", "stages.g100.VCs", "stages.s200.VCs", "stages.s100.VCs",
                   "stages.g200.ref.VCs", "stages.g100.ref.VCs", "stages.s200.ref.VCs", "stages.s100.ref.VCs",
                   "stages.g200.epif.VCs", "stages.g100.epif.VCs", "stages.s200.epif.VCs", "stages.s100.epif.VCs",
                   "stages.g200.inf.VCs", "stages.g100.inf.VCs", "stages.s200.inf.VCs", "stages.s100.inf.VCs")

## Set legend position
legend.position = c("topright", "topright", "topright", "topright",
                    "topright", "topright", "topright", "topright",
                    "topright", "topright", "topright", "topright",
                    "topright", "topright", "topright", "topright")

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
y.ax.inc <- 3
x.ax.inc <- 100

## Run plotting function
plot.spatSubThroughTime(input.strings, output.dir, output.strings, strat.data = periods, time.data = midpoints, titles, legend.position, legend.labels,
                        y.axis.inc = y.ax.inc, x.axis.inc = x.ax.inc, line.pal = c(oranges, greens, purples), line.type.pal)

#### Sensitivity test - subsample by sites first, then by references ####
output.vector <- c("stages_g200_sites",
                   "stages_g100_sites",
                   "stages_s200_sites",
                   "stages_s100_sites")

data.strings <- c("stages.g200.ref",
                  "stages.g100.ref",
                  "stages.s200.ref",
                  "stages.s100.ref")

## get vars
vars <- list(siteQuotas, radii)
names(vars) <- c("sQ","r")

## use biscuitsBatch to run all permutations, using minimum occurrence number for each run as occs number
for(z in 1:length(output.vector)){
  cookies2Batch(dataList = eval(parse(text=data.strings[z])), vars = vars, b.crs = 'EPSG:8857', output.dir = "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub",
                overlapThreshold = overlapThresholds, overlapType = overlapTypes, overlapPruningMode = "maxOccs", rarefaction = "sites",
                name.output = output.vector[z], n.cores = 4, taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))
}

## Drop time bins with no data from each dataset ##
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
output.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
taxa <- T
threshold = threshold.VC = 1
prefix.vector <- output.vector
out.pre.vector <- c("stages_g200_sites_viaTimeBin",
                    "stages_g100_sites_viaTimeBin",
                    "stages_s200_sites_viaTimeBin",
                    "stages_s100_sites_viaTimeBin")
source("functions/drop.unusable.bins.R")

## Run function
for(z in 1:length(out.pre.vector)){
  drop.unusable.bins(input.dir = input.dir, input.pre = prefix.vector[z], output.dir = output.dir, output.pre = out.pre.vector[z],
                     vars = vars, sD = eval(parse(text=data.strings[z])), threshold = threshold.VC, taxa = T)
}

## Now subsample 3 references for each taxon
sitesThenRefs.vector <- c("stages_g200_sitesThenRefs_VTBO",
                          "stages_g100_sitesThenRefs_VTBO",
                          "stages_s200_sitesThenRefs_VTBO",
                          "stages_s100_sitesThenRefs_VTBO")

## Load function
source("functions/subsample.references.R")

## Run function
for(t in 1:length(sitesThenRefs.vector)){
  subsample.references(input.dir = input.dir, input.pre = out.pre.vector[t], output.dir = output.dir, output.pre = sitesThenRefs.vector[t],
                     vars = vars, nRefs = 3, taxa = T, n.cores = 4)
}
