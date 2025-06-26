## 3. Spatial subsampling using modified divvy
## Started by TJS on 08/01/2024

#### Set up - run before any subsection ####
## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("divvy", "remotes", "stringr", "parallel")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divvy)
library(stringr)
library(parallel)
#library(remotes)

## install divvyCompanion from github and load
#remotes::install_github("PalaeoTom/divvyCompanion")
library(divvyCompanion)

## Load data
setwd("~/R_packages/bivbrach")
genera <- readRDS("data/final/master_2_2_uniq.Rds")
species <- readRDS("data/final/master_2_2_uniq_species.Rds")
home <- getwd()

#### Traybake sensitivity testing ####
##### Genera #####
## Get stage numbers
stages <- sort(unique(genera$stage))

## Add sites
sites2 <- cbind("stages"=stages, "grid_cells"=rep(2, length(stages)))
sites3 <- cbind("stages"=stages, "grid_cells"=rep(3, length(stages)))
sites4 <- cbind("stages"=stages, "grid_cells"=rep(4, length(stages)))
sites <- rbind(sites2, sites3, sites4)

## Add radii
options(scipen = 9999)
radius50 <- cbind(sites, "radius"=rep(50000,nrow(sites)))
radius100 <- cbind(sites, "radius"=rep(100000,nrow(sites)))
radius200 <- cbind(sites, "radius"=rep(200000,nrow(sites)))
radius <- rbind(radius50, radius100, radius200)

## Add containers
genera_count <- cbind(radius, "nGridCellsInTimeBin"=rep(NA,nrow(radius)))
genera_count <- cbind(genera_count, "nGridCellsIncludedInSubsamples"=rep(NA,nrow(genera_count)))
genera_count <- cbind(genera_count, "pGridCellsIncludedInSubsamples"=rep(NA,nrow(genera_count)))
genera_count <- cbind(genera_count, "nOccurrencesInTimeBin"=rep(NA,nrow(genera_count)))
genera_count <- cbind(genera_count, "nOccurrencesIncludedInSubsamples"=rep(NA,nrow(genera_count)))
genera_count <- cbind(genera_count, "pOccurrencesIncludedInSubsamples"=rep(NA,nrow(genera_count)))
genera_count <- cbind(genera_count, "nClusters"=rep(NA,nrow(genera_count)))
genera_count <- cbind(genera_count, "nRCRs"=rep(NA,nrow(genera_count)))
genera_count <- cbind(genera_count, "maxNumberOfRCRsInCluster"=rep(NA,nrow(genera_count)))
genera_count <- data.frame(genera_count)

## Format when testing
## Populated up until t=40
start = 1
for(t in start:nrow(genera_count)){
  print(t)
  ## Get time bin
  samp <- genera[which(genera$stage == genera_count[t,"stages"]),]
  ## genera_count number of grid cells and occurrences in time
  genera_count[t,"nGridCellsInTimeBin"] <- length(unique(samp[,"cell_50km"]))
  genera_count[t,"nOccurrencesInTimeBin"] <- nrow(samp)
  ## If nrow 0, skip
  if(nrow(samp)>0){
    ## If not, get RCRs in Clusters
    raw <- traybake(samp, xy = c("cellx_50km", "celly_50km"), uniqID = "cell_50km", r = genera_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = genera_count[t,"grid_cells"], nCookie = 100, nBatch = 100, n.cores = 8)
    seeds <- traybake(samp, xy = c("cellx_50km", "celly_50km"), uniqID = "cell_50km", r = genera_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = genera_count[t,"grid_cells"], nCookie = 100, nBatch = 100, output = "seeds", n.cores = 8)
    if(!is.null(raw) && !is.null(seeds)){
    ## count clusters
    genera_count[t,"nClusters"] <- length(seeds)
    ## count RCRs
    genera_count[t,"nRCRs"] <- length(unique(unlist(lapply(1:length(raw), function(x) names(raw[[x]])))))
    ## Get maximum RCRs in clusters
    genera_count[t,"maxNumberOfRCRsInCluster"] <- max(sapply(1:length(seeds), function(x) nrow(seeds[[x]])))
    ## Create new version of samp
    stanSamp <- lapply(1:length(raw), function(x){
      out <- do.call(rbind, raw[[x]])
    })
    stanSamp <- do.call(rbind,stanSamp)
    stanSamp <- stanSamp[!duplicated(stanSamp),]
    ## Get grid cells
    genera_count[t,"nGridCellsIncludedInSubsamples"] <- length(unique(stanSamp[,"cell_50km"]))
    genera_count[t,"pGridCellsIncludedInSubsamples"] <- round(genera_count[t,"nGridCellsIncludedInSubsamples"]/genera_count[t,"nGridCellsInTimeBin"], digits = 3)
    ## Get occurrences
    genera_count[t,"nOccurrencesIncludedInSubsamples"] <- nrow(stanSamp)
    genera_count[t,"pOccurrencesIncludedInSubsamples"] <- round(genera_count[t,"nOccurrencesIncludedInSubsamples"]/genera_count[t,"nOccurrencesInTimeBin"], digits = 3)
    }
  } else {
    next
  }
}

## Update NAs to 0s and re-calculate
for(t in 1:nrow(genera_count)){
  if(is.na(genera_count[t,"nClusters"])){
    genera_count[t,"nClusters"] <- 0
  }
  if(is.na(genera_count[t,"nRCRs"])){
    genera_count[t,"nRCRs"] <- 0
  }
  if(is.na(genera_count[t,"maxNumberOfRCRsInCluster"])){
    genera_count[t,"maxNumberOfRCRsInCluster"] <- 0
  }
  if(is.na(genera_count[t,"nGridCellsIncludedInSubsamples"])){
    genera_count[t,"nGridCellsIncludedInSubsamples"] <- 0
    genera_count[t,"pGridCellsIncludedInSubsamples"] <- round(genera_count[t,"nGridCellsIncludedInSubsamples"]/genera_count[t,"nGridCellsInTimeBin"], digits = 3)
  }
  if(is.na(genera_count[t,"nOccurrencesIncludedInSubsamples"])){
    genera_count[t,"nOccurrencesIncludedInSubsamples"] <- 0
    genera_count[t,"pOccurrencesIncludedInSubsamples"] <- round(genera_count[t,"nOccurrencesIncludedInSubsamples"]/genera_count[t,"nOccurrencesInTimeBin"], digits = 3)
  }
}

## Save once done
write.csv(genera_count, file="data/counting_occurrences/genera_radius_nSite_sensitivity_testing.csv")

## Then summarise for each method. Number of stage_grid_cells retained, number of occurrences.
genera_summary <- genera_count[!duplicated(genera_count[,c(2,3)]),c(2,3)]
genera_summary$totalGridCells <- sum(genera_count[intersect(which(genera_count[,"radius"]==50000),which(genera_count[,"grid_cells"]==2)),"nGridCellsInTimeBin"])
genera_summary$totalOccurrences <- sum(genera_count[intersect(which(genera_count[,"radius"]==50000),which(genera_count[,"grid_cells"]==2)),"nOccurrencesInTimeBin"])
genera_summary$gridCellsSampled <- NA
genera_summary$occurrencesSampled <- NA
genera_summary$proportionOfGridCells <- NA
genera_summary$proportionOfOccurrences <- NA
genera_summary$maxRCRsInCluster <- NA

for(r in 1:nrow(genera_summary)){
  genera_summary[r,"gridCellsSampled"] <- sum(genera_count[intersect(which(genera_count[,"radius"]==genera_summary[r,"radius"]),which(genera_count[,"grid_cells"]==genera_summary[r,"grid_cells"])),"nGridCellsIncludedInSubsamples"])
  genera_summary[r,"occurrencesSampled"] <- sum(genera_count[intersect(which(genera_count[,"radius"]==genera_summary[r,"radius"]),which(genera_count[,"grid_cells"]==genera_summary[r,"grid_cells"])),"nOccurrencesIncludedInSubsamples"])
  genera_summary[r,"proportionOfGridCells"] <- round(genera_summary[r,"gridCellsSampled"]/genera_summary[r,"totalGridCells"], digits = 3)
  genera_summary[r,"proportionOfOccurrences"] <- round(genera_summary[r,"occurrencesSampled"]/genera_summary[r,"totalOccurrences"], digits = 3)
  genera_summary[r,"maxRCRsInCluster"] <- max(genera_count[intersect(which(genera_count[,"radius"]==genera_summary[r,"radius"]),which(genera_count[,"grid_cells"]==genera_summary[r,"grid_cells"])),"maxNumberOfRCRsInCluster"])
}

## Export genera_summary for reference
## Save once done
write.csv(genera_summary, file="data/counting_occurrences/genera_sensitivity_testing_summary.csv")

##### Species #####
## Get stage numbers
stages <- sort(unique(species$stage))

## Add sites
sites2 <- cbind("stages"=stages, "grid_cells"=rep(2, length(stages)))
sites3 <- cbind("stages"=stages, "grid_cells"=rep(3, length(stages)))
sites4 <- cbind("stages"=stages, "grid_cells"=rep(4, length(stages)))
sites <- rbind(sites2, sites3, sites4)

## Add radii
options(scipen = 9999)
radius50 <- cbind(sites, "radius"=rep(50000,nrow(sites)))
radius100 <- cbind(sites, "radius"=rep(100000,nrow(sites)))
radius200 <- cbind(sites, "radius"=rep(200000,nrow(sites)))
radius <- rbind(radius50, radius100, radius200)

## Add containers
species_count <- cbind(radius, "nGridCellsInTimeBin"=rep(NA,nrow(radius)))
species_count <- cbind(species_count, "nGridCellsIncludedInSubsamples"=rep(NA,nrow(species_count)))
species_count <- cbind(species_count, "pGridCellsIncludedInSubsamples"=rep(NA,nrow(species_count)))
species_count <- cbind(species_count, "nOccurrencesInTimeBin"=rep(NA,nrow(species_count)))
species_count <- cbind(species_count, "nOccurrencesIncludedInSubsamples"=rep(NA,nrow(species_count)))
species_count <- cbind(species_count, "pOccurrencesIncludedInSubsamples"=rep(NA,nrow(species_count)))
species_count <- cbind(species_count, "nClusters"=rep(NA,nrow(species_count)))
species_count <- cbind(species_count, "nRCRs"=rep(NA,nrow(species_count)))
species_count <- cbind(species_count, "maxNumberOfRCRsInCluster"=rep(NA,nrow(species_count)))
species_count <- data.frame(species_count)

## Format when testing
## Populated up until t=40
start = 1
for(t in start:nrow(species_count)){
  print(t)
  ## Get time bin
  samp <- species[which(species$stage == species_count[t,"stages"]),]
  ## species_count number of grid cells and occurrences in time
  species_count[t,"nGridCellsInTimeBin"] <- length(unique(samp[,"cell_50km"]))
  species_count[t,"nOccurrencesInTimeBin"] <- nrow(samp)
  ## If nrow 0, skip
  if(nrow(samp)>0){
    ## If not, get RCRs in Clusters
    raw <- traybake(samp, xy = c("cellx_50km", "celly_50km"), uniqID = "cell_50km", r = species_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = species_count[t,"grid_cells"], nCookie = 100, nBatch = 100, n.cores = 8)
    seeds <- traybake(samp, xy = c("cellx_50km", "celly_50km"), uniqID = "cell_50km", r = species_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = species_count[t,"grid_cells"], nCookie = 100, nBatch = 100, output = "seeds", n.cores = 8)
    if(!is.null(raw) && !is.null(seeds)){
      ## count clusters
      species_count[t,"nClusters"] <- length(seeds)
      ## count RCRs
      species_count[t,"nRCRs"] <- length(unique(unlist(lapply(1:length(raw), function(x) names(raw[[x]])))))
      ## Get maximum RCRs in clusters
      species_count[t,"maxNumberOfRCRsInCluster"] <- max(sapply(1:length(seeds), function(x) nrow(seeds[[x]])))
      ## Create new version of samp
      stanSamp <- lapply(1:length(raw), function(x){
        out <- do.call(rbind, raw[[x]])
      })
      stanSamp <- do.call(rbind,stanSamp)
      stanSamp <- stanSamp[!duplicated(stanSamp),]
      ## Get grid cells
      species_count[t,"nGridCellsIncludedInSubsamples"] <- length(unique(stanSamp[,"cell_50km"]))
      species_count[t,"pGridCellsIncludedInSubsamples"] <- round(species_count[t,"nGridCellsIncludedInSubsamples"]/species_count[t,"nGridCellsInTimeBin"], digits = 3)
      ## Get occurrences
      species_count[t,"nOccurrencesIncludedInSubsamples"] <- nrow(stanSamp)
      species_count[t,"pOccurrencesIncludedInSubsamples"] <- round(species_count[t,"nOccurrencesIncludedInSubsamples"]/species_count[t,"nOccurrencesInTimeBin"], digits = 3)
    }
  } else {
    next
  }
}

## Update NAs to 0s and re-calculate
for(t in 1:nrow(species_count)){
  if(is.na(species_count[t,"nClusters"])){
    species_count[t,"nClusters"] <- 0
  }
  if(is.na(species_count[t,"nRCRs"])){
    species_count[t,"nRCRs"] <- 0
  }
  if(is.na(species_count[t,"maxNumberOfRCRsInCluster"])){
    species_count[t,"maxNumberOfRCRsInCluster"] <- 0
  }
  if(is.na(species_count[t,"nGridCellsIncludedInSubsamples"])){
    species_count[t,"nGridCellsIncludedInSubsamples"] <- 0
    species_count[t,"pGridCellsIncludedInSubsamples"] <- round(species_count[t,"nGridCellsIncludedInSubsamples"]/species_count[t,"nGridCellsInTimeBin"], digits = 3)
  }
  if(is.na(species_count[t,"nOccurrencesIncludedInSubsamples"])){
    species_count[t,"nOccurrencesIncludedInSubsamples"] <- 0
    species_count[t,"pOccurrencesIncludedInSubsamples"] <- round(species_count[t,"nOccurrencesIncludedInSubsamples"]/species_count[t,"nOccurrencesInTimeBin"], digits = 3)
  }
}

## Save once done
write.csv(species_count, file="data/counting_occurrences/species_radius_nSite_sensitivity_testing.csv")

## Then summarise for each method. Number of stage_grid_cells retained, number of occurrences.
species_summary <- species_count[!duplicated(species_count[,c(2,3)]),c(2,3)]
species_summary$totalGridCells <- sum(species_count[intersect(which(species_count[,"radius"]==50000),which(species_count[,"grid_cells"]==2)),"nGridCellsInTimeBin"])
species_summary$totalOccurrences <- sum(species_count[intersect(which(species_count[,"radius"]==50000),which(species_count[,"grid_cells"]==2)),"nOccurrencesInTimeBin"])
species_summary$gridCellsSampled <- NA
species_summary$occurrencesSampled <- NA
species_summary$proportionOfGridCells <- NA
species_summary$proportionOfOccurrences <- NA
species_summary$maxRCRsInCluster <- NA

for(r in 1:nrow(species_summary)){
  species_summary[r,"gridCellsSampled"] <- sum(species_count[intersect(which(species_count[,"radius"]==species_summary[r,"radius"]),which(species_count[,"grid_cells"]==species_summary[r,"grid_cells"])),"nGridCellsIncludedInSubsamples"])
  species_summary[r,"occurrencesSampled"] <- sum(species_count[intersect(which(species_count[,"radius"]==species_summary[r,"radius"]),which(species_count[,"grid_cells"]==species_summary[r,"grid_cells"])),"nOccurrencesIncludedInSubsamples"])
  species_summary[r,"proportionOfGridCells"] <- round(species_summary[r,"gridCellsSampled"]/species_summary[r,"totalGridCells"], digits = 3)
  species_summary[r,"proportionOfOccurrences"] <- round(species_summary[r,"occurrencesSampled"]/species_summary[r,"totalOccurrences"], digits = 3)
  species_summary[r,"maxRCRsInCluster"] <- max(species_count[intersect(which(species_count[,"radius"]==species_summary[r,"radius"]),which(species_count[,"grid_cells"]==species_summary[r,"grid_cells"])),"maxNumberOfRCRsInCluster"])
}

## Export species_summary for reference
## Save once done
write.csv(species_summary, file="data/counting_occurrences/species_sensitivity_testing_summary.csv")

#### Let's bake! ####
## Maximum number of 50km grid cells in a 200km-radius RCR is 81. In a 100km-radius RCR, this is 25.
## Maximum number of RCRs in cluster is 80
## Need a 1000 batches of 1000 cookies. Should run in batches of 100.
rm(list=ls())

## Read in data and stages
## Load data
setwd("~/R_packages/bivbrach")
genera <- readRDS("data/final/master_2_2_uniq.Rds")
species <- readRDS("data/final/master_2_2_uniq_species.Rds")
home <- getwd()

##### Genera #####
## Get stage numbers
stages <- sort(unique(genera$stage))

## Read in genera count
genera_count <- read.csv("data/counting_occurrences/genera_radius_nSite_sensitivity_testing.csv", header = T, row.names = 1)

## Summarise distribution of occurrences across grid cells - use this to inform how many to be drawn.
gcs <- unique(genera$stage_cell)
container <- c()
for(g in gcs){
  container <- c(container, length(which(genera$stage_cell==g)))
}
summary(container)
hist(container)
## Median cell contains 11 occurrences. Going to sample 20 occurrences

## Isolate relevant
nCells <- 2
radius <- 200000
cov.threshold <- 0.75
nBatch <- 500
nBites <- 500
nOccs <- 20
rel <- genera_count[intersect(which(genera_count$grid_cells == nCells), which(genera_count$radius == radius)),]

## Inititlise cluster vector
cluster <- data.frame("stage"=NA, "cluster"=NA)
for(r in 1:nrow(rel)){
  ## Get new index
  if(rel[r,"nClusters"]>0){
    stage <- rep(rel[r,"stages"],rel[r,"nClusters"])
    clus <- paste0(rel[r,"stages"], "_", seq(1,rel[r,"nClusters"],1))
    new <- cbind("stage"=stage,"cluster"=clus)
    cluster <- rbind(cluster,new)
  }
}
cluster <- cluster[-1,]

## Add batches
cluster$batch <- 1
output_2cell <- cluster
for(b in 2:nBatch){
  copy <- cluster
  copy[,"batch"] <- b
  output_2cell <- rbind(output_2cell,copy)
}

## Add columns to be populated
output_2cell$bivalve <- NA
output_2cell$brachiopod <- NA
output_2cell$cookieLith <- NA
output_2cell$cookieBath <- NA
output_2cell$cookieReef <- NA

## Output directory
output.dir <- "~/Library/CloudStorage/OneDrive-Nexus365/Bivalve_brachiopod/traybake_output"

## Read in richness function
source("functions/richness.R")

## For each stage
#i = 1
for(i in 1:length(stages)){
  ## Get sample
  samp <- genera[which(genera$stage == stages[i]),]
  if(nrow(samp)>0){
    ## Run traybake - standardise separately
    tray <- traybake(samp, xy = c("cellx_50km", "celly_50km"), uniqID = "cell_50km", r = radius, standardiseCells = F, exhaustClusters = F, nSite = nCells, nBatch = 500, n.cores = 8)
    ## If not null, proceed
    if(!is.null(tray)){
      ## Save output
      saveRDS(tray, paste0(output.dir, "/genera_stage", stages[i], "_", nCells, "GridCells.Rds"))
      ## For each batch
      #b = 1
      for(b in 1:length(tray)){
        batch <- tray[[b]]
        #c = 2
        for(c in 1:length(batch)){
          print(paste0("Stage ", i, ", Cluster ", str_flatten(c(stages[i],c),collapse = "_"), ", Batch ", b))
          ## Get row
          row <- intersect(intersect(which(output_2cell$stage == stages[i]),which(output_2cell$cluster == str_flatten(c(stages[i],c),collapse = "_"))),which(output_2cell$batch == b))
          ## isolate covariates, remove duplicates
          covariates <- batch[[c]][,c(40,45,46,47)]
          covariates <- covariates[!duplicated(covariates),]
          ## get cell lithology
          n <- nrow(covariates)
          ## cellLith
          if(any(table(covariates[,"cellLith"])/n > cov.threshold)){
            output_2cell[row,"cookieLith"] <- names(table(covariates[,"cellLith"]))[which(table(covariates[,"cellLith"])/n > cov.threshold)]
          } else {
            output_2cell[row,"cookieLith"] <- "mixed"
          }
          ## cellBath
          if(any(table(covariates[,"cellBath"])/n > cov.threshold)){
            output_2cell[row,"cookieBath"] <- names(table(covariates[,"cellBath"]))[which(table(covariates[,"cellBath"])/n > cov.threshold)]
          } else {
            output_2cell[row,"cookieBath"] <- "mixed"
          }
          ## cellReef
          if(any(table(covariates[,"cellReef"])/n > cov.threshold)){
            output_2cell[row,"cookieReef"] <- names(table(covariates[,"cellReef"]))[which(table(covariates[,"cellReef"])/n > cov.threshold)]
          } else {
            output_2cell[row,"cookieReef"] <- "mixed"
          }
          ## Now to write little function that iteratively draws nCells without replacement, then draws set number of occurrences with replacement, counts bivalves and brachiopods, then ultimately reports median of these values
          rich <- richness(data = batch[[c]], cellID = "cell_50km", nCells, nDraws = nBites, nOccs, taxonName = "combined_name", taxonomicRankToTally = "phylum", taxaToTally = c("Mollusca", "Brachiopoda"), n.cores = 8)
          ## Populate output
          output_2cell[row,"bivalve"] <- rich[1]
          output_2cell[row,"brachiopod"] <- rich[2]
        }
      }
    }
    } else {
      next
    }
}

## Add missing

## Export
write.csv(output_2cell, file = "data/analysis_data/genera_2cells_standardised.csv")





