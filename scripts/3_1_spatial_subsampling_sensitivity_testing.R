## 3.1 Testing data retention for different combinations of radius, cells sampled, grid cell size
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
genera_50 <- readRDS("data/final/master_50_2_2_uniq.Rds")
genera_100 <- readRDS("data/final/master_100_2_2_uniq.Rds")
genera_200 <- readRDS("data/final/master_200_2_2_uniq.Rds")
species_50 <- readRDS("data/final/master_50_uniq_species.Rds")
species_100 <- readRDS("data/final/master_100_uniq_species.Rds")
species_200 <- readRDS("data/final/master_200_uniq_species.Rds")
home <- getwd()

genera_50_count <- read.csv(("data/sensitivity_testing/genera_50_radius_nSite_sensitivity_testing.csv"), header = T,row.names = 1)
genera_100_count <- read.csv(("data/sensitivity_testing/genera_100_radius_nSite_sensitivity_testing.csv"), header = T,row.names = 1)
genera_200_count <- read.csv(("data/sensitivity_testing/genera_200_radius_nSite_sensitivity_testing.csv"), header = T,row.names = 1)

species_50_count <- read.csv(("data/sensitivity_testing/species_50_radius_nSite_sensitivity_testing.csv"), header = T,row.names = 1)
species_100_count <- read.csv(("data/sensitivity_testing/species_100_radius_nSite_sensitivity_testing.csv"), header = T,row.names = 1)
species_200_count <- read.csv(("data/sensitivity_testing/species_200_radius_nSite_sensitivity_testing.csv"), header = T,row.names = 1)


#### Traybake sensitivity testing ####
##### Genera - 50km #####
## Get stage numbers
stages <- sort(unique(genera_50$stage))

## Add sites
sites2 <- cbind("stages"=stages, "grid_cells"=rep(2, length(stages)))
sites3 <- cbind("stages"=stages, "grid_cells"=rep(3, length(stages)))
sites4 <- cbind("stages"=stages, "grid_cells"=rep(4, length(stages)))
sites <- rbind(sites2, sites3, sites4)

## Add radii
options(scipen = 9999)
radius100 <- cbind(sites, "radius"=rep(100000,nrow(sites)))
radius200 <- cbind(sites, "radius"=rep(200000,nrow(sites)))
radius <- rbind(radius100, radius200)

## Add containers
genera_50_count <- cbind(radius, "nGridCellsInTimeBin"=rep(NA,nrow(radius)))
genera_50_count <- cbind(genera_50_count, "nGridCellsIncludedInSubsamples"=rep(NA,nrow(genera_50_count)))
genera_50_count <- cbind(genera_50_count, "pGridCellsIncludedInSubsamples"=rep(NA,nrow(genera_50_count)))
genera_50_count <- cbind(genera_50_count, "nOccurrencesInTimeBin"=rep(NA,nrow(genera_50_count)))
genera_50_count <- cbind(genera_50_count, "nOccurrencesIncludedInSubsamples"=rep(NA,nrow(genera_50_count)))
genera_50_count <- cbind(genera_50_count, "pOccurrencesIncludedInSubsamples"=rep(NA,nrow(genera_50_count)))
genera_50_count <- cbind(genera_50_count, "nClusters"=rep(NA,nrow(genera_50_count)))
genera_50_count <- cbind(genera_50_count, "nRCRs"=rep(NA,nrow(genera_50_count)))
genera_50_count <- cbind(genera_50_count, "maxNumberOfRCRsInCluster"=rep(NA,nrow(genera_50_count)))
genera_50_count <- data.frame(genera_50_count)

## Format when testing
## Populated up until t=40
start = 1
for(t in start:nrow(genera_50_count)){
  print(t)
  ## Get time bin
  samp <- genera_50[which(genera_50$stage == genera_50_count[t,"stages"]),]
  ## genera_50_count number of grid cells and occurrences in time
  genera_50_count[t,"nGridCellsInTimeBin"] <- length(unique(samp[,"cell_50km"]))
  genera_50_count[t,"nOccurrencesInTimeBin"] <- nrow(samp)
  ## If nrow 0, skip
  if(nrow(samp)>0){
    ## If not, get RCRs in Clusters
    raw <- traybake(samp, xy = c("cellx_50km", "celly_50km"), uniqID = "cell_50km", r = genera_50_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = genera_50_count[t,"grid_cells"], nCookie = 100, nBatch = 100, n.cores = 8)
    seeds <- traybake(samp, xy = c("cellx_50km", "celly_50km"), uniqID = "cell_50km", r = genera_50_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = genera_50_count[t,"grid_cells"], nCookie = 100, nBatch = 100, output = "seeds", n.cores = 8)
    if(!is.null(raw) && !is.null(seeds)){
    ## count clusters
    genera_50_count[t,"nClusters"] <- length(seeds)
    ## count RCRs
    genera_50_count[t,"nRCRs"] <- length(unique(unlist(lapply(1:length(raw), function(x) names(raw[[x]])))))
    ## Get maximum RCRs in clusters
    genera_50_count[t,"maxNumberOfRCRsInCluster"] <- max(sapply(1:length(seeds), function(x) nrow(seeds[[x]])))
    ## Create new version of samp
    stanSamp <- lapply(1:length(raw), function(x){
      out <- do.call(rbind, raw[[x]])
    })
    stanSamp <- do.call(rbind,stanSamp)
    stanSamp <- stanSamp[!duplicated(stanSamp),]
    ## Get grid cells
    genera_50_count[t,"nGridCellsIncludedInSubsamples"] <- length(unique(stanSamp[,"cell_50km"]))
    genera_50_count[t,"pGridCellsIncludedInSubsamples"] <- round(genera_50_count[t,"nGridCellsIncludedInSubsamples"]/genera_50_count[t,"nGridCellsInTimeBin"], digits = 3)
    ## Get occurrences
    genera_50_count[t,"nOccurrencesIncludedInSubsamples"] <- nrow(stanSamp)
    genera_50_count[t,"pOccurrencesIncludedInSubsamples"] <- round(genera_50_count[t,"nOccurrencesIncludedInSubsamples"]/genera_50_count[t,"nOccurrencesInTimeBin"], digits = 3)
    }
  } else {
    next
  }
}

## Update NAs to 0s and re-calculate
for(t in 1:nrow(genera_50_count)){
  if(is.na(genera_50_count[t,"nClusters"])){
    genera_50_count[t,"nClusters"] <- 0
  }
  if(is.na(genera_50_count[t,"nRCRs"])){
    genera_50_count[t,"nRCRs"] <- 0
  }
  if(is.na(genera_50_count[t,"maxNumberOfRCRsInCluster"])){
    genera_50_count[t,"maxNumberOfRCRsInCluster"] <- 0
  }
  if(is.na(genera_50_count[t,"nGridCellsIncludedInSubsamples"])){
    genera_50_count[t,"nGridCellsIncludedInSubsamples"] <- 0
    genera_50_count[t,"pGridCellsIncludedInSubsamples"] <- round(genera_50_count[t,"nGridCellsIncludedInSubsamples"]/genera_50_count[t,"nGridCellsInTimeBin"], digits = 3)
  }
  if(is.na(genera_50_count[t,"nOccurrencesIncludedInSubsamples"])){
    genera_50_count[t,"nOccurrencesIncludedInSubsamples"] <- 0
    genera_50_count[t,"pOccurrencesIncludedInSubsamples"] <- round(genera_50_count[t,"nOccurrencesIncludedInSubsamples"]/genera_50_count[t,"nOccurrencesInTimeBin"], digits = 3)
  }
}

## Save once done
write.csv(genera_50_count, file="data/sensitivity_testing/genera_50_radius_nSite_sensitivity_testing.csv")

## Then summarise for each method. Number of stage_grid_cells retained, number of occurrences.
genera_50_summary <- genera_50_count[!duplicated(genera_50_count[,c(2,3)]),c(2,3)]
genera_50_summary$totalGridCells <- sum(genera_50_count[intersect(which(genera_50_count[,"radius"]==50000),which(genera_50_count[,"grid_cells"]==2)),"nGridCellsInTimeBin"])
genera_50_summary$totalOccurrences <- sum(genera_50_count[intersect(which(genera_50_count[,"radius"]==50000),which(genera_50_count[,"grid_cells"]==2)),"nOccurrencesInTimeBin"])
genera_50_summary$gridCellsSampled <- NA
genera_50_summary$occurrencesSampled <- NA
genera_50_summary$proportionOfGridCells <- NA
genera_50_summary$proportionOfOccurrences <- NA
genera_50_summary$maxRCRsInCluster <- NA
genera_50_summary$nClusters <- NA

for(r in 1:nrow(genera_50_summary)){
  genera_50_summary[r,"gridCellsSampled"] <- sum(genera_50_count[intersect(which(genera_50_count[,"radius"]==genera_50_summary[r,"radius"]),which(genera_50_count[,"grid_cells"]==genera_50_summary[r,"grid_cells"])),"nGridCellsIncludedInSubsamples"])
  genera_50_summary[r,"occurrencesSampled"] <- sum(genera_50_count[intersect(which(genera_50_count[,"radius"]==genera_50_summary[r,"radius"]),which(genera_50_count[,"grid_cells"]==genera_50_summary[r,"grid_cells"])),"nOccurrencesIncludedInSubsamples"])
  genera_50_summary[r,"proportionOfGridCells"] <- round(genera_50_summary[r,"gridCellsSampled"]/genera_50_summary[r,"totalGridCells"], digits = 3)
  genera_50_summary[r,"proportionOfOccurrences"] <- round(genera_50_summary[r,"occurrencesSampled"]/genera_50_summary[r,"totalOccurrences"], digits = 3)
  genera_50_summary[r,"maxRCRsInCluster"] <- max(genera_50_count[intersect(which(genera_50_count[,"radius"]==genera_50_summary[r,"radius"]),which(genera_50_count[,"grid_cells"]==genera_50_summary[r,"grid_cells"])),"maxNumberOfRCRsInCluster"])
  genera_50_summary[r,"nClusters"] <- sum(genera_50_count[intersect(which(genera_50_count[,"radius"]==genera_50_summary[r,"radius"]),which(genera_50_count[,"grid_cells"]==genera_50_summary[r,"grid_cells"])),"nClusters"])
}

## Export genera_50_summary for reference
## Save once done
write.csv(genera_50_summary, file="data/sensitivity_testing/genera_50_sensitivity_testing_summary.csv")

##### Genera - 100km #####
## Get stage numbers
stages <- sort(unique(genera_100$stage))

## Add sites
sites2 <- cbind("stages"=stages, "grid_cells"=rep(2, length(stages)))
sites3 <- cbind("stages"=stages, "grid_cells"=rep(3, length(stages)))
sites4 <- cbind("stages"=stages, "grid_cells"=rep(4, length(stages)))
sites <- rbind(sites2, sites3, sites4)

## Add radii
options(scipen = 9999)
radius200 <- cbind(sites, "radius"=rep(200000,nrow(sites)))
radius400 <- cbind(sites, "radius"=rep(400000,nrow(sites)))
radius <- rbind(radius200, radius400)

## Add containers
genera_100_count <- cbind(radius, "nGridCellsInTimeBin"=rep(NA,nrow(radius)))
genera_100_count <- cbind(genera_100_count, "nGridCellsIncludedInSubsamples"=rep(NA,nrow(genera_100_count)))
genera_100_count <- cbind(genera_100_count, "pGridCellsIncludedInSubsamples"=rep(NA,nrow(genera_100_count)))
genera_100_count <- cbind(genera_100_count, "nOccurrencesInTimeBin"=rep(NA,nrow(genera_100_count)))
genera_100_count <- cbind(genera_100_count, "nOccurrencesIncludedInSubsamples"=rep(NA,nrow(genera_100_count)))
genera_100_count <- cbind(genera_100_count, "pOccurrencesIncludedInSubsamples"=rep(NA,nrow(genera_100_count)))
genera_100_count <- cbind(genera_100_count, "nClusters"=rep(NA,nrow(genera_100_count)))
genera_100_count <- cbind(genera_100_count, "nRCRs"=rep(NA,nrow(genera_100_count)))
genera_100_count <- cbind(genera_100_count, "maxNumberOfRCRsInCluster"=rep(NA,nrow(genera_100_count)))
genera_100_count <- data.frame(genera_100_count)

## Format when testing
## Populated up until t=40
start = 1
for(t in start:nrow(genera_100_count)){
  print(t)
  ## Get time bin
  samp <- genera_100[which(genera_100$stage == genera_100_count[t,"stages"]),]
  ## genera_100_count number of grid cells and occurrences in time
  genera_100_count[t,"nGridCellsInTimeBin"] <- length(unique(samp[,"cell_100km"]))
  genera_100_count[t,"nOccurrencesInTimeBin"] <- nrow(samp)
  ## If nrow 0, skip
  if(nrow(samp)>0){
    ## If not, get RCRs in Clusters
    raw <- traybake(samp, xy = c("cellx_100km", "celly_100km"), uniqID = "cell_100km", r = genera_100_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = genera_100_count[t,"grid_cells"], nCookie = 100, nBatch = 100, n.cores = 8)
    seeds <- traybake(samp, xy = c("cellx_100km", "celly_100km"), uniqID = "cell_100km", r = genera_100_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = genera_100_count[t,"grid_cells"], nCookie = 100, nBatch = 100, output = "seeds", n.cores = 8)
    if(!is.null(raw) && !is.null(seeds)){
      ## count clusters
      genera_100_count[t,"nClusters"] <- length(seeds)
      ## count RCRs
      genera_100_count[t,"nRCRs"] <- length(unique(unlist(lapply(1:length(raw), function(x) names(raw[[x]])))))
      ## Get maximum RCRs in clusters
      genera_100_count[t,"maxNumberOfRCRsInCluster"] <- max(sapply(1:length(seeds), function(x) nrow(seeds[[x]])))
      ## Create new version of samp
      stanSamp <- lapply(1:length(raw), function(x){
        out <- do.call(rbind, raw[[x]])
      })
      stanSamp <- do.call(rbind,stanSamp)
      stanSamp <- stanSamp[!duplicated(stanSamp),]
      ## Get grid cells
      genera_100_count[t,"nGridCellsIncludedInSubsamples"] <- length(unique(stanSamp[,"cell_100km"]))
      genera_100_count[t,"pGridCellsIncludedInSubsamples"] <- round(genera_100_count[t,"nGridCellsIncludedInSubsamples"]/genera_100_count[t,"nGridCellsInTimeBin"], digits = 3)
      ## Get occurrences
      genera_100_count[t,"nOccurrencesIncludedInSubsamples"] <- nrow(stanSamp)
      genera_100_count[t,"pOccurrencesIncludedInSubsamples"] <- round(genera_100_count[t,"nOccurrencesIncludedInSubsamples"]/genera_100_count[t,"nOccurrencesInTimeBin"], digits = 3)
    }
  } else {
    next
  }
}

## Update NAs to 0s and re-calculate
for(t in 1:nrow(genera_100_count)){
  if(is.na(genera_100_count[t,"nClusters"])){
    genera_100_count[t,"nClusters"] <- 0
  }
  if(is.na(genera_100_count[t,"nRCRs"])){
    genera_100_count[t,"nRCRs"] <- 0
  }
  if(is.na(genera_100_count[t,"maxNumberOfRCRsInCluster"])){
    genera_100_count[t,"maxNumberOfRCRsInCluster"] <- 0
  }
  if(is.na(genera_100_count[t,"nGridCellsIncludedInSubsamples"])){
    genera_100_count[t,"nGridCellsIncludedInSubsamples"] <- 0
    genera_100_count[t,"pGridCellsIncludedInSubsamples"] <- round(genera_100_count[t,"nGridCellsIncludedInSubsamples"]/genera_100_count[t,"nGridCellsInTimeBin"], digits = 3)
  }
  if(is.na(genera_100_count[t,"nOccurrencesIncludedInSubsamples"])){
    genera_100_count[t,"nOccurrencesIncludedInSubsamples"] <- 0
    genera_100_count[t,"pOccurrencesIncludedInSubsamples"] <- round(genera_100_count[t,"nOccurrencesIncludedInSubsamples"]/genera_100_count[t,"nOccurrencesInTimeBin"], digits = 3)
  }
}

## Save once done
write.csv(genera_100_count, file="data/sensitivity_testing/genera_100_radius_nSite_sensitivity_testing.csv")

## Then summarise for each method. Number of stage_grid_cells retained, number of occurrences.
genera_100_summary <- genera_100_count[!duplicated(genera_100_count[,c(2,3)]),c(2,3)]
genera_100_summary$totalGridCells <- sum(genera_100_count[intersect(which(genera_100_count[,"radius"]==200000),which(genera_100_count[,"grid_cells"]==2)),"nGridCellsInTimeBin"])
genera_100_summary$totalOccurrences <- sum(genera_100_count[intersect(which(genera_100_count[,"radius"]==200000),which(genera_100_count[,"grid_cells"]==2)),"nOccurrencesInTimeBin"])
genera_100_summary$gridCellsSampled <- NA
genera_100_summary$occurrencesSampled <- NA
genera_100_summary$proportionOfGridCells <- NA
genera_100_summary$proportionOfOccurrences <- NA
genera_100_summary$maxRCRsInCluster <- NA
genera_100_summary$nClusters <- NA

for(r in 1:nrow(genera_100_summary)){
  genera_100_summary[r,"gridCellsSampled"] <- sum(genera_100_count[intersect(which(genera_100_count[,"radius"]==genera_100_summary[r,"radius"]),which(genera_100_count[,"grid_cells"]==genera_100_summary[r,"grid_cells"])),"nGridCellsIncludedInSubsamples"])
  genera_100_summary[r,"occurrencesSampled"] <- sum(genera_100_count[intersect(which(genera_100_count[,"radius"]==genera_100_summary[r,"radius"]),which(genera_100_count[,"grid_cells"]==genera_100_summary[r,"grid_cells"])),"nOccurrencesIncludedInSubsamples"])
  genera_100_summary[r,"proportionOfGridCells"] <- round(genera_100_summary[r,"gridCellsSampled"]/genera_100_summary[r,"totalGridCells"], digits = 3)
  genera_100_summary[r,"proportionOfOccurrences"] <- round(genera_100_summary[r,"occurrencesSampled"]/genera_100_summary[r,"totalOccurrences"], digits = 3)
  genera_100_summary[r,"maxRCRsInCluster"] <- max(genera_100_count[intersect(which(genera_100_count[,"radius"]==genera_100_summary[r,"radius"]),which(genera_100_count[,"grid_cells"]==genera_100_summary[r,"grid_cells"])),"maxNumberOfRCRsInCluster"])
  genera_100_summary[r,"nClusters"] <- sum(genera_100_count[intersect(which(genera_100_count[,"radius"]==genera_100_summary[r,"radius"]),which(genera_100_count[,"grid_cells"]==genera_100_summary[r,"grid_cells"])),"nClusters"])
}

## Export genera_100_summary for reference
## Save once done
write.csv(genera_100_summary, file="data/sensitivity_testing/genera_100_sensitivity_testing_summary.csv")

##### Genera - 200km #####
## Get stage numbers
stages <- sort(unique(genera_200$stage))

## Add sites
sites2 <- cbind("stages"=stages, "grid_cells"=rep(2, length(stages)))
sites3 <- cbind("stages"=stages, "grid_cells"=rep(3, length(stages)))
sites4 <- cbind("stages"=stages, "grid_cells"=rep(4, length(stages)))
sites <- rbind(sites2, sites3, sites4)

## Add radii
options(scipen = 9999)
radius400 <- cbind(sites, "radius"=rep(400000,nrow(sites)))
radius800 <- cbind(sites, "radius"=rep(800000,nrow(sites)))
radius <- rbind(radius400, radius800)

## Add containers
genera_200_count <- cbind(radius, "nGridCellsInTimeBin"=rep(NA,nrow(radius)))
genera_200_count <- cbind(genera_200_count, "nGridCellsIncludedInSubsamples"=rep(NA,nrow(genera_200_count)))
genera_200_count <- cbind(genera_200_count, "pGridCellsIncludedInSubsamples"=rep(NA,nrow(genera_200_count)))
genera_200_count <- cbind(genera_200_count, "nOccurrencesInTimeBin"=rep(NA,nrow(genera_200_count)))
genera_200_count <- cbind(genera_200_count, "nOccurrencesIncludedInSubsamples"=rep(NA,nrow(genera_200_count)))
genera_200_count <- cbind(genera_200_count, "pOccurrencesIncludedInSubsamples"=rep(NA,nrow(genera_200_count)))
genera_200_count <- cbind(genera_200_count, "nClusters"=rep(NA,nrow(genera_200_count)))
genera_200_count <- cbind(genera_200_count, "nRCRs"=rep(NA,nrow(genera_200_count)))
genera_200_count <- cbind(genera_200_count, "maxNumberOfRCRsInCluster"=rep(NA,nrow(genera_200_count)))
genera_200_count <- data.frame(genera_200_count)

## Format when testing
## Populated up until t=40
start = 1
for(t in start:nrow(genera_200_count)){
  print(t)
  ## Get time bin
  samp <- genera_200[which(genera_200$stage == genera_200_count[t,"stages"]),]
  ## genera_200_count number of grid cells and occurrences in time
  genera_200_count[t,"nGridCellsInTimeBin"] <- length(unique(samp[,"cell_200km"]))
  genera_200_count[t,"nOccurrencesInTimeBin"] <- nrow(samp)
  ## If nrow 0, skip
  if(nrow(samp)>0){
    ## If not, get RCRs in Clusters
    raw <- traybake(samp, xy = c("cellx_200km", "celly_200km"), uniqID = "cell_200km", r = genera_200_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = genera_200_count[t,"grid_cells"], nCookie = 200, nBatch = 200, n.cores = 8)
    seeds <- traybake(samp, xy = c("cellx_200km", "celly_200km"), uniqID = "cell_200km", r = genera_200_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = genera_200_count[t,"grid_cells"], nCookie = 200, nBatch = 200, output = "seeds", n.cores = 8)
    if(!is.null(raw) && !is.null(seeds)){
      ## count clusters
      genera_200_count[t,"nClusters"] <- length(seeds)
      ## count RCRs
      genera_200_count[t,"nRCRs"] <- length(unique(unlist(lapply(1:length(raw), function(x) names(raw[[x]])))))
      ## Get maximum RCRs in clusters
      genera_200_count[t,"maxNumberOfRCRsInCluster"] <- max(sapply(1:length(seeds), function(x) nrow(seeds[[x]])))
      ## Create new version of samp
      stanSamp <- lapply(1:length(raw), function(x){
        out <- do.call(rbind, raw[[x]])
      })
      stanSamp <- do.call(rbind,stanSamp)
      stanSamp <- stanSamp[!duplicated(stanSamp),]
      ## Get grid cells
      genera_200_count[t,"nGridCellsIncludedInSubsamples"] <- length(unique(stanSamp[,"cell_200km"]))
      genera_200_count[t,"pGridCellsIncludedInSubsamples"] <- round(genera_200_count[t,"nGridCellsIncludedInSubsamples"]/genera_200_count[t,"nGridCellsInTimeBin"], digits = 3)
      ## Get occurrences
      genera_200_count[t,"nOccurrencesIncludedInSubsamples"] <- nrow(stanSamp)
      genera_200_count[t,"pOccurrencesIncludedInSubsamples"] <- round(genera_200_count[t,"nOccurrencesIncludedInSubsamples"]/genera_200_count[t,"nOccurrencesInTimeBin"], digits = 3)
    }
  } else {
    next
  }
}

## Update NAs to 0s and re-calculate
for(t in 1:nrow(genera_200_count)){
  if(is.na(genera_200_count[t,"nClusters"])){
    genera_200_count[t,"nClusters"] <- 0
  }
  if(is.na(genera_200_count[t,"nRCRs"])){
    genera_200_count[t,"nRCRs"] <- 0
  }
  if(is.na(genera_200_count[t,"maxNumberOfRCRsInCluster"])){
    genera_200_count[t,"maxNumberOfRCRsInCluster"] <- 0
  }
  if(is.na(genera_200_count[t,"nGridCellsIncludedInSubsamples"])){
    genera_200_count[t,"nGridCellsIncludedInSubsamples"] <- 0
    genera_200_count[t,"pGridCellsIncludedInSubsamples"] <- round(genera_200_count[t,"nGridCellsIncludedInSubsamples"]/genera_200_count[t,"nGridCellsInTimeBin"], digits = 3)
  }
  if(is.na(genera_200_count[t,"nOccurrencesIncludedInSubsamples"])){
    genera_200_count[t,"nOccurrencesIncludedInSubsamples"] <- 0
    genera_200_count[t,"pOccurrencesIncludedInSubsamples"] <- round(genera_200_count[t,"nOccurrencesIncludedInSubsamples"]/genera_200_count[t,"nOccurrencesInTimeBin"], digits = 3)
  }
}

## Save once done
write.csv(genera_200_count, file="data/sensitivity_testing/genera_200_radius_nSite_sensitivity_testing.csv")

## Then summarise for each method. Number of stage_grid_cells retained, number of occurrences.
genera_200_summary <- genera_200_count[!duplicated(genera_200_count[,c(2,3)]),c(2,3)]
genera_200_summary$totalGridCells <- sum(genera_200_count[intersect(which(genera_200_count[,"radius"]==400000),which(genera_200_count[,"grid_cells"]==2)),"nGridCellsInTimeBin"])
genera_200_summary$totalOccurrences <- sum(genera_200_count[intersect(which(genera_200_count[,"radius"]==400000),which(genera_200_count[,"grid_cells"]==2)),"nOccurrencesInTimeBin"])
genera_200_summary$gridCellsSampled <- NA
genera_200_summary$occurrencesSampled <- NA
genera_200_summary$proportionOfGridCells <- NA
genera_200_summary$proportionOfOccurrences <- NA
genera_200_summary$maxRCRsInCluster <- NA
genera_200_summary$nClusters <- NA

for(r in 1:nrow(genera_200_summary)){
  genera_200_summary[r,"gridCellsSampled"] <- sum(genera_200_count[intersect(which(genera_200_count[,"radius"]==genera_200_summary[r,"radius"]),which(genera_200_count[,"grid_cells"]==genera_200_summary[r,"grid_cells"])),"nGridCellsIncludedInSubsamples"])
  genera_200_summary[r,"occurrencesSampled"] <- sum(genera_200_count[intersect(which(genera_200_count[,"radius"]==genera_200_summary[r,"radius"]),which(genera_200_count[,"grid_cells"]==genera_200_summary[r,"grid_cells"])),"nOccurrencesIncludedInSubsamples"])
  genera_200_summary[r,"proportionOfGridCells"] <- round(genera_200_summary[r,"gridCellsSampled"]/genera_200_summary[r,"totalGridCells"], digits = 3)
  genera_200_summary[r,"proportionOfOccurrences"] <- round(genera_200_summary[r,"occurrencesSampled"]/genera_200_summary[r,"totalOccurrences"], digits = 3)
  genera_200_summary[r,"maxRCRsInCluster"] <- max(genera_200_count[intersect(which(genera_200_count[,"radius"]==genera_200_summary[r,"radius"]),which(genera_200_count[,"grid_cells"]==genera_200_summary[r,"grid_cells"])),"maxNumberOfRCRsInCluster"])
  genera_200_summary[r,"nClusters"] <- sum(genera_200_count[intersect(which(genera_200_count[,"radius"]==genera_200_summary[r,"radius"]),which(genera_200_count[,"grid_cells"]==genera_200_summary[r,"grid_cells"])),"nClusters"])
}

## Export genera_200_summary for reference
## Save once done
write.csv(genera_200_summary, file="data/sensitivity_testing/genera_200_sensitivity_testing_summary.csv")

##### species - 50km #####
## Get stage numbers
stages <- sort(unique(species_50$stage))

## Add sites
sites2 <- cbind("stages"=stages, "grid_cells"=rep(2, length(stages)))
sites3 <- cbind("stages"=stages, "grid_cells"=rep(3, length(stages)))
sites4 <- cbind("stages"=stages, "grid_cells"=rep(4, length(stages)))
sites <- rbind(sites2, sites3, sites4)

## Add radii
options(scipen = 9999)
radius100 <- cbind(sites, "radius"=rep(100000,nrow(sites)))
radius200 <- cbind(sites, "radius"=rep(200000,nrow(sites)))
radius <- rbind(radius100, radius200)

## Add containers
species_50_count <- cbind(radius, "nGridCellsInTimeBin"=rep(NA,nrow(radius)))
species_50_count <- cbind(species_50_count, "nGridCellsIncludedInSubsamples"=rep(NA,nrow(species_50_count)))
species_50_count <- cbind(species_50_count, "pGridCellsIncludedInSubsamples"=rep(NA,nrow(species_50_count)))
species_50_count <- cbind(species_50_count, "nOccurrencesInTimeBin"=rep(NA,nrow(species_50_count)))
species_50_count <- cbind(species_50_count, "nOccurrencesIncludedInSubsamples"=rep(NA,nrow(species_50_count)))
species_50_count <- cbind(species_50_count, "pOccurrencesIncludedInSubsamples"=rep(NA,nrow(species_50_count)))
species_50_count <- cbind(species_50_count, "nClusters"=rep(NA,nrow(species_50_count)))
species_50_count <- cbind(species_50_count, "nRCRs"=rep(NA,nrow(species_50_count)))
species_50_count <- cbind(species_50_count, "maxNumberOfRCRsInCluster"=rep(NA,nrow(species_50_count)))
species_50_count <- data.frame(species_50_count)

## Format when testing
## Populated up until t=40
start = 1
for(t in start:nrow(species_50_count)){
  print(t)
  ## Get time bin
  samp <- species_50[which(species_50$stage == species_50_count[t,"stages"]),]
  ## species_50_count number of grid cells and occurrences in time
  species_50_count[t,"nGridCellsInTimeBin"] <- length(unique(samp[,"cell_50km"]))
  species_50_count[t,"nOccurrencesInTimeBin"] <- nrow(samp)
  ## If nrow 0, skip
  if(nrow(samp)>0){
    ## If not, get RCRs in Clusters
    raw <- traybake(samp, xy = c("cellx_50km", "celly_50km"), uniqID = "cell_50km", r = species_50_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = species_50_count[t,"grid_cells"], nCookie = 100, nBatch = 100, n.cores = 8)
    seeds <- traybake(samp, xy = c("cellx_50km", "celly_50km"), uniqID = "cell_50km", r = species_50_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = species_50_count[t,"grid_cells"], nCookie = 100, nBatch = 100, output = "seeds", n.cores = 8)
    if(!is.null(raw) && !is.null(seeds)){
      ## count clusters
      species_50_count[t,"nClusters"] <- length(seeds)
      ## count RCRs
      species_50_count[t,"nRCRs"] <- length(unique(unlist(lapply(1:length(raw), function(x) names(raw[[x]])))))
      ## Get maximum RCRs in clusters
      species_50_count[t,"maxNumberOfRCRsInCluster"] <- max(sapply(1:length(seeds), function(x) nrow(seeds[[x]])))
      ## Create new version of samp
      stanSamp <- lapply(1:length(raw), function(x){
        out <- do.call(rbind, raw[[x]])
      })
      stanSamp <- do.call(rbind,stanSamp)
      stanSamp <- stanSamp[!duplicated(stanSamp),]
      ## Get grid cells
      species_50_count[t,"nGridCellsIncludedInSubsamples"] <- length(unique(stanSamp[,"cell_50km"]))
      species_50_count[t,"pGridCellsIncludedInSubsamples"] <- round(species_50_count[t,"nGridCellsIncludedInSubsamples"]/species_50_count[t,"nGridCellsInTimeBin"], digits = 3)
      ## Get occurrences
      species_50_count[t,"nOccurrencesIncludedInSubsamples"] <- nrow(stanSamp)
      species_50_count[t,"pOccurrencesIncludedInSubsamples"] <- round(species_50_count[t,"nOccurrencesIncludedInSubsamples"]/species_50_count[t,"nOccurrencesInTimeBin"], digits = 3)
    }
  } else {
    next
  }
}

## Update NAs to 0s and re-calculate
for(t in 1:nrow(species_50_count)){
  if(is.na(species_50_count[t,"nClusters"])){
    species_50_count[t,"nClusters"] <- 0
  }
  if(is.na(species_50_count[t,"nRCRs"])){
    species_50_count[t,"nRCRs"] <- 0
  }
  if(is.na(species_50_count[t,"maxNumberOfRCRsInCluster"])){
    species_50_count[t,"maxNumberOfRCRsInCluster"] <- 0
  }
  if(is.na(species_50_count[t,"nGridCellsIncludedInSubsamples"])){
    species_50_count[t,"nGridCellsIncludedInSubsamples"] <- 0
    species_50_count[t,"pGridCellsIncludedInSubsamples"] <- round(species_50_count[t,"nGridCellsIncludedInSubsamples"]/species_50_count[t,"nGridCellsInTimeBin"], digits = 3)
  }
  if(is.na(species_50_count[t,"nOccurrencesIncludedInSubsamples"])){
    species_50_count[t,"nOccurrencesIncludedInSubsamples"] <- 0
    species_50_count[t,"pOccurrencesIncludedInSubsamples"] <- round(species_50_count[t,"nOccurrencesIncludedInSubsamples"]/species_50_count[t,"nOccurrencesInTimeBin"], digits = 3)
  }
}

## Save once done
write.csv(species_50_count, file="data/sensitivity_testing/species_50_radius_nSite_sensitivity_testing.csv")

## Then summarise for each method. Number of stage_grid_cells retained, number of occurrences.
species_50_summary <- species_50_count[!duplicated(species_50_count[,c(2,3)]),c(2,3)]
species_50_summary$totalGridCells <- sum(species_50_count[intersect(which(species_50_count[,"radius"]==50000),which(species_50_count[,"grid_cells"]==2)),"nGridCellsInTimeBin"])
species_50_summary$totalOccurrences <- sum(species_50_count[intersect(which(species_50_count[,"radius"]==50000),which(species_50_count[,"grid_cells"]==2)),"nOccurrencesInTimeBin"])
species_50_summary$gridCellsSampled <- NA
species_50_summary$occurrencesSampled <- NA
species_50_summary$proportionOfGridCells <- NA
species_50_summary$proportionOfOccurrences <- NA
species_50_summary$maxRCRsInCluster <- NA
species_50_summary$nClusters <- NA

for(r in 1:nrow(species_50_summary)){
  species_50_summary[r,"gridCellsSampled"] <- sum(species_50_count[intersect(which(species_50_count[,"radius"]==species_50_summary[r,"radius"]),which(species_50_count[,"grid_cells"]==species_50_summary[r,"grid_cells"])),"nGridCellsIncludedInSubsamples"])
  species_50_summary[r,"occurrencesSampled"] <- sum(species_50_count[intersect(which(species_50_count[,"radius"]==species_50_summary[r,"radius"]),which(species_50_count[,"grid_cells"]==species_50_summary[r,"grid_cells"])),"nOccurrencesIncludedInSubsamples"])
  species_50_summary[r,"proportionOfGridCells"] <- round(species_50_summary[r,"gridCellsSampled"]/species_50_summary[r,"totalGridCells"], digits = 3)
  species_50_summary[r,"proportionOfOccurrences"] <- round(species_50_summary[r,"occurrencesSampled"]/species_50_summary[r,"totalOccurrences"], digits = 3)
  species_50_summary[r,"maxRCRsInCluster"] <- max(species_50_count[intersect(which(species_50_count[,"radius"]==species_50_summary[r,"radius"]),which(species_50_count[,"grid_cells"]==species_50_summary[r,"grid_cells"])),"maxNumberOfRCRsInCluster"])
  species_50_summary[r,"nClusters"] <- sum(species_50_count[intersect(which(species_50_count[,"radius"]==species_50_summary[r,"radius"]),which(species_50_count[,"grid_cells"]==species_50_summary[r,"grid_cells"])),"nClusters"])
}

## Export species_50_summary for reference
## Save once done
write.csv(species_50_summary, file="data/sensitivity_testing/species_50_sensitivity_testing_summary.csv")

##### species - 100km #####
## Get stage numbers
stages <- sort(unique(species_100$stage))

## Add sites
sites2 <- cbind("stages"=stages, "grid_cells"=rep(2, length(stages)))
sites3 <- cbind("stages"=stages, "grid_cells"=rep(3, length(stages)))
sites4 <- cbind("stages"=stages, "grid_cells"=rep(4, length(stages)))
sites <- rbind(sites2, sites3, sites4)

## Add radii
options(scipen = 9999)
radius200 <- cbind(sites, "radius"=rep(200000,nrow(sites)))
radius400 <- cbind(sites, "radius"=rep(400000,nrow(sites)))
radius <- rbind(radius200, radius400)

## Add containers
species_100_count <- cbind(radius, "nGridCellsInTimeBin"=rep(NA,nrow(radius)))
species_100_count <- cbind(species_100_count, "nGridCellsIncludedInSubsamples"=rep(NA,nrow(species_100_count)))
species_100_count <- cbind(species_100_count, "pGridCellsIncludedInSubsamples"=rep(NA,nrow(species_100_count)))
species_100_count <- cbind(species_100_count, "nOccurrencesInTimeBin"=rep(NA,nrow(species_100_count)))
species_100_count <- cbind(species_100_count, "nOccurrencesIncludedInSubsamples"=rep(NA,nrow(species_100_count)))
species_100_count <- cbind(species_100_count, "pOccurrencesIncludedInSubsamples"=rep(NA,nrow(species_100_count)))
species_100_count <- cbind(species_100_count, "nClusters"=rep(NA,nrow(species_100_count)))
species_100_count <- cbind(species_100_count, "nRCRs"=rep(NA,nrow(species_100_count)))
species_100_count <- cbind(species_100_count, "maxNumberOfRCRsInCluster"=rep(NA,nrow(species_100_count)))
species_100_count <- data.frame(species_100_count)

## Format when testing
## Populated up until t=40
start = 1
for(t in start:nrow(species_100_count)){
  print(t)
  ## Get time bin
  samp <- species_100[which(species_100$stage == species_100_count[t,"stages"]),]
  ## species_100_count number of grid cells and occurrences in time
  species_100_count[t,"nGridCellsInTimeBin"] <- length(unique(samp[,"cell_100km"]))
  species_100_count[t,"nOccurrencesInTimeBin"] <- nrow(samp)
  ## If nrow 0, skip
  if(nrow(samp)>0){
    ## If not, get RCRs in Clusters
    raw <- traybake(samp, xy = c("cellx_100km", "celly_100km"), uniqID = "cell_100km", r = species_100_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = species_100_count[t,"grid_cells"], nCookie = 100, nBatch = 100, n.cores = 8)
    seeds <- traybake(samp, xy = c("cellx_100km", "celly_100km"), uniqID = "cell_100km", r = species_100_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = species_100_count[t,"grid_cells"], nCookie = 100, nBatch = 100, output = "seeds", n.cores = 8)
    if(!is.null(raw) && !is.null(seeds)){
      ## count clusters
      species_100_count[t,"nClusters"] <- length(seeds)
      ## count RCRs
      species_100_count[t,"nRCRs"] <- length(unique(unlist(lapply(1:length(raw), function(x) names(raw[[x]])))))
      ## Get maximum RCRs in clusters
      species_100_count[t,"maxNumberOfRCRsInCluster"] <- max(sapply(1:length(seeds), function(x) nrow(seeds[[x]])))
      ## Create new version of samp
      stanSamp <- lapply(1:length(raw), function(x){
        out <- do.call(rbind, raw[[x]])
      })
      stanSamp <- do.call(rbind,stanSamp)
      stanSamp <- stanSamp[!duplicated(stanSamp),]
      ## Get grid cells
      species_100_count[t,"nGridCellsIncludedInSubsamples"] <- length(unique(stanSamp[,"cell_100km"]))
      species_100_count[t,"pGridCellsIncludedInSubsamples"] <- round(species_100_count[t,"nGridCellsIncludedInSubsamples"]/species_100_count[t,"nGridCellsInTimeBin"], digits = 3)
      ## Get occurrences
      species_100_count[t,"nOccurrencesIncludedInSubsamples"] <- nrow(stanSamp)
      species_100_count[t,"pOccurrencesIncludedInSubsamples"] <- round(species_100_count[t,"nOccurrencesIncludedInSubsamples"]/species_100_count[t,"nOccurrencesInTimeBin"], digits = 3)
    }
  } else {
    next
  }
}

## Update NAs to 0s and re-calculate
for(t in 1:nrow(species_100_count)){
  if(is.na(species_100_count[t,"nClusters"])){
    species_100_count[t,"nClusters"] <- 0
  }
  if(is.na(species_100_count[t,"nRCRs"])){
    species_100_count[t,"nRCRs"] <- 0
  }
  if(is.na(species_100_count[t,"maxNumberOfRCRsInCluster"])){
    species_100_count[t,"maxNumberOfRCRsInCluster"] <- 0
  }
  if(is.na(species_100_count[t,"nGridCellsIncludedInSubsamples"])){
    species_100_count[t,"nGridCellsIncludedInSubsamples"] <- 0
    species_100_count[t,"pGridCellsIncludedInSubsamples"] <- round(species_100_count[t,"nGridCellsIncludedInSubsamples"]/species_100_count[t,"nGridCellsInTimeBin"], digits = 3)
  }
  if(is.na(species_100_count[t,"nOccurrencesIncludedInSubsamples"])){
    species_100_count[t,"nOccurrencesIncludedInSubsamples"] <- 0
    species_100_count[t,"pOccurrencesIncludedInSubsamples"] <- round(species_100_count[t,"nOccurrencesIncludedInSubsamples"]/species_100_count[t,"nOccurrencesInTimeBin"], digits = 3)
  }
}

## Save once done
write.csv(species_100_count, file="data/sensitivity_testing/species_100_radius_nSite_sensitivity_testing.csv")

## Then summarise for each method. Number of stage_grid_cells retained, number of occurrences.
species_100_summary <- species_100_count[!duplicated(species_100_count[,c(2,3)]),c(2,3)]
species_100_summary$totalGridCells <- sum(species_100_count[intersect(which(species_100_count[,"radius"]==200000),which(species_100_count[,"grid_cells"]==2)),"nGridCellsInTimeBin"])
species_100_summary$totalOccurrences <- sum(species_100_count[intersect(which(species_100_count[,"radius"]==200000),which(species_100_count[,"grid_cells"]==2)),"nOccurrencesInTimeBin"])
species_100_summary$gridCellsSampled <- NA
species_100_summary$occurrencesSampled <- NA
species_100_summary$proportionOfGridCells <- NA
species_100_summary$proportionOfOccurrences <- NA
species_100_summary$maxRCRsInCluster <- NA
species_100_summary$nClusters <- NA

for(r in 1:nrow(species_100_summary)){
  species_100_summary[r,"gridCellsSampled"] <- sum(species_100_count[intersect(which(species_100_count[,"radius"]==species_100_summary[r,"radius"]),which(species_100_count[,"grid_cells"]==species_100_summary[r,"grid_cells"])),"nGridCellsIncludedInSubsamples"])
  species_100_summary[r,"occurrencesSampled"] <- sum(species_100_count[intersect(which(species_100_count[,"radius"]==species_100_summary[r,"radius"]),which(species_100_count[,"grid_cells"]==species_100_summary[r,"grid_cells"])),"nOccurrencesIncludedInSubsamples"])
  species_100_summary[r,"proportionOfGridCells"] <- round(species_100_summary[r,"gridCellsSampled"]/species_100_summary[r,"totalGridCells"], digits = 3)
  species_100_summary[r,"proportionOfOccurrences"] <- round(species_100_summary[r,"occurrencesSampled"]/species_100_summary[r,"totalOccurrences"], digits = 3)
  species_100_summary[r,"maxRCRsInCluster"] <- max(species_100_count[intersect(which(species_100_count[,"radius"]==species_100_summary[r,"radius"]),which(species_100_count[,"grid_cells"]==species_100_summary[r,"grid_cells"])),"maxNumberOfRCRsInCluster"])
  species_100_summary[r,"nClusters"] <- sum(species_100_count[intersect(which(species_100_count[,"radius"]==species_100_summary[r,"radius"]),which(species_100_count[,"grid_cells"]==species_100_summary[r,"grid_cells"])),"nClusters"])
}

## Export species_100_summary for reference
## Save once done
write.csv(species_100_summary, file="data/sensitivity_testing/species_100_sensitivity_testing_summary.csv")

##### species - 200km #####
## Get stage numbers
stages <- sort(unique(species_200$stage))

## Add sites
sites2 <- cbind("stages"=stages, "grid_cells"=rep(2, length(stages)))
sites3 <- cbind("stages"=stages, "grid_cells"=rep(3, length(stages)))
sites4 <- cbind("stages"=stages, "grid_cells"=rep(4, length(stages)))
sites <- rbind(sites2, sites3, sites4)

## Add radii
options(scipen = 9999)
radius400 <- cbind(sites, "radius"=rep(400000,nrow(sites)))
radius800 <- cbind(sites, "radius"=rep(800000,nrow(sites)))
radius <- rbind(radius400, radius800)

## Add containers
species_200_count <- cbind(radius, "nGridCellsInTimeBin"=rep(NA,nrow(radius)))
species_200_count <- cbind(species_200_count, "nGridCellsIncludedInSubsamples"=rep(NA,nrow(species_200_count)))
species_200_count <- cbind(species_200_count, "pGridCellsIncludedInSubsamples"=rep(NA,nrow(species_200_count)))
species_200_count <- cbind(species_200_count, "nOccurrencesInTimeBin"=rep(NA,nrow(species_200_count)))
species_200_count <- cbind(species_200_count, "nOccurrencesIncludedInSubsamples"=rep(NA,nrow(species_200_count)))
species_200_count <- cbind(species_200_count, "pOccurrencesIncludedInSubsamples"=rep(NA,nrow(species_200_count)))
species_200_count <- cbind(species_200_count, "nClusters"=rep(NA,nrow(species_200_count)))
species_200_count <- cbind(species_200_count, "nRCRs"=rep(NA,nrow(species_200_count)))
species_200_count <- cbind(species_200_count, "maxNumberOfRCRsInCluster"=rep(NA,nrow(species_200_count)))
species_200_count <- data.frame(species_200_count)

## Format when testing
## Populated up until t=40
start = 230
for(t in start:nrow(species_200_count)){
  print(t)
  ## Get time bin
  samp <- species_200[which(species_200$stage == species_200_count[t,"stages"]),]
  ## species_200_count number of grid cells and occurrences in time
  species_200_count[t,"nGridCellsInTimeBin"] <- length(unique(samp[,"cell_200km"]))
  species_200_count[t,"nOccurrencesInTimeBin"] <- nrow(samp)
  ## If nrow 0, skip
  if(nrow(samp)>0){
    ## If not, get RCRs in Clusters
    raw <- traybake(samp, xy = c("cellx_200km", "celly_200km"), uniqID = "cell_200km", r = species_200_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = species_200_count[t,"grid_cells"], nCookie = 200, nBatch = 200, n.cores = 8)
    seeds <- traybake(samp, xy = c("cellx_200km", "celly_200km"), uniqID = "cell_200km", r = species_200_count[t,"radius"], standardiseCells = F, exhaustClusters = F, nSite = species_200_count[t,"grid_cells"], nCookie = 200, nBatch = 200, output = "seeds", n.cores = 8)
    if(!is.null(raw) && !is.null(seeds)){
      ## count clusters
      species_200_count[t,"nClusters"] <- length(seeds)
      ## count RCRs
      species_200_count[t,"nRCRs"] <- length(unique(unlist(lapply(1:length(raw), function(x) names(raw[[x]])))))
      ## Get maximum RCRs in clusters
      species_200_count[t,"maxNumberOfRCRsInCluster"] <- max(sapply(1:length(seeds), function(x) nrow(seeds[[x]])))
      ## Create new version of samp
      stanSamp <- lapply(1:length(raw), function(x){
        out <- do.call(rbind, raw[[x]])
      })
      stanSamp <- do.call(rbind,stanSamp)
      stanSamp <- stanSamp[!duplicated(stanSamp),]
      ## Get grid cells
      species_200_count[t,"nGridCellsIncludedInSubsamples"] <- length(unique(stanSamp[,"cell_200km"]))
      species_200_count[t,"pGridCellsIncludedInSubsamples"] <- round(species_200_count[t,"nGridCellsIncludedInSubsamples"]/species_200_count[t,"nGridCellsInTimeBin"], digits = 3)
      ## Get occurrences
      species_200_count[t,"nOccurrencesIncludedInSubsamples"] <- nrow(stanSamp)
      species_200_count[t,"pOccurrencesIncludedInSubsamples"] <- round(species_200_count[t,"nOccurrencesIncludedInSubsamples"]/species_200_count[t,"nOccurrencesInTimeBin"], digits = 3)
    }
  } else {
    next
  }
}

## Update NAs to 0s and re-calculate
for(t in 1:nrow(species_200_count)){
  if(is.na(species_200_count[t,"nClusters"])){
    species_200_count[t,"nClusters"] <- 0
  }
  if(is.na(species_200_count[t,"nRCRs"])){
    species_200_count[t,"nRCRs"] <- 0
  }
  if(is.na(species_200_count[t,"maxNumberOfRCRsInCluster"])){
    species_200_count[t,"maxNumberOfRCRsInCluster"] <- 0
  }
  if(is.na(species_200_count[t,"nGridCellsIncludedInSubsamples"])){
    species_200_count[t,"nGridCellsIncludedInSubsamples"] <- 0
    species_200_count[t,"pGridCellsIncludedInSubsamples"] <- round(species_200_count[t,"nGridCellsIncludedInSubsamples"]/species_200_count[t,"nGridCellsInTimeBin"], digits = 3)
  }
  if(is.na(species_200_count[t,"nOccurrencesIncludedInSubsamples"])){
    species_200_count[t,"nOccurrencesIncludedInSubsamples"] <- 0
    species_200_count[t,"pOccurrencesIncludedInSubsamples"] <- round(species_200_count[t,"nOccurrencesIncludedInSubsamples"]/species_200_count[t,"nOccurrencesInTimeBin"], digits = 3)
  }
}

## Save once done
write.csv(species_200_count, file="data/sensitivity_testing/species_200_radius_nSite_sensitivity_testing.csv")

## Then summarise for each method. Number of stage_grid_cells retained, number of occurrences.
species_200_summary <- species_200_count[!duplicated(species_200_count[,c(2,3)]),c(2,3)]
species_200_summary$totalGridCells <- sum(species_200_count[intersect(which(species_200_count[,"radius"]==400000),which(species_200_count[,"grid_cells"]==2)),"nGridCellsInTimeBin"])
species_200_summary$totalOccurrences <- sum(species_200_count[intersect(which(species_200_count[,"radius"]==400000),which(species_200_count[,"grid_cells"]==2)),"nOccurrencesInTimeBin"])
species_200_summary$gridCellsSampled <- NA
species_200_summary$occurrencesSampled <- NA
species_200_summary$proportionOfGridCells <- NA
species_200_summary$proportionOfOccurrences <- NA
species_200_summary$maxRCRsInCluster <- NA
species_200_summary$nClusters <- NA

for(r in 1:nrow(species_200_summary)){
  species_200_summary[r,"gridCellsSampled"] <- sum(species_200_count[intersect(which(species_200_count[,"radius"]==species_200_summary[r,"radius"]),which(species_200_count[,"grid_cells"]==species_200_summary[r,"grid_cells"])),"nGridCellsIncludedInSubsamples"])
  species_200_summary[r,"occurrencesSampled"] <- sum(species_200_count[intersect(which(species_200_count[,"radius"]==species_200_summary[r,"radius"]),which(species_200_count[,"grid_cells"]==species_200_summary[r,"grid_cells"])),"nOccurrencesIncludedInSubsamples"])
  species_200_summary[r,"proportionOfGridCells"] <- round(species_200_summary[r,"gridCellsSampled"]/species_200_summary[r,"totalGridCells"], digits = 3)
  species_200_summary[r,"proportionOfOccurrences"] <- round(species_200_summary[r,"occurrencesSampled"]/species_200_summary[r,"totalOccurrences"], digits = 3)
  species_200_summary[r,"maxRCRsInCluster"] <- max(species_200_count[intersect(which(species_200_count[,"radius"]==species_200_summary[r,"radius"]),which(species_200_count[,"grid_cells"]==species_200_summary[r,"grid_cells"])),"maxNumberOfRCRsInCluster"])
  species_200_summary[r,"nClusters"] <- sum(species_200_count[intersect(which(species_200_count[,"radius"]==species_200_summary[r,"radius"]),which(species_200_count[,"grid_cells"]==species_200_summary[r,"grid_cells"])),"nClusters"])
}

## Export species_200_summary for reference
## Save once done
write.csv(species_200_summary, file="data/sensitivity_testing/species_200_sensitivity_testing_summary.csv")

##### Plotting results of sensitivity test #####
rm(list=ls())

## Read in results of summarisation
genera_50 <- read.csv(("data/sensitivity_testing/genera_50_radius_nSite_sensitivity_testing.csv"), header = T,row.names = 1)
genera_100 <- read.csv(("data/sensitivity_testing/genera_100_radius_nSite_sensitivity_testing.csv"), header = T,row.names = 1)
genera_200 <- read.csv(("data/sensitivity_testing/genera_200_radius_nSite_sensitivity_testing.csv"), header = T,row.names = 1)

species_50 <- read.csv(("data/sensitivity_testing/species_50_radius_nSite_sensitivity_testing.csv"), header = T,row.names = 1)
species_100 <- read.csv(("data/sensitivity_testing/species_100_radius_nSite_sensitivity_testing.csv"), header = T,row.names = 1)
species_200 <- read.csv(("data/sensitivity_testing/species_200_radius_nSite_sensitivity_testing.csv"), header = T,row.names = 1)

## Load stages and get midpoints
time_bins <- read.csv("data/metadata/binning_timescale.csv", row.names = 1)
time_bins$number <- seq(1,nrow(time_bins),1)

##### Genera, 50km grid cells ####
## Plotting frames
plot_genera_50_occs <- time_bins[,c(6,2)]
plot_genera_50_GCs <- time_bins[,c(6,2)]
plot_genera_50_RCRs <- time_bins[,c(6,2)]
plot_genera_50_clusters <- time_bins[,c(6,2)]
plot_genera_50_maxRCRinClus <- time_bins[,c(6,2)]

## Isolate variables to test
radii <- sort(unique(genera_50$radius))
gridCells <- sort(unique(genera_50$grid_cells))

## Create containers
for(r in radii){
  for(g in gridCells){
    ## Get new column names
    cn <- c(colnames(plot_genera_50_occs), paste0(r/1000, "km radius, ", g, " grid cells"))
    ## Add frame
    plot_genera_50_occs <- cbind(plot_genera_50_occs, NA)
    plot_genera_50_GCs <- cbind(plot_genera_50_GCs, NA)
    plot_genera_50_RCRs <- cbind(plot_genera_50_RCRs, NA)
    plot_genera_50_clusters <- cbind(plot_genera_50_clusters, NA)
    plot_genera_50_maxRCRinClus <- cbind(plot_genera_50_maxRCRinClus, NA)
    ## Update colnames
    colnames(plot_genera_50_occs) <- colnames(plot_genera_50_GCs) <- colnames(plot_genera_50_RCRs) <- colnames(plot_genera_50_clusters) <- colnames(plot_genera_50_maxRCRinClus) <- cn
  }
}

## Populate plotting frames
for(r in radii){
  for(g in gridCells){
    ## Get column name
    cn <- paste0(r/1000, "km radius, ", g, " grid cells")
    ## Add frame
    plot_genera_50_occs[genera_50[intersect(which(genera_50$radius==r),which(genera_50$grid_cells==g)),"stages"],cn] <- genera_50[intersect(which(genera_50$radius==r),which(genera_50$grid_cells==g)),"nOccurrencesIncludedInSubsamples"]
    plot_genera_50_GCs[genera_50[intersect(which(genera_50$radius==r),which(genera_50$grid_cells==g)),"stages"],cn] <- genera_50[intersect(which(genera_50$radius==r),which(genera_50$grid_cells==g)),"nGridCellsIncludedInSubsamples"]
    plot_genera_50_RCRs[genera_50[intersect(which(genera_50$radius==r),which(genera_50$grid_cells==g)),"stages"],cn] <- genera_50[intersect(which(genera_50$radius==r),which(genera_50$grid_cells==g)),"nRCRs"]
    plot_genera_50_clusters[genera_50[intersect(which(genera_50$radius==r),which(genera_50$grid_cells==g)),"stages"],cn] <- genera_50[intersect(which(genera_50$radius==r),which(genera_50$grid_cells==g)),"nClusters"]
    plot_genera_50_maxRCRinClus[genera_50[intersect(which(genera_50$radius==r),which(genera_50$grid_cells==g)),"stages"],cn] <- genera_50[intersect(which(genera_50$radius==r),which(genera_50$grid_cells==g)),"maxNumberOfRCRsInCluster"]
  }
}

## Convert NAs to 0
plot_genera_50_occs[is.na(plot_genera_50_occs)] <- 0
plot_genera_50_GCs[is.na(plot_genera_50_GCs)] <- 0
plot_genera_50_RCRs[is.na(plot_genera_50_RCRs)] <- 0
plot_genera_50_clusters[is.na(plot_genera_50_clusters)] <- 0
plot_genera_50_maxRCRinClus[is.na(plot_genera_50_maxRCRinClus)] <- 0

##### Genera, 100km grid cells ####
## Plotting frames
plot_genera_100_occs <- time_bins[,c(6,2)]
plot_genera_100_GCs <- time_bins[,c(6,2)]
plot_genera_100_RCRs <- time_bins[,c(6,2)]
plot_genera_100_clusters <- time_bins[,c(6,2)]
plot_genera_100_maxRCRinClus <- time_bins[,c(6,2)]

## Isolate variables to test
radii <- sort(unique(genera_100$radius))
gridCells <- sort(unique(genera_100$grid_cells))

## Create containers
for(r in radii){
  for(g in gridCells){
    ## Get new column names
    cn <- c(colnames(plot_genera_100_occs), paste0(r/1000, "km radius, ", g, " grid cells"))
    ## Add frame
    plot_genera_100_occs <- cbind(plot_genera_100_occs, NA)
    plot_genera_100_GCs <- cbind(plot_genera_100_GCs, NA)
    plot_genera_100_RCRs <- cbind(plot_genera_100_RCRs, NA)
    plot_genera_100_clusters <- cbind(plot_genera_100_clusters, NA)
    plot_genera_100_maxRCRinClus <- cbind(plot_genera_100_maxRCRinClus, NA)
    ## Update colnames
    colnames(plot_genera_100_occs) <- colnames(plot_genera_100_GCs) <- colnames(plot_genera_100_RCRs) <- colnames(plot_genera_100_clusters) <- colnames(plot_genera_100_maxRCRinClus) <- cn
  }
}

## Populate plotting frames
for(r in radii){
  for(g in gridCells){
    ## Get column name
    cn <- paste0(r/1000, "km radius, ", g, " grid cells")
    ## Add frame
    plot_genera_100_occs[genera_100[intersect(which(genera_100$radius==r),which(genera_100$grid_cells==g)),"stages"],cn] <- genera_100[intersect(which(genera_100$radius==r),which(genera_100$grid_cells==g)),"nOccurrencesIncludedInSubsamples"]
    plot_genera_100_GCs[genera_100[intersect(which(genera_100$radius==r),which(genera_100$grid_cells==g)),"stages"],cn] <- genera_100[intersect(which(genera_100$radius==r),which(genera_100$grid_cells==g)),"nGridCellsIncludedInSubsamples"]
    plot_genera_100_RCRs[genera_100[intersect(which(genera_100$radius==r),which(genera_100$grid_cells==g)),"stages"],cn] <- genera_100[intersect(which(genera_100$radius==r),which(genera_100$grid_cells==g)),"nRCRs"]
    plot_genera_100_clusters[genera_100[intersect(which(genera_100$radius==r),which(genera_100$grid_cells==g)),"stages"],cn] <- genera_100[intersect(which(genera_100$radius==r),which(genera_100$grid_cells==g)),"nClusters"]
    plot_genera_100_maxRCRinClus[genera_100[intersect(which(genera_100$radius==r),which(genera_100$grid_cells==g)),"stages"],cn] <- genera_100[intersect(which(genera_100$radius==r),which(genera_100$grid_cells==g)),"maxNumberOfRCRsInCluster"]
  }
}

## Convert NAs to 0
plot_genera_100_occs[is.na(plot_genera_100_occs)] <- 0
plot_genera_100_GCs[is.na(plot_genera_100_GCs)] <- 0
plot_genera_100_RCRs[is.na(plot_genera_100_RCRs)] <- 0
plot_genera_100_clusters[is.na(plot_genera_100_clusters)] <- 0
plot_genera_100_maxRCRinClus[is.na(plot_genera_100_maxRCRinClus)] <- 0

##### Genera, 200km grid cells ####
## Plotting frames
plot_genera_200_occs <- time_bins[,c(6,2)]
plot_genera_200_GCs <- time_bins[,c(6,2)]
plot_genera_200_RCRs <- time_bins[,c(6,2)]
plot_genera_200_clusters <- time_bins[,c(6,2)]
plot_genera_200_maxRCRinClus <- time_bins[,c(6,2)]

## Isolate variables to test
radii <- sort(unique(genera_200$radius))
gridCells <- sort(unique(genera_200$grid_cells))

## Create containers
for(r in radii){
  for(g in gridCells){
    ## Get new column names
    cn <- c(colnames(plot_genera_200_occs), paste0(r/1000, "km radius, ", g, " grid cells"))
    ## Add frame
    plot_genera_200_occs <- cbind(plot_genera_200_occs, NA)
    plot_genera_200_GCs <- cbind(plot_genera_200_GCs, NA)
    plot_genera_200_RCRs <- cbind(plot_genera_200_RCRs, NA)
    plot_genera_200_clusters <- cbind(plot_genera_200_clusters, NA)
    plot_genera_200_maxRCRinClus <- cbind(plot_genera_200_maxRCRinClus, NA)
    ## Update colnames
    colnames(plot_genera_200_occs) <- colnames(plot_genera_200_GCs) <- colnames(plot_genera_200_RCRs) <- colnames(plot_genera_200_clusters) <- colnames(plot_genera_200_maxRCRinClus) <- cn
  }
}

## Populate plotting frames
for(r in radii){
  for(g in gridCells){
    ## Get column name
    cn <- paste0(r/1000, "km radius, ", g, " grid cells")
    ## Add frame
    plot_genera_200_occs[genera_200[intersect(which(genera_200$radius==r),which(genera_200$grid_cells==g)),"stages"],cn] <- genera_200[intersect(which(genera_200$radius==r),which(genera_200$grid_cells==g)),"nOccurrencesIncludedInSubsamples"]
    plot_genera_200_GCs[genera_200[intersect(which(genera_200$radius==r),which(genera_200$grid_cells==g)),"stages"],cn] <- genera_200[intersect(which(genera_200$radius==r),which(genera_200$grid_cells==g)),"nGridCellsIncludedInSubsamples"]
    plot_genera_200_RCRs[genera_200[intersect(which(genera_200$radius==r),which(genera_200$grid_cells==g)),"stages"],cn] <- genera_200[intersect(which(genera_200$radius==r),which(genera_200$grid_cells==g)),"nRCRs"]
    plot_genera_200_clusters[genera_200[intersect(which(genera_200$radius==r),which(genera_200$grid_cells==g)),"stages"],cn] <- genera_200[intersect(which(genera_200$radius==r),which(genera_200$grid_cells==g)),"nClusters"]
    plot_genera_200_maxRCRinClus[genera_200[intersect(which(genera_200$radius==r),which(genera_200$grid_cells==g)),"stages"],cn] <- genera_200[intersect(which(genera_200$radius==r),which(genera_200$grid_cells==g)),"maxNumberOfRCRsInCluster"]
  }
}

## Convert NAs to 0
plot_genera_200_occs[is.na(plot_genera_200_occs)] <- 0
plot_genera_200_GCs[is.na(plot_genera_200_GCs)] <- 0
plot_genera_200_RCRs[is.na(plot_genera_200_RCRs)] <- 0
plot_genera_200_clusters[is.na(plot_genera_200_clusters)] <- 0
plot_genera_200_maxRCRinClus[is.na(plot_genera_200_maxRCRinClus)] <- 0

##### Plotting occurrences #####
## line vector
lty.v <- c(1,2,3,1,2,3,1,2,3)
col.v <- c(rep(rgb(red = 1, green = 0.4, blue = 0, alpha = 0.75), 3),
           rep(rgb(red = 0, green = 0, blue = 0.6, alpha = 0.75), 3),
           rep(rgb(red = 0, green = 0.6, blue = 0.6, alpha = 0.75), 3),
           rep(rgb(red = 0.4, green = 0.2, blue = 0.6, alpha = 0.75), 3),
           rep(rgb(red = 1, green = 0.6, blue = 1, alpha = 0.75), 3))
col.v.50 <- col.v[c(1:9)]
col.v.100 <- col.v[c(7:12)]
col.v.200 <- col.v[c(10:15)]

## 50km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_50km_occurrences_through_time.pdf")
## 50km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_genera_50_occs$midpoint, y = plot_genera_50_occs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,6500), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of unique occurrences", type = "l", lty = lty.v[1], col = col.v.50[1], main = "50km grid cells, unique occurrences through time")
for(i in 4:ncol(plot_genera_50_occs)){
  lines(x = plot_genera_50_occs$midpoint, y = plot_genera_50_occs[,i], xlim = c(540, 0), ylim = c(0,6500), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.50[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_genera_50_occs)[c(-1,-2)] ,col = col.v.50, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 100km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_100km_occurrences_through_time.pdf")
## 100km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_genera_100_occs$midpoint, y = plot_genera_100_occs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,6500), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of unique occurrences", type = "l", lty = lty.v[1], col = col.v.100[1], main = "100km grid cells, unique occurrences through time")
for(i in 4:ncol(plot_genera_100_occs)){
  lines(x = plot_genera_100_occs$midpoint, y = plot_genera_100_occs[,i], xlim = c(540, 0), ylim = c(0,6500), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.100[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_genera_100_occs)[c(-1,-2)] ,col = col.v.100, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 200km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_200km_occurrences_through_time.pdf")
## 200km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_genera_200_occs$midpoint, y = plot_genera_200_occs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,6500), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of unique occurrences", type = "l", lty = lty.v[1], col = col.v.200[1], main = "200km grid cells, unique occurrences through time")
for(i in 4:ncol(plot_genera_200_occs)){
  lines(x = plot_genera_200_occs$midpoint, y = plot_genera_200_occs[,i], xlim = c(540, 0), ylim = c(0,6500), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.200[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_genera_200_occs)[c(-1,-2)] ,col = col.v.200, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

##### Plotting grid cells #####
## 50km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_50km_grid_cells_through_time.pdf")
## 50km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_genera_50_GCs$midpoint, y = plot_genera_50_GCs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,400), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of grid cells", type = "l", lty = lty.v[1], col = col.v.50[1], main = "50km grid cells, grid cells through time")
for(i in 4:ncol(plot_genera_50_GCs)){
  lines(x = plot_genera_50_GCs$midpoint, y = plot_genera_50_GCs[,i], xlim = c(540, 0), ylim = c(0,400), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.50[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_genera_50_GCs)[c(-1,-2)] ,col = col.v.50, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 100km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_100km_grid_cells_through_time.pdf")
## 100km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_genera_100_GCs$midpoint, y = plot_genera_100_GCs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,400), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of grid cells", type = "l", lty = lty.v[1], col = col.v.100[1], main = "100km grid cells, grid cells through time")
for(i in 4:ncol(plot_genera_100_GCs)){
  lines(x = plot_genera_100_GCs$midpoint, y = plot_genera_100_GCs[,i], xlim = c(540, 0), ylim = c(0,400), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.100[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_genera_100_GCs)[c(-1,-2)] ,col = col.v.100, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 200km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_200km_grid_cells_through_time.pdf")
## 200km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_genera_200_GCs$midpoint, y = plot_genera_200_GCs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,400), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of grid cells", type = "l", lty = lty.v[1], col = col.v.200[1], main = "200km grid cells, grid cells through time")
for(i in 4:ncol(plot_genera_200_GCs)){
  lines(x = plot_genera_200_GCs$midpoint, y = plot_genera_200_GCs[,i], xlim = c(540, 0), ylim = c(0,400), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.200[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_genera_200_GCs)[c(-1,-2)] ,col = col.v.200, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

##### Plotting cookies #####
## 50km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_50km_cookies_through_time.pdf")
## 50km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_genera_50_RCRs$midpoint, y = plot_genera_50_RCRs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,300), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of cookies", type = "l", lty = lty.v[1], col = col.v.50[1], main = "50km grid cells, cookies through time")
for(i in 4:ncol(plot_genera_50_RCRs)){
  lines(x = plot_genera_50_RCRs$midpoint, y = plot_genera_50_RCRs[,i], xlim = c(540, 0), ylim = c(0,300), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.50[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_genera_50_RCRs)[c(-1,-2)] ,col = col.v.50, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 100km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_100km_cookies_through_time.pdf")
## 100km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_genera_100_RCRs$midpoint, y = plot_genera_100_RCRs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,300), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of cookies", type = "l", lty = lty.v[1], col = col.v.100[1], main = "100km grid cells, cookies through time")
for(i in 4:ncol(plot_genera_100_RCRs)){
  lines(x = plot_genera_100_RCRs$midpoint, y = plot_genera_100_RCRs[,i], xlim = c(540, 0), ylim = c(0,300), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.100[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_genera_100_RCRs)[c(-1,-2)] ,col = col.v.100, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 200km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_200km_cookies_through_time.pdf")
## 200km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_genera_200_RCRs$midpoint, y = plot_genera_200_RCRs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,300), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of cookies", type = "l", lty = lty.v[1], col = col.v.200[1], main = "200km grid cells, cookies through time")
for(i in 4:ncol(plot_genera_200_RCRs)){
  lines(x = plot_genera_200_RCRs$midpoint, y = plot_genera_200_RCRs[,i], xlim = c(540, 0), ylim = c(0,300), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.200[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_genera_200_RCRs)[c(-1,-2)] ,col = col.v.200, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

##### Plotting clusters #####
## 50km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_50km_clusters_through_time.pdf")
## 50km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_genera_50_clusters$midpoint, y = plot_genera_50_clusters[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,60), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of clusters", type = "l", lty = lty.v[1], col = col.v.50[1], main = "50km grid cells, clusters through time")
for(i in 4:ncol(plot_genera_50_clusters)){
  lines(x = plot_genera_50_clusters$midpoint, y = plot_genera_50_clusters[,i], xlim = c(540, 0), ylim = c(0,60), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.50[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_genera_50_clusters)[c(-1,-2)] ,col = col.v.50, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 100km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_100km_clusters_through_time.pdf")
## 100km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_genera_100_clusters$midpoint, y = plot_genera_100_clusters[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,60), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of clusters", type = "l", lty = lty.v[1], col = col.v.100[1], main = "100km grid cells, clusters through time")
for(i in 4:ncol(plot_genera_100_clusters)){
  lines(x = plot_genera_100_clusters$midpoint, y = plot_genera_100_clusters[,i], xlim = c(540, 0), ylim = c(0,60), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.100[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_genera_100_clusters)[c(-1,-2)] ,col = col.v.100, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 200km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_200km_clusters_through_time.pdf")
## 200km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_genera_200_clusters$midpoint, y = plot_genera_200_clusters[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,60), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of clusters", type = "l", lty = lty.v[1], col = col.v.200[1], main = "200km grid cells, clusters through time")
for(i in 4:ncol(plot_genera_200_clusters)){
  lines(x = plot_genera_200_clusters$midpoint, y = plot_genera_200_clusters[,i], xlim = c(540, 0), ylim = c(0,60), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.200[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_genera_200_clusters)[c(-1,-2)] ,col = col.v.200, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

##### Plotting maxRCRinClus #####
## 50km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_50km_maxRCRinClus_through_time.pdf")
## 50km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_genera_50_maxRCRinClus$midpoint, y = plot_genera_50_maxRCRinClus[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,100), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Maximum number of cookies in cluster", type = "l", lty = lty.v[1], col = col.v.50[1], main = "50km grid cells, maximum number of cookies in cluster through time")
for(i in 4:ncol(plot_genera_50_maxRCRinClus)){
  lines(x = plot_genera_50_maxRCRinClus$midpoint, y = plot_genera_50_maxRCRinClus[,i], xlim = c(540, 0), ylim = c(0,100), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.50[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_genera_50_maxRCRinClus)[c(-1,-2)] ,col = col.v.50, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 100km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_100km_maxRCRinClus_through_time.pdf")
## 100km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_genera_100_maxRCRinClus$midpoint, y = plot_genera_100_maxRCRinClus[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,100), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Maximum number of cookies in cluster", type = "l", lty = lty.v[1], col = col.v.100[1], main = "100km grid cells, maximum number of cookies in cluster through time")
for(i in 4:ncol(plot_genera_100_maxRCRinClus)){
  lines(x = plot_genera_100_maxRCRinClus$midpoint, y = plot_genera_100_maxRCRinClus[,i], xlim = c(540, 0), ylim = c(0,100), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.100[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_genera_100_maxRCRinClus)[c(-1,-2)] ,col = col.v.100, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 200km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_200km_maxRCRinClus_through_time.pdf")
## 200km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_genera_200_maxRCRinClus$midpoint, y = plot_genera_200_maxRCRinClus[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,100), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Maximum number of cookies in cluster", type = "l", lty = lty.v[1], col = col.v.200[1], main = "200km grid cells, maximum number of cookies in cluster through time")
for(i in 4:ncol(plot_genera_200_maxRCRinClus)){
  lines(x = plot_genera_200_maxRCRinClus$midpoint, y = plot_genera_200_maxRCRinClus[,i], xlim = c(540, 0), ylim = c(0,100), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.200[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_genera_200_maxRCRinClus)[c(-1,-2)] ,col = col.v.200, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

##### species, 50km grid cells ####
## Plotting frames
plot_species_50_occs <- time_bins[,c(6,2)]
plot_species_50_GCs <- time_bins[,c(6,2)]
plot_species_50_RCRs <- time_bins[,c(6,2)]
plot_species_50_clusters <- time_bins[,c(6,2)]
plot_species_50_maxRCRinClus <- time_bins[,c(6,2)]

## Isolate variables to test
radii <- sort(unique(species_50$radius))
gridCells <- sort(unique(species_50$grid_cells))

## Create containers
for(r in radii){
  for(g in gridCells){
    ## Get new column names
    cn <- c(colnames(plot_species_50_occs), paste0(r/1000, "km radius, ", g, " grid cells"))
    ## Add frame
    plot_species_50_occs <- cbind(plot_species_50_occs, NA)
    plot_species_50_GCs <- cbind(plot_species_50_GCs, NA)
    plot_species_50_RCRs <- cbind(plot_species_50_RCRs, NA)
    plot_species_50_clusters <- cbind(plot_species_50_clusters, NA)
    plot_species_50_maxRCRinClus <- cbind(plot_species_50_maxRCRinClus, NA)
    ## Update colnames
    colnames(plot_species_50_occs) <- colnames(plot_species_50_GCs) <- colnames(plot_species_50_RCRs) <- colnames(plot_species_50_clusters) <- colnames(plot_species_50_maxRCRinClus) <- cn
  }
}

## Populate plotting frames
for(r in radii){
  for(g in gridCells){
    ## Get column name
    cn <- paste0(r/1000, "km radius, ", g, " grid cells")
    ## Add frame
    plot_species_50_occs[species_50[intersect(which(species_50$radius==r),which(species_50$grid_cells==g)),"stages"],cn] <- species_50[intersect(which(species_50$radius==r),which(species_50$grid_cells==g)),"nOccurrencesIncludedInSubsamples"]
    plot_species_50_GCs[species_50[intersect(which(species_50$radius==r),which(species_50$grid_cells==g)),"stages"],cn] <- species_50[intersect(which(species_50$radius==r),which(species_50$grid_cells==g)),"nGridCellsIncludedInSubsamples"]
    plot_species_50_RCRs[species_50[intersect(which(species_50$radius==r),which(species_50$grid_cells==g)),"stages"],cn] <- species_50[intersect(which(species_50$radius==r),which(species_50$grid_cells==g)),"nRCRs"]
    plot_species_50_clusters[species_50[intersect(which(species_50$radius==r),which(species_50$grid_cells==g)),"stages"],cn] <- species_50[intersect(which(species_50$radius==r),which(species_50$grid_cells==g)),"nClusters"]
    plot_species_50_maxRCRinClus[species_50[intersect(which(species_50$radius==r),which(species_50$grid_cells==g)),"stages"],cn] <- species_50[intersect(which(species_50$radius==r),which(species_50$grid_cells==g)),"maxNumberOfRCRsInCluster"]
  }
}

## Convert NAs to 0
plot_species_50_occs[is.na(plot_species_50_occs)] <- 0
plot_species_50_GCs[is.na(plot_species_50_GCs)] <- 0
plot_species_50_RCRs[is.na(plot_species_50_RCRs)] <- 0
plot_species_50_clusters[is.na(plot_species_50_clusters)] <- 0
plot_species_50_maxRCRinClus[is.na(plot_species_50_maxRCRinClus)] <- 0

##### species, 100km grid cells ####
## Plotting frames
plot_species_100_occs <- time_bins[,c(6,2)]
plot_species_100_GCs <- time_bins[,c(6,2)]
plot_species_100_RCRs <- time_bins[,c(6,2)]
plot_species_100_clusters <- time_bins[,c(6,2)]
plot_species_100_maxRCRinClus <- time_bins[,c(6,2)]

## Isolate variables to test
radii <- sort(unique(species_100$radius))
gridCells <- sort(unique(species_100$grid_cells))

## Create containers
for(r in radii){
  for(g in gridCells){
    ## Get new column names
    cn <- c(colnames(plot_species_100_occs), paste0(r/1000, "km radius, ", g, " grid cells"))
    ## Add frame
    plot_species_100_occs <- cbind(plot_species_100_occs, NA)
    plot_species_100_GCs <- cbind(plot_species_100_GCs, NA)
    plot_species_100_RCRs <- cbind(plot_species_100_RCRs, NA)
    plot_species_100_clusters <- cbind(plot_species_100_clusters, NA)
    plot_species_100_maxRCRinClus <- cbind(plot_species_100_maxRCRinClus, NA)
    ## Update colnames
    colnames(plot_species_100_occs) <- colnames(plot_species_100_GCs) <- colnames(plot_species_100_RCRs) <- colnames(plot_species_100_clusters) <- colnames(plot_species_100_maxRCRinClus) <- cn
  }
}

## Populate plotting frames
for(r in radii){
  for(g in gridCells){
    ## Get column name
    cn <- paste0(r/1000, "km radius, ", g, " grid cells")
    ## Add frame
    plot_species_100_occs[species_100[intersect(which(species_100$radius==r),which(species_100$grid_cells==g)),"stages"],cn] <- species_100[intersect(which(species_100$radius==r),which(species_100$grid_cells==g)),"nOccurrencesIncludedInSubsamples"]
    plot_species_100_GCs[species_100[intersect(which(species_100$radius==r),which(species_100$grid_cells==g)),"stages"],cn] <- species_100[intersect(which(species_100$radius==r),which(species_100$grid_cells==g)),"nGridCellsIncludedInSubsamples"]
    plot_species_100_RCRs[species_100[intersect(which(species_100$radius==r),which(species_100$grid_cells==g)),"stages"],cn] <- species_100[intersect(which(species_100$radius==r),which(species_100$grid_cells==g)),"nRCRs"]
    plot_species_100_clusters[species_100[intersect(which(species_100$radius==r),which(species_100$grid_cells==g)),"stages"],cn] <- species_100[intersect(which(species_100$radius==r),which(species_100$grid_cells==g)),"nClusters"]
    plot_species_100_maxRCRinClus[species_100[intersect(which(species_100$radius==r),which(species_100$grid_cells==g)),"stages"],cn] <- species_100[intersect(which(species_100$radius==r),which(species_100$grid_cells==g)),"maxNumberOfRCRsInCluster"]
  }
}

## Convert NAs to 0
plot_species_100_occs[is.na(plot_species_100_occs)] <- 0
plot_species_100_GCs[is.na(plot_species_100_GCs)] <- 0
plot_species_100_RCRs[is.na(plot_species_100_RCRs)] <- 0
plot_species_100_clusters[is.na(plot_species_100_clusters)] <- 0
plot_species_100_maxRCRinClus[is.na(plot_species_100_maxRCRinClus)] <- 0

##### species, 200km grid cells ####
## Plotting frames
plot_species_200_occs <- time_bins[,c(6,2)]
plot_species_200_GCs <- time_bins[,c(6,2)]
plot_species_200_RCRs <- time_bins[,c(6,2)]
plot_species_200_clusters <- time_bins[,c(6,2)]
plot_species_200_maxRCRinClus <- time_bins[,c(6,2)]

## Isolate variables to test
radii <- sort(unique(species_200$radius))
gridCells <- sort(unique(species_200$grid_cells))

## Create containers
for(r in radii){
  for(g in gridCells){
    ## Get new column names
    cn <- c(colnames(plot_species_200_occs), paste0(r/1000, "km radius, ", g, " grid cells"))
    ## Add frame
    plot_species_200_occs <- cbind(plot_species_200_occs, NA)
    plot_species_200_GCs <- cbind(plot_species_200_GCs, NA)
    plot_species_200_RCRs <- cbind(plot_species_200_RCRs, NA)
    plot_species_200_clusters <- cbind(plot_species_200_clusters, NA)
    plot_species_200_maxRCRinClus <- cbind(plot_species_200_maxRCRinClus, NA)
    ## Update colnames
    colnames(plot_species_200_occs) <- colnames(plot_species_200_GCs) <- colnames(plot_species_200_RCRs) <- colnames(plot_species_200_clusters) <- colnames(plot_species_200_maxRCRinClus) <- cn
  }
}

## Populate plotting frames
for(r in radii){
  for(g in gridCells){
    ## Get column name
    cn <- paste0(r/1000, "km radius, ", g, " grid cells")
    ## Add frame
    plot_species_200_occs[species_200[intersect(which(species_200$radius==r),which(species_200$grid_cells==g)),"stages"],cn] <- species_200[intersect(which(species_200$radius==r),which(species_200$grid_cells==g)),"nOccurrencesIncludedInSubsamples"]
    plot_species_200_GCs[species_200[intersect(which(species_200$radius==r),which(species_200$grid_cells==g)),"stages"],cn] <- species_200[intersect(which(species_200$radius==r),which(species_200$grid_cells==g)),"nGridCellsIncludedInSubsamples"]
    plot_species_200_RCRs[species_200[intersect(which(species_200$radius==r),which(species_200$grid_cells==g)),"stages"],cn] <- species_200[intersect(which(species_200$radius==r),which(species_200$grid_cells==g)),"nRCRs"]
    plot_species_200_clusters[species_200[intersect(which(species_200$radius==r),which(species_200$grid_cells==g)),"stages"],cn] <- species_200[intersect(which(species_200$radius==r),which(species_200$grid_cells==g)),"nClusters"]
    plot_species_200_maxRCRinClus[species_200[intersect(which(species_200$radius==r),which(species_200$grid_cells==g)),"stages"],cn] <- species_200[intersect(which(species_200$radius==r),which(species_200$grid_cells==g)),"maxNumberOfRCRsInCluster"]
  }
}

## Convert NAs to 0
plot_species_200_occs[is.na(plot_species_200_occs)] <- 0
plot_species_200_GCs[is.na(plot_species_200_GCs)] <- 0
plot_species_200_RCRs[is.na(plot_species_200_RCRs)] <- 0
plot_species_200_clusters[is.na(plot_species_200_clusters)] <- 0
plot_species_200_maxRCRinClus[is.na(plot_species_200_maxRCRinClus)] <- 0

##### Plotting occurrences #####
## 50km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_50km_occurrences_through_time.pdf")
## 50km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_species_50_occs$midpoint, y = plot_species_50_occs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,6500), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of unique occurrences", type = "l", lty = lty.v[1], col = col.v.50[1], main = "50km grid cells, unique occurrences through time")
for(i in 4:ncol(plot_species_50_occs)){
  lines(x = plot_species_50_occs$midpoint, y = plot_species_50_occs[,i], xlim = c(540, 0), ylim = c(0,6500), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.50[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_species_50_occs)[c(-1,-2)] ,col = col.v.50, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 100km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_100km_occurrences_through_time.pdf")
## 100km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_species_100_occs$midpoint, y = plot_species_100_occs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,6500), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of unique occurrences", type = "l", lty = lty.v[1], col = col.v.100[1], main = "100km grid cells, unique occurrences through time")
for(i in 4:ncol(plot_species_100_occs)){
  lines(x = plot_species_100_occs$midpoint, y = plot_species_100_occs[,i], xlim = c(540, 0), ylim = c(0,6500), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.100[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_species_100_occs)[c(-1,-2)] ,col = col.v.100, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 200km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_200km_occurrences_through_time.pdf")
## 200km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_species_200_occs$midpoint, y = plot_species_200_occs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,6500), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of unique occurrences", type = "l", lty = lty.v[1], col = col.v.200[1], main = "200km grid cells, unique occurrences through time")
for(i in 4:ncol(plot_species_200_occs)){
  lines(x = plot_species_200_occs$midpoint, y = plot_species_200_occs[,i], xlim = c(540, 0), ylim = c(0,6500), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.200[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_species_200_occs)[c(-1,-2)] ,col = col.v.200, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

##### Plotting grid cells #####
## 50km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_50km_grid_cells_through_time.pdf")
## 50km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_species_50_GCs$midpoint, y = plot_species_50_GCs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,400), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of grid cells", type = "l", lty = lty.v[1], col = col.v.50[1], main = "50km grid cells, grid cells through time")
for(i in 4:ncol(plot_species_50_GCs)){
  lines(x = plot_species_50_GCs$midpoint, y = plot_species_50_GCs[,i], xlim = c(540, 0), ylim = c(0,400), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.50[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_species_50_GCs)[c(-1,-2)] ,col = col.v.50, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 100km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_100km_grid_cells_through_time.pdf")
## 100km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_species_100_GCs$midpoint, y = plot_species_100_GCs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,400), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of grid cells", type = "l", lty = lty.v[1], col = col.v.100[1], main = "100km grid cells, grid cells through time")
for(i in 4:ncol(plot_species_100_GCs)){
  lines(x = plot_species_100_GCs$midpoint, y = plot_species_100_GCs[,i], xlim = c(540, 0), ylim = c(0,400), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.100[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_species_100_GCs)[c(-1,-2)] ,col = col.v.100, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 200km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_200km_grid_cells_through_time.pdf")
## 200km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_species_200_GCs$midpoint, y = plot_species_200_GCs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,400), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of grid cells", type = "l", lty = lty.v[1], col = col.v.200[1], main = "200km grid cells, grid cells through time")
for(i in 4:ncol(plot_species_200_GCs)){
  lines(x = plot_species_200_GCs$midpoint, y = plot_species_200_GCs[,i], xlim = c(540, 0), ylim = c(0,400), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.200[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_species_200_GCs)[c(-1,-2)] ,col = col.v.200, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

##### Plotting cookies #####
## 50km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_50km_cookies_through_time.pdf")
## 50km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_species_50_RCRs$midpoint, y = plot_species_50_RCRs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,300), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of cookies", type = "l", lty = lty.v[1], col = col.v.50[1], main = "50km grid cells, cookies through time")
for(i in 4:ncol(plot_species_50_RCRs)){
  lines(x = plot_species_50_RCRs$midpoint, y = plot_species_50_RCRs[,i], xlim = c(540, 0), ylim = c(0,300), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.50[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_species_50_RCRs)[c(-1,-2)] ,col = col.v.50, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 100km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_100km_cookies_through_time.pdf")
## 100km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_species_100_RCRs$midpoint, y = plot_species_100_RCRs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,300), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of cookies", type = "l", lty = lty.v[1], col = col.v.100[1], main = "100km grid cells, cookies through time")
for(i in 4:ncol(plot_species_100_RCRs)){
  lines(x = plot_species_100_RCRs$midpoint, y = plot_species_100_RCRs[,i], xlim = c(540, 0), ylim = c(0,300), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.100[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_species_100_RCRs)[c(-1,-2)] ,col = col.v.100, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 200km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_200km_cookies_through_time.pdf")
## 200km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_species_200_RCRs$midpoint, y = plot_species_200_RCRs[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,300), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of cookies", type = "l", lty = lty.v[1], col = col.v.200[1], main = "200km grid cells, cookies through time")
for(i in 4:ncol(plot_species_200_RCRs)){
  lines(x = plot_species_200_RCRs$midpoint, y = plot_species_200_RCRs[,i], xlim = c(540, 0), ylim = c(0,300), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.200[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_species_200_RCRs)[c(-1,-2)] ,col = col.v.200, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

##### Plotting clusters #####
## 50km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_50km_clusters_through_time.pdf")
## 50km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_species_50_clusters$midpoint, y = plot_species_50_clusters[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,60), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of clusters", type = "l", lty = lty.v[1], col = col.v.50[1], main = "50km grid cells, clusters through time")
for(i in 4:ncol(plot_species_50_clusters)){
  lines(x = plot_species_50_clusters$midpoint, y = plot_species_50_clusters[,i], xlim = c(540, 0), ylim = c(0,60), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.50[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_species_50_clusters)[c(-1,-2)] ,col = col.v.50, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 100km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_100km_clusters_through_time.pdf")
## 100km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_species_100_clusters$midpoint, y = plot_species_100_clusters[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,60), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of clusters", type = "l", lty = lty.v[1], col = col.v.100[1], main = "100km grid cells, clusters through time")
for(i in 4:ncol(plot_species_100_clusters)){
  lines(x = plot_species_100_clusters$midpoint, y = plot_species_100_clusters[,i], xlim = c(540, 0), ylim = c(0,60), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.100[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_species_100_clusters)[c(-1,-2)] ,col = col.v.100, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 200km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_200km_clusters_through_time.pdf")
## 200km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_species_200_clusters$midpoint, y = plot_species_200_clusters[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,60), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Number of clusters", type = "l", lty = lty.v[1], col = col.v.200[1], main = "200km grid cells, clusters through time")
for(i in 4:ncol(plot_species_200_clusters)){
  lines(x = plot_species_200_clusters$midpoint, y = plot_species_200_clusters[,i], xlim = c(540, 0), ylim = c(0,60), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.200[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_species_200_clusters)[c(-1,-2)] ,col = col.v.200, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

##### Plotting maxRCRinClus #####
## 50km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_50km_maxRCRinClus_through_time.pdf")
## 50km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_species_50_maxRCRinClus$midpoint, y = plot_species_50_maxRCRinClus[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,100), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Maximum number of cookies in cluster", type = "l", lty = lty.v[1], col = col.v.50[1], main = "50km grid cells, maximum number of cookies in cluster through time")
for(i in 4:ncol(plot_species_50_maxRCRinClus)){
  lines(x = plot_species_50_maxRCRinClus$midpoint, y = plot_species_50_maxRCRinClus[,i], xlim = c(540, 0), ylim = c(0,100), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.50[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_species_50_maxRCRinClus)[c(-1,-2)] ,col = col.v.50, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 100km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_100km_maxRCRinClus_through_time.pdf")
## 100km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_species_100_maxRCRinClus$midpoint, y = plot_species_100_maxRCRinClus[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,100), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Maximum number of cookies in cluster", type = "l", lty = lty.v[1], col = col.v.100[1], main = "100km grid cells, maximum number of cookies in cluster through time")
for(i in 4:ncol(plot_species_100_maxRCRinClus)){
  lines(x = plot_species_100_maxRCRinClus$midpoint, y = plot_species_100_maxRCRinClus[,i], xlim = c(540, 0), ylim = c(0,100), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.100[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_species_100_maxRCRinClus)[c(-1,-2)] ,col = col.v.100, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## 200km
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_200km_maxRCRinClus_through_time.pdf")
## 200km grid cells
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = plot_species_200_maxRCRinClus$midpoint, y = plot_species_200_maxRCRinClus[,3], axes = FALSE, xlim = c(540, 0), ylim = c(0,100), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Maximum number of cookies in cluster", type = "l", lty = lty.v[1], col = col.v.200[1], main = "200km grid cells, maximum number of cookies in cluster through time")
for(i in 4:ncol(plot_species_200_maxRCRinClus)){
  lines(x = plot_species_200_maxRCRinClus$midpoint, y = plot_species_200_maxRCRinClus[,i], xlim = c(540, 0), ylim = c(0,100), yaxs="i", xaxs="i",
        xlab = NA, ylab = "", type = "l", lty = lty.v[i-2], col = col.v.200[i-2])
}
box()
axis(2)
axis(4)
legend("top",legend = colnames(plot_species_200_maxRCRinClus)[c(-1,-2)] ,col = col.v.200, lty = lty.v, cex = 0.8)
axis_geo(side = 1, intervals = "international periods")
dev.off()
