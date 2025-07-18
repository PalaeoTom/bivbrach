## 3.3 Final spatial subsampling
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
#setwd("~/GitHub/bivbrach/")
setwd("/Users/tjs/R_packages/bivbrach")
genera <- readRDS("data/final/master_50_2_2_uniq.Rds")
species <- readRDS("data/final/master_50_uniq_species.Rds")
home <- getwd()

## Set number of cores
core.set <- 8

## Output directory
#output.dir <- "~/Dropbox/unfinished_projects/bivalve_brachiopod/traybake_output"
output.dir <- "~/Library/CloudStorage/Dropbox/unfinished_projects/bivalve_brachiopod/traybake_output"

## Read in richness function
source("functions/richness.R")

## Set radius, nBatch and nBites
radius <- 100000
nBatch <- 100
nBites <- 100

## Define covariate columns using vector
cov.names <- c("cellx_50km", "celly_50km", "cellLith", "cellBath", "cellReef")

## Change covariates to numeric
genera$cellBath <- as.numeric(genera$cellBath)
genera$cellLith <- as.numeric(genera$cellLith)
genera$cellReef <- as.numeric(genera$cellReef)

##### Genera - 2 cells, 50km cells, 100km radius: creating frame #####
## Read in genera count
genera_count <- read.csv("data/sensitivity_testing/genera_50_radius_nSite_sensitivity_testing.csv", header = T, row.names = 1)

## Set number of cells
nCells <- 2

## Identify relevant data from results of sensivity analysis
rel <- genera_count[intersect(which(genera_count$grid_cells == nCells), which(genera_count$radius == radius)),]

## Inititlise cluster_2cell vector
cluster_2cell <- data.frame("stage"=NA, "cluster"=NA)
for(r in 1:nrow(rel)){
  ## Get new index
  if(rel[r,"nClusters"]>0){
    stage <- rep(rel[r,"stages"],rel[r,"nClusters"])
    clus <- paste0(rel[r,"stages"], "_", seq(1,rel[r,"nClusters"],1))
    new <- cbind("stage"=stage,"cluster"=clus)
    cluster_2cell <- rbind(cluster_2cell,new)
  }
}
cluster_2cell <- cluster_2cell[-1,]

## Add columns to be populated to finalise template output. Make sure absolute latitude is on end and order of everything else matches cov.names
cluster_2cell$bivalve <- NA
cluster_2cell$brachiopod <- NA
cluster_2cell$cookiePalaeoLng <- NA
cluster_2cell$cookiePalaeoLat <- NA
cluster_2cell$cookieLith <- NA
cluster_2cell$cookieBath <- NA
cluster_2cell$cookieReef <- NA
cluster_2cell$cookieAbsLat <- NA

## Create three outputs
cluster_2cell_raw <- cluster_2cell
cluster_2cell_stdOccs_noRepl <- cluster_2cell
cluster_2cell_stdOccs_Repl <- cluster_2cell

## Get stages from cluster_2cell
stages <- as.character(sort(as.numeric(unique(cluster_2cell[,"stage"]))))

##### Genera - 2 cells, 50km cells, 100km radius: spatial subsampling #####
## For each stage, run traybake
#start = 1
#for(i in start:length(stages)){
#  ## Get sample
#  samp <- genera[which(genera$stage == stages[i]),]
#  if(nrow(samp)>0){
#    ## Run traybake - standardise separately
#    tray <- traybake(samp, xy = c("cellx_50km", "celly_50km"), uniqID = "cell_50km", r = radius, standardiseCells = F, exhaustClusters = F, nSite = nCells, nBatch = nBatch, n.cores = 8)
#    ## If not null, proceed
#    if(!is.null(tray)){
#      ## Save output
#      saveRDS(tray, paste0(output.dir, "/genera_stage", stages[i], "_", nCells, "GridCells.Rds"))
#    }
#  }
#}

##### Genera - 2 cells, 50km cells, 100km radius: richness estimates from occurrences standardised without replacement #####
#cluster_2cell <- read.csv("data/analysis_data/genera_2cell_stdOccs_noRepl.csv", row.names = 1)

## Set number of occurrences to be drawn as minimum
nOccs <- (nCells*5)

## Update stages
## For each stage, load data, summarise across clusters
start = 1
for(i in start:length(stages)){
  ## try reading in data -
  tray <- suppressWarnings(tryCatch(readRDS(paste0(output.dir, "/genera_stage", stages[i], "_", nCells, "GridCells.Rds")), error = function(e){}))
  if(is.null(tray)){
    next
  } else {
    ## get clusters
    clusters <- 1:length(tray[[1]])
    ## for each cluster
    for(c in clusters){
      ## Get row from cluster and stage to populate
      row <- which(cluster_2cell_stdOccs_noRepl$cluster == str_flatten(c(stages[i],c),collapse = "_"))
      ## Isolate clusters
      cluster <- lapply(1:nBatch, function(x) tray[[x]][[c]])
      ## Convert clusters into vectors of data
      values <- mclapply(1:length(cluster), mc.cores = core.set, function(y){
        ## richness and covariates
        rich <- richness(data = cluster[[y]], cellID = "cell_50km", nCells = nCells, cov.names = cov.names, nDraws = nBites, nOccs = nOccs, standardiseOccs = T, standardiseOccsWithReplacement = F, splitTaxa = F, taxonName = "combined_name", taxaToTally = c("Mollusca", "Brachiopoda"), n.cores = 1)
      })
      ## Get median of richness and mean of everything else
      summary <- c()
      for(tt in 1:2){
        summary <- c(summary, median(sapply(1:length(values), function(x) values[[x]][tt])))
      }
      for(cc in 3:7){
        summary <- c(summary, mean(sapply(1:length(values), function(x) values[[x]][cc])))
      }
      ## Palaeolat is celly (summary[4]). Add absolute value to end
      summary <- c(summary, abs(summary[4]))
      ## Populate row
      cluster_2cell_stdOccs_noRepl[row, c(3:ncol(cluster_2cell_stdOccs_noRepl))] <- summary
    }
    ## Save to keep track in between stages
    write.csv(cluster_2cell_stdOccs_noRepl, file = "data/analysis_data/genera_2cell_stdOccs_noRepl.csv")
  }
}


## Add term for pre/post permian and stage midpoints
timeData <- read.csv("data/metadata/binning_timescale.csv", row.names = 1)
timeData$number <- seq(1,nrow(timeData),1)

## Add containers
cluster_2cell_stdOccs_noRepl$stage_midpoint <- NA
cluster_2cell_stdOccs_noRepl$PTME <- NA

## Stage 48 is last pre-PTME
## for each timeData
for(i in timeData$number){
  if(any(cluster_2cell_stdOccs_noRepl[,"stage"]==i)){
    cluster_2cell_stdOccs_noRepl[which(cluster_2cell_stdOccs_noRepl$stage == i),"stage_midpoint"] <- timeData[i,"midpoint"]
    if(i <= 48){
      cluster_2cell_stdOccs_noRepl[which(cluster_2cell_stdOccs_noRepl$stage == i),"PTME"] <- "PrePTME"
    } else {
      cluster_2cell_stdOccs_noRepl[which(cluster_2cell_stdOccs_noRepl$stage == i),"PTME"] <- "PostPTME"
    }
  }
}

## Export
write.csv(cluster_2cell_stdOccs_noRepl, file = "data/analysis_data/genera_2cell_stdOccs_noRepl.csv")

##### Genera - 2 cells, 50km cells, 100km radius: richness estimates from occurrences standardised with replacement #####
## Set number of occurrences to be drawn as minimum
nOccs <- 50

## Update stages
## For each stage, load data, summarise across clusters
start = 1
for(i in start:length(stages)){
  ## try reading in data -
  tray <- suppressWarnings(tryCatch(readRDS(paste0(output.dir, "/genera_stage", stages[i], "_", nCells, "GridCells.Rds")), error = function(e){}))
  if(is.null(tray)){
    next
  } else {
    ## get clusters
    clusters <- 1:length(tray[[1]])
    ## for each cluster
    for(c in clusters){
      ## Get row from cluster and stage to populate
      row <- which(cluster_2cell_stdOccs_noRepl$cluster == str_flatten(c(stages[i],c),collapse = "_"))
      ## Isolate clusters
      cluster <- lapply(1:nBatch, function(x) tray[[x]][[c]])
      ## Convert clusters into vectors of data
      values <- mclapply(1:length(cluster), mc.cores = core.set, function(y){
        ## richness and covariates
        rich <- richness(data = cluster[[y]], cellID = "cell_50km", nCells = nCells, cov.names = cov.names, nDraws = nBites, nOccs = nOccs, standardiseOccs = T, standardiseOccsWithReplacement = T, splitTaxa = T, taxonName = "combined_name", taxaToTally = c("Mollusca", "Brachiopoda"), n.cores = 1)
      })
      ## Get median of richness and mean of everything else
      summary <- c()
      for(tt in 1:2){
        summary <- c(summary, median(sapply(1:length(values), function(x) values[[x]][tt])))
      }
      for(cc in 3:7){
        summary <- c(summary, mean(sapply(1:length(values), function(x) values[[x]][cc])))
      }
      ## Palaeolat is celly (summary[4]). Add absolute value to end
      summary <- c(summary, abs(summary[4]))
      ## Populate row
      cluster_2cell_stdOccs_Repl[row, c(3:ncol(cluster_2cell_stdOccs_Repl))] <- summary
    }
    ## Save to keep track in between stages
    write.csv(cluster_2cell_stdOccs_Repl, file = "data/analysis_data/genera_2cell_stdOccs_Repl.csv")
  }
}

## Add term for pre/post permian and stage midpoints
timeData <- read.csv("data/metadata/binning_timescale.csv", row.names = 1)
timeData$number <- seq(1,nrow(timeData),1)

## Add containers
cluster_2cell_stdOccs_Repl$stage_midpoint <- NA
cluster_2cell_stdOccs_Repl$PTME <- NA

## Stage 48 is last pre-PTME
## for each timeData
for(i in timeData$number){
  if(any(cluster_2cell_stdOccs_Repl[,"stage"]==i)){
    cluster_2cell_stdOccs_Repl[which(cluster_2cell_stdOccs_Repl$stage == i),"stage_midpoint"] <- timeData[i,"midpoint"]
    if(i <= 48){
      cluster_2cell_stdOccs_Repl[which(cluster_2cell_stdOccs_Repl$stage == i),"PTME"] <- "PrePTME"
    } else {
      cluster_2cell_stdOccs_Repl[which(cluster_2cell_stdOccs_Repl$stage == i),"PTME"] <- "PostPTME"
    }
  }
}

## Export
write.csv(cluster_2cell_stdOccs_Repl, file = "data/analysis_data/genera_2cell_stdOccs_Repl.csv")

##### Genera - 2 cells, 50km cells, 100km radius: richness estimates from raw occurrences #####
## For each stage, load data, summarise across clusters
start = 1
for(i in start:length(stages)){
  ## try reading in data -
  tray <- suppressWarnings(tryCatch(readRDS(paste0(output.dir, "/genera_stage", stages[i], "_", nCells, "GridCells.Rds")), error = function(e){}))
  if(is.null(tray)){
    next
  } else {
    ## get clusters
    clusters <- 1:length(tray[[1]])
    ## for each cluster
    for(c in clusters){
      ## Get row from cluster and stage to populate
      row <- which(cluster_2cell_stdOccs_noRepl$cluster == str_flatten(c(stages[i],c),collapse = "_"))
      ## Isolate clusters
      cluster <- lapply(1:nBatch, function(x) tray[[x]][[c]])
      ## Convert clusters into vectors of data
      values <- mclapply(1:length(cluster), mc.cores = core.set, function(y){
        ## richness and covariates
        rich <- richness(data = cluster[[y]], cellID = "cell_50km", nCells = nCells, cov.names = cov.names, nDraws = nBites, nOccs = nOccs, standardiseOccs = F, standardiseOccsWithReplacement = F, splitTaxa = F, taxonName = "combined_name", taxaToTally = c("Mollusca", "Brachiopoda"), n.cores = 1)
      })
      ## Get median of richness and mean of everything else
      summary <- c()
      for(tt in 1:2){
        summary <- c(summary, median(sapply(1:length(values), function(x) values[[x]][tt])))
      }
      for(cc in 3:7){
        summary <- c(summary, mean(sapply(1:length(values), function(x) values[[x]][cc])))
      }
      ## Palaeolat is celly (summary[4]). Add absolute value to end
      summary <- c(summary, abs(summary[4]))
      ## Populate row
      cluster_2cell_raw[row, c(3:ncol(cluster_2cell_raw))] <- summary
    }
    ## Save to keep track in between stages
    write.csv(cluster_2cell_raw, file = "data/analysis_data/genera_2cell_raw.csv")
  }
}

## Add term for pre/post permian and stage midpoints
timeData <- read.csv("data/metadata/binning_timescale.csv", row.names = 1)
timeData$number <- seq(1,nrow(timeData),1)

## Add containers
cluster_2cell_raw$stage_midpoint <- NA
cluster_2cell_raw$PTME <- NA

## Stage 48 is last pre-PTME
## for each timeData
for(i in timeData$number){
  if(any(cluster_2cell_raw[,"stage"]==i)){
    cluster_2cell_raw[which(cluster_2cell_raw$stage == i),"stage_midpoint"] <- timeData[i,"midpoint"]
    if(i <= 48){
      cluster_2cell_raw[which(cluster_2cell_raw$stage == i),"PTME"] <- "PrePTME"
    } else {
      cluster_2cell_raw[which(cluster_2cell_raw$stage == i),"PTME"] <- "PostPTME"
    }
  }
}

## Export
write.csv(cluster_2cell_raw, file = "data/analysis_data/genera_2cell_raw.csv")

##### Genera - 3 cells, 50km cells, 100km radius: creating frame #####
## Read in genera count
genera_count <- read.csv("data/sensitivity_testing/genera_50_radius_nSite_sensitivity_testing.csv", header = T, row.names = 1)

## Set number of cells
nCells <- 3

## Identify relevant data from results of sensivity analysis
rel <- genera_count[intersect(which(genera_count$grid_cells == nCells), which(genera_count$radius == radius)),]

## Inititlise cluster_3cell vector
cluster_3cell <- data.frame("stage"=NA, "cluster"=NA)
for(r in 1:nrow(rel)){
  ## Get new index
  if(rel[r,"nClusters"]>0){
    stage <- rep(rel[r,"stages"],rel[r,"nClusters"])
    clus <- paste0(rel[r,"stages"], "_", seq(1,rel[r,"nClusters"],1))
    new <- cbind("stage"=stage,"cluster"=clus)
    cluster_3cell <- rbind(cluster_3cell,new)
  }
}
cluster_3cell <- cluster_3cell[-1,]

## Add columns to be populated to finalise template output
cluster_3cell$bivalve <- NA
cluster_3cell$brachiopod <- NA
cluster_3cell$cookiePalaeoLng <- NA
cluster_3cell$cookiePalaeoLat <- NA
cluster_3cell$cookieLith <- NA
cluster_3cell$cookieBath <- NA
cluster_3cell$cookieReef <- NA
cluster_3cell$cookieAbsLat <- NA

## Create three outputs
cluster_3cell_raw <- cluster_3cell
cluster_3cell_stdOccs_noRepl <- cluster_3cell
cluster_3cell_stdOccs_Repl <- cluster_3cell

## Get stages from cluster_3cell
stages <- as.character(sort(as.numeric(unique(cluster_3cell[,"stage"]))))

##### Genera - 3 cells, 50km cells, 100km radius: spatial subsampling #####
## For each stage, run traybake
#start = 1
#for(i in start:length(stages)){
#  ## Get sample
#  samp <- genera[which(genera$stage == stages[i]),]
#  if(nrow(samp)>0){
#    ## Run traybake - standardise separately
#    tray <- traybake(samp, xy = c("cellx_50km", "celly_50km"), uniqID = "cell_50km", r = radius, standardiseCells = F, exhaustClusters = F, nSite = nCells, nBatch = nBatch, n.cores = 8)
#    ## If not null, proceed
#    if(!is.null(tray)){
#      ## Save output
#      saveRDS(tray, paste0(output.dir, "/genera_stage", stages[i], "_", nCells, "GridCells.Rds"))
#    }
#  }
#}

##### Genera - 3 cells, 50km cells, 100km radius: richness estimates from occurrences standardised without replacement #####
#cluster_3cell <- read.csv("data/analysis_data/genera_3cell_stdOccs_noRepl.csv", row.names = 1)

## Set number of occurrences to be drawn as minimum
nOccs <- (nCells*5)

## Update stages
## For each stage, load data, summarise across clusters
start = 1
for(i in start:length(stages)){
  ## try reading in data -
  tray <- suppressWarnings(tryCatch(readRDS(paste0(output.dir, "/genera_stage", stages[i], "_", nCells, "GridCells.Rds")), error = function(e){}))
  if(is.null(tray)){
    next
  } else {
    ## get clusters
    clusters <- 1:length(tray[[1]])
    ## for each cluster
    for(c in clusters){
      ## Get row from cluster and stage to populate
      row <- which(cluster_3cell_stdOccs_noRepl$cluster == str_flatten(c(stages[i],c),collapse = "_"))
      ## Isolate clusters
      cluster <- lapply(1:nBatch, function(x) tray[[x]][[c]])
      ## Convert clusters into vectors of data
      values <- mclapply(1:length(cluster), mc.cores = core.set, function(y){
        ## richness and covariates
        rich <- richness(data = cluster[[y]], cellID = "cell_50km", nCells = nCells, cov.names = cov.names, nDraws = nBites, nOccs = nOccs, standardiseOccs = T, standardiseOccsWithReplacement = F, splitTaxa = F, taxonName = "combined_name", taxaToTally = c("Mollusca", "Brachiopoda"), n.cores = 1)
      })
      ## Get median of richness and mean of everything else
      summary <- c()
      for(tt in 1:2){
        summary <- c(summary, median(sapply(1:length(values), function(x) values[[x]][tt])))
      }
      for(cc in 3:7){
        summary <- c(summary, mean(sapply(1:length(values), function(x) values[[x]][cc])))
      }
      ## Palaeolat is celly (summary[4]). Add absolute value to end
      summary <- c(summary, abs(summary[4]))
      ## Populate row
      cluster_3cell_stdOccs_noRepl[row, c(3:ncol(cluster_3cell_stdOccs_noRepl))] <- summary
    }
    ## Save to keep track in between stages
    write.csv(cluster_3cell_stdOccs_noRepl, file = "data/analysis_data/genera_3cell_stdOccs_noRepl.csv")
  }
}


## Add term for pre/post permian and stage midpoints
timeData <- read.csv("data/metadata/binning_timescale.csv", row.names = 1)
timeData$number <- seq(1,nrow(timeData),1)

## Add containers
cluster_3cell_stdOccs_noRepl$stage_midpoint <- NA
cluster_3cell_stdOccs_noRepl$PTME <- NA

## Stage 48 is last pre-PTME
## for each timeData
for(i in timeData$number){
  if(any(cluster_3cell_stdOccs_noRepl[,"stage"]==i)){
    cluster_3cell_stdOccs_noRepl[which(cluster_3cell_stdOccs_noRepl$stage == i),"stage_midpoint"] <- timeData[i,"midpoint"]
    if(i <= 48){
      cluster_3cell_stdOccs_noRepl[which(cluster_3cell_stdOccs_noRepl$stage == i),"PTME"] <- "PrePTME"
    } else {
      cluster_3cell_stdOccs_noRepl[which(cluster_3cell_stdOccs_noRepl$stage == i),"PTME"] <- "PostPTME"
    }
  }
}

## Export
write.csv(cluster_3cell_stdOccs_noRepl, file = "data/analysis_data/genera_3cell_stdOccs_noRepl.csv")

##### Genera - 3 cells, 50km cells, 100km radius: richness estimates from occurrences standardised with replacement #####
## Set number of occurrences to be drawn as minimum
nOccs <- 50

## Update stages
## For each stage, load data, summarise across clusters
start = 1
for(i in start:length(stages)){
  ## try reading in data -
  tray <- suppressWarnings(tryCatch(readRDS(paste0(output.dir, "/genera_stage", stages[i], "_", nCells, "GridCells.Rds")), error = function(e){}))
  if(is.null(tray)){
    next
  } else {
    ## get clusters
    clusters <- 1:length(tray[[1]])
    ## for each cluster
    for(c in clusters){
      ## Get row from cluster and stage to populate
      row <- which(cluster_3cell_stdOccs_noRepl$cluster == str_flatten(c(stages[i],c),collapse = "_"))
      ## Isolate clusters
      cluster <- lapply(1:nBatch, function(x) tray[[x]][[c]])
      ## Convert clusters into vectors of data
      values <- mclapply(1:length(cluster), mc.cores = core.set, function(y){
        ## richness and covariates
        rich <- richness(data = cluster[[y]], cellID = "cell_50km", nCells = nCells, cov.names = cov.names, nDraws = nBites, nOccs = nOccs, standardiseOccs = T, standardiseOccsWithReplacement = T, splitTaxa = T, taxonName = "combined_name", taxaToTally = c("Mollusca", "Brachiopoda"), n.cores = 1)
      })
      ## Get median of richness and mean of everything else
      summary <- c()
      for(tt in 1:2){
        summary <- c(summary, median(sapply(1:length(values), function(x) values[[x]][tt])))
      }
      for(cc in 3:7){
        summary <- c(summary, mean(sapply(1:length(values), function(x) values[[x]][cc])))
      }
      ## Palaeolat is celly (summary[4]). Add absolute value to end
      summary <- c(summary, abs(summary[4]))
      ## Populate row
      cluster_3cell_stdOccs_Repl[row, c(3:ncol(cluster_3cell_stdOccs_Repl))] <- summary
    }
    ## Save to keep track in between stages
    write.csv(cluster_3cell_stdOccs_Repl, file = "data/analysis_data/genera_3cell_stdOccs_Repl.csv")
  }
}

## Add term for pre/post permian and stage midpoints
timeData <- read.csv("data/metadata/binning_timescale.csv", row.names = 1)
timeData$number <- seq(1,nrow(timeData),1)

## Add containers
cluster_3cell_stdOccs_Repl$stage_midpoint <- NA
cluster_3cell_stdOccs_Repl$PTME <- NA

## Stage 48 is last pre-PTME
## for each timeData
for(i in timeData$number){
  if(any(cluster_3cell_stdOccs_Repl[,"stage"]==i)){
    cluster_3cell_stdOccs_Repl[which(cluster_3cell_stdOccs_Repl$stage == i),"stage_midpoint"] <- timeData[i,"midpoint"]
    if(i <= 48){
      cluster_3cell_stdOccs_Repl[which(cluster_3cell_stdOccs_Repl$stage == i),"PTME"] <- "PrePTME"
    } else {
      cluster_3cell_stdOccs_Repl[which(cluster_3cell_stdOccs_Repl$stage == i),"PTME"] <- "PostPTME"
    }
  }
}

## Export
write.csv(cluster_3cell_stdOccs_Repl, file = "data/analysis_data/genera_3cell_stdOccs_Repl.csv")

##### Genera - 3 cells, 50km cells, 100km radius: richness estimates from raw occurrences #####
## For each stage, load data, summarise across clusters
start = 1
for(i in start:length(stages)){
  ## try reading in data -
  tray <- suppressWarnings(tryCatch(readRDS(paste0(output.dir, "/genera_stage", stages[i], "_", nCells, "GridCells.Rds")), error = function(e){}))
  if(is.null(tray)){
    next
  } else {
    ## get clusters
    clusters <- 1:length(tray[[1]])
    ## for each cluster
    for(c in clusters){
      ## Get row from cluster and stage to populate
      row <- which(cluster_3cell_stdOccs_noRepl$cluster == str_flatten(c(stages[i],c),collapse = "_"))
      ## Isolate clusters
      cluster <- lapply(1:nBatch, function(x) tray[[x]][[c]])
      ## Convert clusters into vectors of data
      values <- mclapply(1:length(cluster), mc.cores = core.set, function(y){
        ## richness and covariates
        rich <- richness(data = cluster[[y]], cellID = "cell_50km", nCells = nCells, cov.names = cov.names, nDraws = nBites, nOccs = nOccs, standardiseOccs = F, standardiseOccsWithReplacement = F, splitTaxa = F, taxonName = "combined_name", taxaToTally = c("Mollusca", "Brachiopoda"), n.cores = 1)
      })
      ## Get median of richness and mean of everything else
      summary <- c()
      for(tt in 1:2){
        summary <- c(summary, median(sapply(1:length(values), function(x) values[[x]][tt])))
      }
      for(cc in 3:7){
        summary <- c(summary, mean(sapply(1:length(values), function(x) values[[x]][cc])))
      }
      ## Palaeolat is celly (summary[4]). Add absolute value to end
      summary <- c(summary, abs(summary[4]))
      ## Populate row
      cluster_3cell_raw[row, c(3:ncol(cluster_3cell_raw))] <- summary
    }
    ## Save to keep track in between stages
    write.csv(cluster_3cell_raw, file = "data/analysis_data/genera_3cell_raw.csv")
  }
}

## Add term for pre/post permian and stage midpoints
timeData <- read.csv("data/metadata/binning_timescale.csv", row.names = 1)
timeData$number <- seq(1,nrow(timeData),1)

## Add containers
cluster_3cell_raw$stage_midpoint <- NA
cluster_3cell_raw$PTME <- NA

## Stage 48 is last pre-PTME
## for each timeData
for(i in timeData$number){
  if(any(cluster_3cell_raw[,"stage"]==i)){
    cluster_3cell_raw[which(cluster_3cell_raw$stage == i),"stage_midpoint"] <- timeData[i,"midpoint"]
    if(i <= 48){
      cluster_3cell_raw[which(cluster_3cell_raw$stage == i),"PTME"] <- "PrePTME"
    } else {
      cluster_3cell_raw[which(cluster_3cell_raw$stage == i),"PTME"] <- "PostPTME"
    }
  }
}

## Export
write.csv(cluster_3cell_raw, file = "data/analysis_data/genera_3cell_raw.csv")

##### Genera - 4 cells, 50km cells, 100km radius: creating frame #####
## Read in genera count
genera_count <- read.csv("data/sensitivity_testing/genera_50_radius_nSite_sensitivity_testing.csv", header = T, row.names = 1)

## Set number of cells
nCells <- 4

## Identify relevant data from results of sensivity analysis
rel <- genera_count[intersect(which(genera_count$grid_cells == nCells), which(genera_count$radius == radius)),]

## Inititlise cluster_4cell vector
cluster_4cell <- data.frame("stage"=NA, "cluster"=NA)
for(r in 1:nrow(rel)){
  ## Get new index
  if(rel[r,"nClusters"]>0){
    stage <- rep(rel[r,"stages"],rel[r,"nClusters"])
    clus <- paste0(rel[r,"stages"], "_", seq(1,rel[r,"nClusters"],1))
    new <- cbind("stage"=stage,"cluster"=clus)
    cluster_4cell <- rbind(cluster_4cell,new)
  }
}
cluster_4cell <- cluster_4cell[-1,]

## Add columns to be populated to finalise template output
cluster_4cell$bivalve <- NA
cluster_4cell$brachiopod <- NA
cluster_4cell$cookiePalaeoLng <- NA
cluster_4cell$cookiePalaeoLat <- NA
cluster_4cell$cookieLith <- NA
cluster_4cell$cookieBath <- NA
cluster_4cell$cookieReef <- NA
cluster_4cell$cookieAbsLat <- NA

## Create three outputs
cluster_4cell_raw <- cluster_4cell
cluster_4cell_stdOccs_noRepl <- cluster_4cell
cluster_4cell_stdOccs_Repl <- cluster_4cell

## Get stages from cluster_4cell
stages <- as.character(sort(as.numeric(unique(cluster_4cell[,"stage"]))))

##### Genera - 4 cells, 50km cells, 100km radius: spatial subsampling #####
## For each stage, run traybake
#start = 1
#for(i in start:length(stages)){
#  ## Get sample
#  samp <- genera[which(genera$stage == stages[i]),]
#  if(nrow(samp)>0){
#    ## Run traybake - standardise separately
#    tray <- traybake(samp, xy = c("cellx_50km", "celly_50km"), uniqID = "cell_50km", r = radius, standardiseCells = F, exhaustClusters = F, nSite = nCells, nBatch = nBatch, n.cores = 8)
#    ## If not null, proceed
#    if(!is.null(tray)){
#      ## Save output
#      saveRDS(tray, paste0(output.dir, "/genera_stage", stages[i], "_", nCells, "GridCells.Rds"))
#    }
#  }
#}

##### Genera - 4 cells, 50km cells, 100km radius: richness estimates from occurrences standardised without replacement #####
#cluster_4cell <- read.csv("data/analysis_data/genera_4cell_stdOccs_noRepl.csv", row.names = 1)

## Set number of occurrences to be drawn as minimum
nOccs <- (nCells*5)

## Update stages
## For each stage, load data, summarise across clusters
start = 1
for(i in start:length(stages)){
  ## try reading in data -
  tray <- suppressWarnings(tryCatch(readRDS(paste0(output.dir, "/genera_stage", stages[i], "_", nCells, "GridCells.Rds")), error = function(e){}))
  if(is.null(tray)){
    next
  } else {
    ## get clusters
    clusters <- 1:length(tray[[1]])
    ## for each cluster
    for(c in clusters){
      ## Get row from cluster and stage to populate
      row <- which(cluster_4cell_stdOccs_noRepl$cluster == str_flatten(c(stages[i],c),collapse = "_"))
      ## Isolate clusters
      cluster <- lapply(1:nBatch, function(x) tray[[x]][[c]])
      ## Convert clusters into vectors of data
      values <- mclapply(1:length(cluster), mc.cores = core.set, function(y){
        ## richness and covariates
        rich <- richness(data = cluster[[y]], cellID = "cell_50km", nCells = nCells, cov.names = cov.names, nDraws = nBites, nOccs = nOccs, standardiseOccs = T, standardiseOccsWithReplacement = F, splitTaxa = F, taxonName = "combined_name", taxaToTally = c("Mollusca", "Brachiopoda"), n.cores = 1)
      })
      ## Get median of richness and mean of everything else
      summary <- c()
      for(tt in 1:2){
        summary <- c(summary, median(sapply(1:length(values), function(x) values[[x]][tt])))
      }
      for(cc in 3:7){
        summary <- c(summary, mean(sapply(1:length(values), function(x) values[[x]][cc])))
      }
      ## Palaeolat is celly (summary[4]). Add absolute value to end
      summary <- c(summary, abs(summary[4]))
      ## Populate row
      cluster_4cell_stdOccs_noRepl[row, c(3:ncol(cluster_4cell_stdOccs_noRepl))] <- summary
    }
    ## Save to keep track in between stages
    write.csv(cluster_4cell_stdOccs_noRepl, file = "data/analysis_data/genera_4cell_stdOccs_noRepl.csv")
  }
}


## Add term for pre/post permian and stage midpoints
timeData <- read.csv("data/metadata/binning_timescale.csv", row.names = 1)
timeData$number <- seq(1,nrow(timeData),1)

## Add containers
cluster_4cell_stdOccs_noRepl$stage_midpoint <- NA
cluster_4cell_stdOccs_noRepl$PTME <- NA

## Stage 48 is last pre-PTME
## for each timeData
for(i in timeData$number){
  if(any(cluster_4cell_stdOccs_noRepl[,"stage"]==i)){
    cluster_4cell_stdOccs_noRepl[which(cluster_4cell_stdOccs_noRepl$stage == i),"stage_midpoint"] <- timeData[i,"midpoint"]
    if(i <= 48){
      cluster_4cell_stdOccs_noRepl[which(cluster_4cell_stdOccs_noRepl$stage == i),"PTME"] <- "PrePTME"
    } else {
      cluster_4cell_stdOccs_noRepl[which(cluster_4cell_stdOccs_noRepl$stage == i),"PTME"] <- "PostPTME"
    }
  }
}

## Export
write.csv(cluster_4cell_stdOccs_noRepl, file = "data/analysis_data/genera_4cell_stdOccs_noRepl.csv")

##### Genera - 4 cells, 50km cells, 100km radius: richness estimates from occurrences standardised with replacement #####
## Set number of occurrences to be drawn as minimum
nOccs <- 50

## Update stages
## For each stage, load data, summarise across clusters
start = 1
for(i in start:length(stages)){
  ## try reading in data -
  tray <- suppressWarnings(tryCatch(readRDS(paste0(output.dir, "/genera_stage", stages[i], "_", nCells, "GridCells.Rds")), error = function(e){}))
  if(is.null(tray)){
    next
  } else {
    ## get clusters
    clusters <- 1:length(tray[[1]])
    ## for each cluster
    for(c in clusters){
      ## Get row from cluster and stage to populate
      row <- which(cluster_4cell_stdOccs_noRepl$cluster == str_flatten(c(stages[i],c),collapse = "_"))
      ## Isolate clusters
      cluster <- lapply(1:nBatch, function(x) tray[[x]][[c]])
      ## Convert clusters into vectors of data
      values <- mclapply(1:length(cluster), mc.cores = core.set, function(y){
        ## richness and covariates
        rich <- richness(data = cluster[[y]], cellID = "cell_50km", nCells = nCells, cov.names = cov.names, nDraws = nBites, nOccs = nOccs, standardiseOccs = T, standardiseOccsWithReplacement = T, splitTaxa = T, taxonName = "combined_name", taxaToTally = c("Mollusca", "Brachiopoda"), n.cores = 1)
      })
      ## Get median of richness and mean of everything else
      summary <- c()
      for(tt in 1:2){
        summary <- c(summary, median(sapply(1:length(values), function(x) values[[x]][tt])))
      }
      for(cc in 3:7){
        summary <- c(summary, mean(sapply(1:length(values), function(x) values[[x]][cc])))
      }
      ## Palaeolat is celly (summary[4]). Add absolute value to end
      summary <- c(summary, abs(summary[4]))
      ## Populate row
      cluster_4cell_stdOccs_Repl[row, c(3:ncol(cluster_4cell_stdOccs_Repl))] <- summary
    }
    ## Save to keep track in between stages
    write.csv(cluster_4cell_stdOccs_Repl, file = "data/analysis_data/genera_4cell_stdOccs_Repl.csv")
  }
}

## Add term for pre/post permian and stage midpoints
timeData <- read.csv("data/metadata/binning_timescale.csv", row.names = 1)
timeData$number <- seq(1,nrow(timeData),1)

## Add containers
cluster_4cell_stdOccs_Repl$stage_midpoint <- NA
cluster_4cell_stdOccs_Repl$PTME <- NA

## Stage 48 is last pre-PTME
## for each timeData
for(i in timeData$number){
  if(any(cluster_4cell_stdOccs_Repl[,"stage"]==i)){
    cluster_4cell_stdOccs_Repl[which(cluster_4cell_stdOccs_Repl$stage == i),"stage_midpoint"] <- timeData[i,"midpoint"]
    if(i <= 48){
      cluster_4cell_stdOccs_Repl[which(cluster_4cell_stdOccs_Repl$stage == i),"PTME"] <- "PrePTME"
    } else {
      cluster_4cell_stdOccs_Repl[which(cluster_4cell_stdOccs_Repl$stage == i),"PTME"] <- "PostPTME"
    }
  }
}

## Export
write.csv(cluster_4cell_stdOccs_Repl, file = "data/analysis_data/genera_4cell_stdOccs_Repl.csv")

##### Genera - 4 cells, 50km cells, 100km radius: richness estimates from raw occurrences #####
## For each stage, load data, summarise across clusters
start = 1
for(i in start:length(stages)){
  ## try reading in data -
  tray <- suppressWarnings(tryCatch(readRDS(paste0(output.dir, "/genera_stage", stages[i], "_", nCells, "GridCells.Rds")), error = function(e){}))
  if(is.null(tray)){
    next
  } else {
    ## get clusters
    clusters <- 1:length(tray[[1]])
    ## for each cluster
    for(c in clusters){
      ## Get row from cluster and stage to populate
      row <- which(cluster_4cell_stdOccs_noRepl$cluster == str_flatten(c(stages[i],c),collapse = "_"))
      ## Isolate clusters
      cluster <- lapply(1:nBatch, function(x) tray[[x]][[c]])
      ## Convert clusters into vectors of data
      values <- mclapply(1:length(cluster), mc.cores = core.set, function(y){
        ## richness and covariates
        rich <- richness(data = cluster[[y]], cellID = "cell_50km", nCells = nCells, cov.names = cov.names, nDraws = nBites, nOccs = nOccs, standardiseOccs = F, standardiseOccsWithReplacement = F, splitTaxa = F, taxonName = "combined_name", taxaToTally = c("Mollusca", "Brachiopoda"), n.cores = 1)
      })
      ## Get median of richness and mean of everything else
      summary <- c()
      for(tt in 1:2){
        summary <- c(summary, median(sapply(1:length(values), function(x) values[[x]][tt])))
      }
      for(cc in 3:7){
        summary <- c(summary, mean(sapply(1:length(values), function(x) values[[x]][cc])))
      }
      ## Palaeolat is celly (summary[4]). Add absolute value to end
      summary <- c(summary, abs(summary[4]))
      ## Populate row
      cluster_4cell_raw[row, c(3:ncol(cluster_4cell_raw))] <- summary
    }
    ## Save to keep track in between stages
    write.csv(cluster_4cell_raw, file = "data/analysis_data/genera_4cell_raw.csv")
  }
}

## Add term for pre/post permian and stage midpoints
timeData <- read.csv("data/metadata/binning_timescale.csv", row.names = 1)
timeData$number <- seq(1,nrow(timeData),1)

## Add containers
cluster_4cell_raw$stage_midpoint <- NA
cluster_4cell_raw$PTME <- NA

## Stage 48 is last pre-PTME
## for each timeData
for(i in timeData$number){
  if(any(cluster_4cell_raw[,"stage"]==i)){
    cluster_4cell_raw[which(cluster_4cell_raw$stage == i),"stage_midpoint"] <- timeData[i,"midpoint"]
    if(i <= 48){
      cluster_4cell_raw[which(cluster_4cell_raw$stage == i),"PTME"] <- "PrePTME"
    } else {
      cluster_4cell_raw[which(cluster_4cell_raw$stage == i),"PTME"] <- "PostPTME"
    }
  }
}

## Export
write.csv(cluster_4cell_raw, file = "data/analysis_data/genera_4cell_raw.csv")


