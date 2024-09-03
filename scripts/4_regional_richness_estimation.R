## 4. Species richness estimation of spatial regions
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("parallel", "velociraptr", "dplyr", "plyr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(parallel)
library(velociraptr)
library(dplyr)
library(plyr)

## Set working directory
setwd("~/R_packages/bivbrach")
home <- getwd()

## Load variable vectors
radii <- as.integer(c(200000, 500000, 1000000))
siteQuotas <- c(2, 3, 4, 5)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))

## Set input strings
input.strings <- c("stages_g200_viaTimeBin",
                   "stages_g100_viaTimeBin",
                   "stages_s200_viaTimeBin",
                   "stages_s100_viaTimeBin",
                   "stages_g200_epif_viaTimeBin",
                   "stages_g100_epif_viaTimeBin",
                   "stages_s200_epif_viaTimeBin",
                   "stages_s100_epif_viaTimeBin",
                   "stages_g200_inf_viaTimeBin",
                   "stages_g100_inf_viaTimeBin",
                   "stages_s200_inf_viaTimeBin",
                   "stages_s100_inf_viaTimeBin",
                   "stages_g200_sitesThenRefs_VTBO",
                   "stages_g100_sitesThenRefs_VTBO",
                   "stages_s200_sitesThenRefs_VTBO",
                   "stages_s100_sitesThenRefs_VTBO")

## Set output strings
output.strings <- c("stages_g200",
                    "stages_g100",
                    "stages_s200",
                    "stages_s100",
                    "stages_g200_epif",
                    "stages_g100_epif",
                    "stages_s200_epif",
                    "stages_s100_epif",
                    "stages_g200_inf",
                    "stages_g100_inf",
                    "stages_s200_inf",
                    "stages_s100_inf",
                    "stages_g200_sitesThenRefs",
                    "stages_g100_sitesThenRefs",
                    "stages_s200_sitesThenRefs",
                    "stages_s100_sitesThenRefs")

## Set taxonomic variable string
taxVar.strings <- rep(c(rep("genus", 2), rep("unique_name", 2)),4)

## Set input, and output directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_spaSub"
output.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
source("functions/get.regional.richness.R")

## get regional richness arguments
a = 1
input.dir = input.dir
input.pre = input.strings[a]
output.dir = output.dir
output.pre = output.strings[a]
vars = vars
taxa = T
n.cores = 4
taxVar = taxVar.strings[a]
d = 1
cov.cols <- c("cellLith", "cellEnv", "cellReef", "cellY")
cov.types <- c("categorical", "categorical", "categorical", "continuous")
cov.names <- c("sampLith", "sampEnv", "sampReef", "sampLat")
cell.col <- "cell"

get.regional.richness <- function(input.dir, input.pre, output.dir, output.pre, vars, cov.cols, cov.types, cov.names, cell.col, taxa = T, n.cores = 1, taxVar = "genus"){
  ## set up inputs
  combin <- expand.grid(vars)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".Rds")
  output.dirs <- paste0(output.dir, "/", output.pre, "_", varStrings, ".csv")
  if(taxa){
    ## for each input.dirs
    for(d in 1:length(input.dirs)){
      print(d)
      ## read in data
      data <- suppressWarnings(tryCatch(readRDS(input.dirs[d]), error = function(e){}))
      ## if data is NULL, skip to next
      if(is.null(data)){
        next
      } else {
        taxa.labels <- names(data)
        time.labels <- names(data[[1]])
        cookie.counts <- lapply(1:length(data[[1]]), function(t) length(data[[1]][[t]]))
        ## get counts of taxa
        output <- mclapply(1:length(data[[1]]), mc.cores = n.cores, function(t){
          per.taxon <- sapply(1:length(data), function(b){
            per.cookie <- sapply(1:length(data[[b]][[t]]), function(c){
              per.rep <- sapply(1:length(data[[b]][[t]][[c]]), function(r){
                if(nrow(data[[b]][[t]][[c]][[r]]) > 0){
                  out <- length(unique(data[[b]][[t]][[c]][[r]][,taxVar]))
                } else {
                  out <- 0
                }
                return(out)
              })
            })
          })
          colnames(per.taxon) <- taxa.labels
          return(per.taxon)
        })
        ## get times column
        times.out <- c()
        for(i in 1:length(time.labels)){
          if(length(output[[i]]) > 2){
            times.out <- c(times.out,rep(time.labels[i],nrow(output[[i]])))
          } else {
            times.out <- c(times.out, time.labels[i])
          }
        }
        ## get cookie.id column
        cookies.out <- c()
        for(t in 1:length(time.labels)){
          if(length(output[[t]]) > 2){
            cookie.labels <- paste0(time.labels[t], ".", seq(1,cookie.counts[[t]],1))
            for(c in 1:length(cookie.labels)){
              cookies.out <- c(cookies.out, rep(cookie.labels[c], nrow(output[[t]])/cookie.counts[[t]]))
            }
          } else {
            cookies.out <- c(cookies.out, paste0(time.labels[t], ".", seq(1,cookie.counts[[t]],1)))
          }
        }
        ## Now rbind output
        output <- do.call(rbind, output)
        ## get covariates
        ## as taxa separated, recombine
        cov.data <- mclapply(1:length(data[[1]]), mc.cores = n.cores, function(t){
            per.cookie <- lapply(1:length(data[[1]][[t]]), function(c){
              per.rep <- lapply(1:length(data[[1]][[t]][[c]]), function(r){
                out <- rbind(data[[1]][[t]][[c]][[r]],data[[2]][[t]][[c]][[r]])
              })
          })
        })
        ## then get output covariates
        cov.out <- mclapply(1:length(cov.data), mc.cores = n.cores, function(t){
          per.cookie <- lapply(1:length(cov.data[[t]]), function(c){
            per.rep <- t(sapply(1:length(cov.data[[t]][[c]]), function(r){
              covs <- distinct(cov.data[[t]][[c]][[r]][,c(cell.col,cov.cols)])[,-1]
              sampCov <- c()
              for(i in 1:ncol(covs)){
                if(cov.types[i] == "categorical"){
                  if(length(unique(covs[,i])) == 1){
                    sampCov <- c(sampCov, as.character(unique(covs[,i])))
                  } else {
                    sampCov <- c(sampCov, "mix")
                  }
                } else {
                  if(cov.types[i] == "continuous"){
                    sampCov <- c(sampCov, mean(covs[,i]))
                  }
                }
              }
              names(sampCov) <- cov.names
              return(sampCov)
            }))
          })
          per.cookie <- do.call(rbind, per.cookie)
          return(per.cookie)
        })
        cov.out <- do.call(rbind, cov.out)
        ## reformat outputs
        output.mat <- cbind(as.numeric(times.out), cookies.out, cov.out, output)
        colnames(output.mat) <- c("times", "source.subregion.ID", cov.names, taxa.labels)
        rownames(output.mat) <- NULL
        ## Save output here
        write.csv(output.mat, output.dirs[d])
      }
    }
  } else {
    ## for each input.dirs
    for(d in 1:length(input.dirs)){
      ## read in data
      data <- suppressWarnings(tryCatch(readRDS(input.dirs[d]), error = function(e){}))
      ## if data is NULL, skip to next
      if(is.null(data)){
        next
      } else {
        time.labels <- names(data)
        cookie.counts <- lapply(1:length(data), function(t) length(data[[t]]))
        ## get counts of taxa
        output <- mclapply(1:length(data), mc.cores = n.cores, function(t){
          per.cookie <- as.vector(sapply(1:length(data[[t]]), function(c){
            per.rep <- sapply(1:length(data[[t]][[c]]), function(r){
              if(nrow(data[[t]][[c]][[r]]) > 0){
                out <- length(unique(data[[t]][[c]][[r]][,taxVar]))
              } else {
                out <- 0
              }
              return(out)
            })
          }))
        })
        names(output) <- time.labels
        ## get times column
        times.out <- c()
        for(i in 1:length(time.labels)){
          if(length(output[[i]]) > 1){
            times.out <- c(times.out,rep(time.labels[i],length(output[[i]])))
          } else {
            times.out <- c(times.out, time.labels[i])
          }
        }
        ## get cookie.id column
        cookies.out <- c()
        for(t in 1:length(time.labels)){
          if(length(output[[t]]) > 1){
            cookie.labels <- paste0(time.labels[t], ".", seq(1,cookie.counts[[t]],1))
            for(c in 1:length(cookie.labels)){
              cookies.out <- c(cookies.out, rep(cookie.labels[c], length(output[[t]])/cookie.counts[[t]]))
            }
          } else {
            cookies.out <- c(cookies.out, paste0(time.labels[t], ".", seq(1,cookie.counts[[t]],1)))
          }
        }
        ## get covariates
        cov.out <- mclapply(1:length(data), mc.cores = n.cores, function(t){
          per.cookie <- lapply(1:length(data[[t]]), function(c){
            per.rep <- t(sapply(1:length(data[[t]][[c]]), function(r){
              covs <- distinct(data[[t]][[c]][[r]][,c(cell.col,cov.cols)])[,-1]
              sampCov <- c()
              for(i in 1:ncol(covs)){
                if(cov.type[i] == "categorical"){
                  if(length(unique(covs[,i])) == 1){
                    sampCov <- c(sampCov, as.character(unique(covs[,i])))
                  } else {
                    sampCov <- c(sampCov, "mix")
                  }
                } else {
                  if(cov.type[i] == "continuous"){
                    sampCov <- c(sampCov, mean(covs[,i]))
                  }
                }
              }
              names(sampCov) <- cov.names
              return(sampCov)
            }))
          })
          per.cookie <- do.call(rbind, per.cookie)
          return(per.cookie)
        })
        cov.out <- do.call(rbind, cov.out)
        ## reformat outputs
        output.mat <- cbind(as.numeric(times.out), cookies.out, cov.out, unlist(output))
        colnames(output.mat) <- c("times", "source.subregion.ID", cov.names, paste0(taxVar, ".count"))
        rownames(output.mat) <- NULL
        ## Save output here
        write.csv(output.mat, output.dirs[d])
      }
    }
  }
}


## Calculate richness
for(a in 1:length(input.strings)){
  get.regional.richness(input.dir = input.dir, input.pre = input.strings[a], output.dir = output.dir, output.pre = output.strings[a],
                        vars = vars, cov.cols = c("cellLith", "cellEnv", "cellReef", "cellY"),
                        cov.types = c("categorical", "categorical", "categorical", "continuous"),
                        cov.names = c("sampLith", "sampEnv", "sampReef", "sampLat"), cell.col = "cell",
                        taxa = T, n.cores = 4, taxVar = taxVar.strings[a])
}


#### DEFUNCT Plotting bivalve versus brachiopod richness ####
## Using sjplot now
## Define directories
#input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
#output.dir <- "/Users/tjs/R_packages/bivbrach/figures/richness"

## Define variables and labels
#radii <- as.integer(c(200000, 500000, 1000000))
#siteQuotas <- c(2, 3, 4, 5)
#vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
#vars.label <- list(paste0(seq(2, 5, 1), " site minima"), paste0(c(200, 500, 1000), "km radius"))

## Define colour palette for periods
#periods <- downloadTime("international periods")
#periods <- periods[order(periods$b_age, decreasing=TRUE), ]
#periods <- periods[periods$b_age <= periods[which(rownames(periods) == "Cambrian"),"b_age"],c(2,4,5,8)]

## Set legend position
#legend.position = c("topright", "topright", "topright", "topright")

## Define geo.scale object, which specifies colours and point shapes for different time bins
#geo.scale <- cbind(periods, "shape" = c(rep(16, 6), rep(15, 3), rep(17, 3)))

## Set names of plotting columns and columns containing time data
#plotting.col <- c("Bivalvia", "Brachiopoda")
#times.col <- "times"

## Define input and output strings
#input.pre <- output.pre <- c("stages_g200",
#                  "stages_g100",
#                  "stages_s200",
#                  "stages_s100")

#output.title <- c("Genera, 200km grid cells, STO rarefaction",
#                      "Genera, 100km grid cells, STO rarefaction",
#                      "Species, 200km grid cells, STO rarefaction",
#                      "Species, 100km grid cells, STO rarefaction")

## Read in function
#source("functions/plot.richness.R")

## Run plotting function
#for(a in 1:length(input.pre)){
#  plot.richness(input.dir, input.pre = input.pre[a], output.dir, output.pre = output.pre[a], output.title = output.title[a],
#                vars, vars.label, plotting.col, times.col, geo.scale, legend.position = legend.position[a])
#}
