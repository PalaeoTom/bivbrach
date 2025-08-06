## S10 trimming out grid cells with a RC slope of more than 0.05
## Started by TJS on 08/01/2024

## If packages aren't installed, install them, then load them
packages <- c("divvy", "stringr", "fossilbrush", "divDyn", "ggplot2", "rnaturalearth", "rnaturalearthdata")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divvy)
library(stringr)
library(fossilbrush)
library(divDyn)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

## Clean directory
rm(list = ls())

#### Slope of 0.05 or less ####
rm(list=ls())

## Read it in if starting from scratch
master_50 <- readRDS("data/final/master_50_2_2_min3gen_min20occs.Rds")
master_100 <- readRDS("data/final/master_100_2_2_min3gen_min20occs.Rds")
master_150 <- readRDS("data/final/master_150_2_2_min3gen_min20occs.Rds")
master_200 <- readRDS("data/final/master_200_2_2_min3gen_min20occs.Rds")

## Read in rarefaction curves
master_50_RCs <- readRDS("data/sensitivity_testing/master_50_2_2_min3gen_min20occs_RCs.Rds")
master_100_RCs <- readRDS("data/sensitivity_testing/master_100_2_2_min3gen_min20occs_RCs.Rds")
master_150_RCs <- readRDS("data/sensitivity_testing/master_150_2_2_min3gen_min20occs_RCs.Rds")
master_200_RCs <- readRDS("data/sensitivity_testing/master_200_2_2_min3gen_min20occs_RCs.Rds")

## Wittle down to grid cells that meet asymptote criteria
asymptote.occs <- 5
slope.threshold <- 0.05
source("functions/test_RC_tail_asymptote.R")
master_50_RCs_bool <- test_RC_tail_asymptote(RCs = master_50_RCs[,-1], n = asymptote.occs, threshold = slope.threshold)
master_100_RCs_bool <- test_RC_tail_asymptote(RCs = master_100_RCs[,-1], n = asymptote.occs, threshold = slope.threshold)
master_150_RCs_bool <- test_RC_tail_asymptote(RCs = master_150_RCs[,-1], n = asymptote.occs, threshold = slope.threshold)
master_200_RCs_bool <- test_RC_tail_asymptote(RCs = master_200_RCs[,-1], n = asymptote.occs, threshold = slope.threshold)

## Trim grid cells from RCs, count grid cells, and count occurrences
master_50_GCs <- colnames(master_50_RCs)[-1]
master_50_GCs <- master_50_GCs[master_50_RCs_bool]
length(master_50_GCs)
occs_50 <- length(master_50[which(master_50$stage_cell %in% master_50_GCs),"stage_cell"])
occs_50

master_100_GCs <- colnames(master_100_RCs)[-1]
master_100_GCs <- master_100_GCs[master_100_RCs_bool]
length(master_100_GCs)
occs_100 <- length(master_100[which(master_100$stage_cell %in% master_100_GCs),"stage_cell"])
occs_100

master_150_GCs <- colnames(master_150_RCs)[-1]
master_150_GCs <- master_150_GCs[master_150_RCs_bool]
length(master_150_GCs)
occs_150 <- length(master_150[which(master_150$stage_cell %in% master_150_GCs),"stage_cell"]) ## 452,574 occurrences
occs_150

master_200_GCs <- colnames(master_200_RCs)[-1]
master_200_GCs <- master_200_GCs[master_200_RCs_bool] ## 645 grid cells
length(master_200_GCs)
occs_200 <- length(master_200[which(master_200$stage_cell %in% master_200_GCs),"stage_cell"]) ## 460,786 occurrences
occs_200

## Get final versions of dataset
master_50_final <- master_50[which(master_50$stage_cell %in% master_50_GCs),]
master_100_final <- master_100[which(master_100$stage_cell %in% master_100_GCs),]
master_150_final <- master_150[which(master_150$stage_cell %in% master_150_GCs),]
master_200_final <- master_200[which(master_200$stage_cell %in% master_200_GCs),]

## Assessing grid cell quality
## Isolate cells and coordinates
master_50_final_uniqGC <- master_50_final[,c(41,42,44)]
master_100_final_uniqGC <- master_100_final[,c(41,42,44)]
master_150_final_uniqGC <- master_150_final[,c(41,42,44)]
master_200_final_uniqGC <- master_200_final[,c(41,42,44)]

## Drop duplicated rows
master_50_final_uniqGC <- master_50_final[which(!duplicated(master_50_final_uniqGC)),]
master_100_final_uniqGC <- master_100_final[which(!duplicated(master_100_final_uniqGC)),]
master_150_final_uniqGC <- master_150_final[which(!duplicated(master_150_final_uniqGC)),]
master_200_final_uniqGC <- master_200_final[which(!duplicated(master_200_final_uniqGC)),]

## Read in functions to check stages and cells
source("functions/stage.check.R")
source("functions/cov.check.R")

## Run functions - first, get stages
length(stage.check(master_50_final_uniqGC, output = "stages"))
length(stage.check(master_100_final_uniqGC, output = "stages"))
length(stage.check(master_150_final_uniqGC, output = "stages"))
length(stage.check(master_200_final_uniqGC, output = "stages"))

## Then get grid cells
length(stage.check(master_50_final_uniqGC, output = "cells"))
length(stage.check(master_100_final_uniqGC, output = "cells"))
length(stage.check(master_150_final_uniqGC, output = "cells"))
length(stage.check(master_200_final_uniqGC, output = "cells"))

##Get occurrences associated with these grid cells
master_50_final_min2cellStage <- master_50_final[which(master_50_final$stage_cell %in% stage.check(master_50_final_uniqGC, output = "cells")),]
master_100_final_min2cellStage <- master_100_final[which(master_100_final$stage_cell %in% stage.check(master_100_final_uniqGC, output = "cells")),]
master_150_final_min2cellStage <- master_150_final[which(master_150_final$stage_cell %in% stage.check(master_150_final_uniqGC, output = "cells")),]
master_200_final_min2cellStage <- master_200_final[which(master_200_final$stage_cell %in% stage.check(master_200_final_uniqGC, output = "cells")),]
nrow(master_50_final_min2cellStage)
nrow(master_100_final_min2cellStage)
nrow(master_150_final_min2cellStage)
nrow(master_200_final_min2cellStage)

## Individual covariates
length(cov.check(data = master_50_final_uniqGC, covVar = "cellLith"))
length(cov.check(data = master_100_final_uniqGC, covVar = "cellLith"))
length(cov.check(data = master_150_final_uniqGC, covVar = "cellLith"))
length(cov.check(data = master_200_final_uniqGC, covVar = "cellLith"))

length(cov.check(data = master_50_final_uniqGC, covVar = "cellBath"))
length(cov.check(data = master_100_final_uniqGC, covVar = "cellBath"))
length(cov.check(data = master_150_final_uniqGC, covVar = "cellBath"))
length(cov.check(data = master_200_final_uniqGC, covVar = "cellBath"))

length(cov.check(data = master_50_final_uniqGC, covVar = "cellReef"))
length(cov.check(data = master_100_final_uniqGC, covVar = "cellReef"))
length(cov.check(data = master_150_final_uniqGC, covVar = "cellReef"))
length(cov.check(data = master_200_final_uniqGC, covVar = "cellReef"))

length(cov.check(data = master_50_final_uniqGC, covVar = c("cellLith", "cellBath", "cellReef")))
length(cov.check(data = master_100_final_uniqGC, covVar = c("cellLith", "cellBath", "cellReef")))
length(cov.check(data = master_150_final_uniqGC, covVar = c("cellLith", "cellBath", "cellReef")))
length(cov.check(data = master_200_final_uniqGC, covVar = c("cellLith", "cellBath", "cellReef")))

## Trim dataset to get occurrences
master_50_final_covData <- master_50_final[which(master_50_final$stage_cell %in% cov.check(data = master_50_final_uniqGC, covVar = c("cellLith", "cellBath", "cellReef"))),]
master_100_final_covData <- master_100_final[which(master_100_final$stage_cell %in% cov.check(data = master_100_final_uniqGC, covVar = c("cellLith", "cellBath", "cellReef"))),]
master_150_final_covData <- master_150_final[which(master_150_final$stage_cell %in% cov.check(data = master_150_final_uniqGC, covVar = c("cellLith", "cellBath", "cellReef"))),]
master_200_final_covData <- master_200_final[which(master_200_final$stage_cell %in% cov.check(data = master_200_final_uniqGC, covVar = c("cellLith", "cellBath", "cellReef"))),]
nrow(master_50_final_covData)
nrow(master_100_final_covData)
nrow(master_150_final_covData)
nrow(master_200_final_covData)

## Get final version of dataset where each stage is represented by at least two grid cells
## First, get unique version of dataset
## Isolate cells and coordinates
master_50_final_covData_uniqGC <- master_50_final_covData[,c(41,42,44)]
master_100_final_covData_uniqGC <- master_100_final_covData[,c(41,42,44)]
master_150_final_covData_uniqGC <- master_150_final_covData[,c(41,42,44)]
master_200_final_covData_uniqGC <- master_200_final_covData[,c(41,42,44)]

## Drop duplicated rows
master_50_final_covData_uniqGC <- master_50_final_covData[which(!duplicated(master_50_final_covData_uniqGC)),]
master_100_final_covData_uniqGC <- master_100_final_covData[which(!duplicated(master_100_final_covData_uniqGC)),]
master_150_final_covData_uniqGC <- master_150_final_covData[which(!duplicated(master_150_final_covData_uniqGC)),]
master_200_final_covData_uniqGC <- master_200_final_covData[which(!duplicated(master_200_final_covData_uniqGC)),]

## Run functions - first, get stages
length(stage.check(master_50_final_covData_uniqGC, output = "stages"))
length(stage.check(master_100_final_covData_uniqGC, output = "stages"))
length(stage.check(master_150_final_covData_uniqGC, output = "stages"))
length(stage.check(master_200_final_covData_uniqGC, output = "stages"))

## Then get grid cells
length(stage.check(master_50_final_covData_uniqGC, output = "cells"))
length(stage.check(master_100_final_covData_uniqGC, output = "cells"))
length(stage.check(master_150_final_covData_uniqGC, output = "cells"))
length(stage.check(master_200_final_covData_uniqGC, output = "cells"))

##Get occurrences associated with these grid cells
master_50_final_min2cellStage_covData <- master_50_final_covData[which(master_50_final_covData$stage_cell %in% stage.check(master_50_final_covData_uniqGC, output = "cells")),]
master_100_final_min2cellStage_covData <- master_100_final_covData[which(master_100_final_covData$stage_cell %in% stage.check(master_100_final_covData_uniqGC, output = "cells")),]
master_150_final_min2cellStage_covData <- master_150_final_covData[which(master_150_final_covData$stage_cell %in% stage.check(master_150_final_covData_uniqGC, output = "cells")),]
master_200_final_min2cellStage_covData <- master_200_final_covData[which(master_200_final_covData$stage_cell %in% stage.check(master_200_final_covData_uniqGC, output = "cells")),]
nrow(master_50_final_min2cellStage_covData)
nrow(master_100_final_min2cellStage_covData)
nrow(master_150_final_min2cellStage_covData)
nrow(master_200_final_min2cellStage_covData)

## Plot generic occurrences as map
## Get world
world <- ne_countries(scale = "medium", returnclass = "sf")

## Plot occurrences
pdf(file = "figures/final/supplemental/genera_50v200_min3gen_min20occs_max0.05RCslope_OCCSmap.pdf")
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgrey")+
  ggtitle("50km (blue) versus 200km (orange) grid cells - occurrences pre-rasterisation") +
  coord_sf(crs = "epsg:4326") +
  geom_point(aes(x = longitude, y = latitude), data = master_50_final, colour = "blue", alpha = 0.3, size = 0.5) +
  geom_point(aes(x = longitude, y = latitude), data = master_200_final, colour = "orange", alpha = 0.3, size = 0.5)
dev.off()

## Plot occurrences
pdf(file = "figures/final/supplemental/genera_50v100_min3gen_min20occs_max0.05RCslope_OCCSmap.pdf")
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgrey")+
  ggtitle("50km (blue) versus 100km (violet) grid cells - occurrences pre-rasterisation") +
  coord_sf(crs = "epsg:4326") +
  geom_point(aes(x = longitude, y = latitude), data = master_50_final, colour = "blue", alpha = 0.3, size = 0.5)+
  geom_point(aes(x = longitude, y = latitude), data = master_100_final, colour = "violet", alpha = 0.3, size = 0.5)
dev.off()

## Plot occurrences
pdf(file = "figures/final/supplemental/genera_50v150_min3gen_min20occs_max0.05RCslope_OCCSmap.pdf")
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgrey")+
  ggtitle("50km (blue) versus 150km (pink) grid cells - occurrences pre-rasterisation") +
  coord_sf(crs = "epsg:4326") +
  geom_point(aes(x = longitude, y = latitude), data = master_50_final, colour = "blue", alpha = 0.3, size = 0.5) +
  geom_point(aes(x = longitude, y = latitude), data = master_150_final, colour = "pink", alpha = 0.3, size = 0.5)
dev.off()

#### Plot richness distribution amongst cells
cellOccsN50 <- as.vector(table(master_50[which(master_50$stage_cell %in% master_50_GCs),"stage_cell"]))
cellOccsN100 <- as.vector(table(master_100[which(master_100$stage_cell %in% master_100_GCs),"stage_cell"]))
cellOccsN150 <- as.vector(table(master_150[which(master_150$stage_cell %in% master_150_GCs),"stage_cell"]))
cellOccsN200 <- as.vector(table(master_200[which(master_200$stage_cell %in% master_200_GCs),"stage_cell"]))
cats <- c(rep(paste0("50km grid cells (", length(cellOccsN50), " viable grid cells, ", occs_50, " occurrences)"), times = length(cellOccsN50)),
          rep(paste0("100km grid cells (", length(cellOccsN100), " viable grid cells, ", occs_100, " occurrences)"), times = length(cellOccsN100)),
          rep(paste0("150km grid cells (", length(cellOccsN150), " viable grid cells, ", occs_150, " occurrences)"), times = length(cellOccsN150)),
          rep(paste0("200km grid cells (", length(cellOccsN200), " viable grid cells, ", occs_200, " occurrences)"), times = length(cellOccsN200)))
cats <- factor(cats, levels = c(paste0("50km grid cells (", length(cellOccsN50), " viable grid cells, ", occs_50, " occurrences)"),
                                paste0("100km grid cells (", length(cellOccsN100), " viable grid cells, ", occs_100, " occurrences)"),
                                paste0("150km grid cells (", length(cellOccsN150), " viable grid cells, ", occs_150, " occurrences)"),
                                paste0("200km grid cells (", length(cellOccsN200), " viable grid cells, ", occs_200, " occurrences)")))
occsValues <- c(cellOccsN50, cellOccsN100, cellOccsN150, cellOccsN200)
plotData <- data.frame(cbind(cats, occsValues))

## Define labels with line breaks
xlabs <- c(paste0("50km grid cells\n", length(cellOccsN50), " grid cells, ", occs_50, " occurrences"),
           paste0("100km grid cells\n", length(cellOccsN100), " grid cells, ", occs_100, " occurrences"),
           paste0("150km grid cells\n", length(cellOccsN150), " grid cells, ", occs_150, " occurrences"),
           paste0("200km grid cells\n", length(cellOccsN200), " grid cells, ", occs_200, " occurrences"))

## Plot occurrences
pdf("figures/final/supplemental/genera_nOccurrencesPerCell_min3gen_min20occ_max0.05RCtailSlope.pdf", width = 14, height = 14)
par(mfrow = c(2,1))
boxplot(plotData$occsValues ~ plotData$cats, ylab = "Number of occurrences in grid cell", xlab = "", yaxs = "i", xaxt = "n", main = "Number of occurrences in viable grid cells (no limit)", col = "lightblue")
axis(side = 1, at = c(1, 2, 3, 4), labels = xlabs)
boxplot(plotData$occsValues ~ plotData$cats, ylim = c(0,1000), ylab = "Number of occurrences in grid cell", xlab = "", yaxs = "i", xaxt = "n", main = "Number of occurrences in viable grid cells (y-axis capped at 1000 occurrences)", col = "pink")
axis(side = 1, at = c(1, 2, 3, 4), labels = xlabs)
dev.off()
