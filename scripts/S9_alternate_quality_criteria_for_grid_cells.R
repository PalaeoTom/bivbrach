#### S9 -  Using tangent line intercept with rarefaction curve as quality criterion
## Started 31/07/2026

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

## Clean things up
rm(list=ls())

## Read in master_XXX_2_2
master_50 <- readRDS("data/final/master_50_2_2.Rds")
master_100 <- readRDS("data/final/master_100_2_2.Rds")
master_150 <- readRDS("data/final/master_150_2_2.Rds")
master_200 <- readRDS("data/final/master_200_2_2.Rds")

## Start by standardising grid cells. Remove all that have fewer than 20 occs.
source("functions/standardiseCells.R")

## Set minimum
gen.min <- 3

## Get standardised data - minimum 3 unique taxa per cell
master_50 <- standardiseCells(master_50, cell = "stage_cell", type = "taxa", minOccs = gen.min)
master_100 <- standardiseCells(master_100, cell = "stage_cell", type = "taxa", minOccs = gen.min)
master_150 <- standardiseCells(master_150, cell = "stage_cell", type = "taxa", minOccs = gen.min)
master_200 <- standardiseCells(master_200, cell = "stage_cell", type = "taxa", minOccs = gen.min)

## Export
saveRDS(master_50, "data/final/master_50_2_2_min3gen.Rds")
saveRDS(master_100, "data/final/master_100_2_2_min3gen.Rds")
saveRDS(master_150, "data/final/master_150_2_2_min3gen.Rds")
saveRDS(master_200, "data/final/master_200_2_2_min3gen.Rds")

## Clean up once again
rm(list=ls())

## Read in rarefaction functions
source("functions/rarefaction_curve.R")
source("functions/rarefaction_curve_all_cells.R")

## Now, for each grid cell, assess sample sizes required for asymptotes.
## Read in standardised data
master_50 <- readRDS("data/final/master_50_2_2_min3gen.Rds")
master_100 <- readRDS("data/final/master_100_2_2_min3gen.Rds")
master_150 <- readRDS("data/final/master_150_2_2_min3gen.Rds")
master_200 <- readRDS("data/final/master_200_2_2_min3gen.Rds")

## Run rarefaction function
master_50_RCs <- rarefaction_curve_all_cells(data = master_50, cell = "stage_cell", taxVar = "combined_name", iter = 1000)
master_100_RCs <- rarefaction_curve_all_cells(data = master_100, cell = "stage_cell", taxVar = "combined_name", iter = 1000)
master_150_RCs <- rarefaction_curve_all_cells(data = master_150, cell = "stage_cell", taxVar = "combined_name", iter = 1000)
master_200_RCs <- rarefaction_curve_all_cells(data = master_200, cell = "stage_cell", taxVar = "combined_name", iter = 1000)

## Export
saveRDS(master_50_RCs, file = "data/sensitivity_testing/genera_50_rarefactionCurves.Rds")
saveRDS(master_100_RCs, file = "data/sensitivity_testing/genera_100_rarefactionCurves.Rds")
saveRDS(master_150_RCs, file = "data/sensitivity_testing/genera_150_rarefactionCurves.Rds")
saveRDS(master_200_RCs, file = "data/sensitivity_testing/genera_200_rarefactionCurves.Rds")

#### 50km ####
## Clean up
rm(list=ls())

## Read in functions
source("functions/get_intercepts.R")
source("functions/get_line_values.R")

## Read in rarefaction curve data and proceed!
master_50_RCs <- readRDS("data/sensitivity_testing/genera_50_rarefactionCurves.Rds")

## Read in standardised data
master_50 <- readRDS("data/final/master_50_2_2_min3gen.Rds")

## Get intercepts for 2.5, 5 and 10 degrees
master_50_intercepts2.5 <- get_intercepts(data = master_50_RCs[,-1], angle = 2.5)
master_50_intercepts5 <- get_intercepts(data = master_50_RCs[,-1], angle = 5)
master_50_intercepts10 <- get_intercepts(data = master_50_RCs[,-1], angle = 10)

## 2.5 degree intercept
GCs_2.5_50 <- master_50_intercepts2.5[which(!is.na(master_50_intercepts2.5[,"x_value"])),"cells"]
IntOccs_2.5_50 <- as.numeric(master_50_intercepts2.5[which(!is.na(master_50_intercepts2.5[,"x_value"])),"x_value"])
IntRich_2.5_50 <- as.numeric(master_50_intercepts2.5[which(!is.na(master_50_intercepts2.5[,"x_value"])),"y_value"])
Occs_2.5_50 <- master_50[which(master_50$stage_cell %in% GCs_2.5_50),"combined_name"]
OccsPerCell_2.5_50 <- as.vector(table(master_50[which(master_50$stage_cell %in% GCs_2.5_50),"stage_cell"]))
length(Occs_2.5_50)

## 5 degree intercept
GCs_5_50 <- master_50_intercepts5[which(!is.na(master_50_intercepts5[,"x_value"])),"cells"]
IntOccs_5_50 <- as.numeric(master_50_intercepts5[which(!is.na(master_50_intercepts5[,"x_value"])),"x_value"])
IntRich_5_50 <- as.numeric(master_50_intercepts5[which(!is.na(master_50_intercepts5[,"x_value"])),"y_value"])
Occs_5_50 <- master_50[which(master_50$stage_cell %in% GCs_5_50),"combined_name"]
OccsPerCell_5_50 <- as.vector(table(master_50[which(master_50$stage_cell %in% GCs_5_50),"stage_cell"]))
length(Occs_5_50)

## 10 degree intercept
GCs_10_50 <- master_50_intercepts10[which(!is.na(master_50_intercepts10[,"x_value"])),"cells"]
IntOccs_10_50 <- as.numeric(master_50_intercepts10[which(!is.na(master_50_intercepts10[,"x_value"])),"x_value"])
IntRich_10_50 <- as.numeric(master_50_intercepts10[which(!is.na(master_50_intercepts10[,"x_value"])),"y_value"])
Occs_10_50 <- master_50[which(master_50$stage_cell %in% GCs_10_50),"combined_name"]
OccsPerCell_10_50 <- as.vector(table(master_50[which(master_50$stage_cell %in% GCs_10_50),"stage_cell"]))
length(Occs_10_50)

## Plot grouped boxplot for each
cats <- c(rep(paste0("2.5 degree tangent (", length(GCs_2.5_50), " grid cells with intercepts)"), times = length(GCs_2.5_50)),
          rep(paste0("5 degree tangent (", length(GCs_5_50), " grid cells with intercepts)"), times = length(GCs_5_50)),
          rep(paste0("10 degree tangent (", length(GCs_10_50), " grid cells with intercepts)"), times = length(GCs_10_50)))
cats <- factor(cats, levels = c(paste0("2.5 degree tangent (", length(GCs_2.5_50), " grid cells with intercepts)"),
                                paste0("5 degree tangent (", length(GCs_5_50), " grid cells with intercepts)"),
                                paste0("10 degree tangent (", length(GCs_10_50), " grid cells with intercepts)")))
occsValues <- c(IntOccs_2.5_50, IntOccs_5_50, IntOccs_10_50)
richValues <- c(IntRich_2.5_50, IntRich_5_50, IntRich_10_50)
OccsPerCellvalues <- c(OccsPerCell_2.5_50, OccsPerCell_5_50, OccsPerCell_10_50)

plotData <- data.frame(cbind(cats, occsValues, richValues, OccsPerCellvalues))

## Plot occurrences
pdf("figures/final/supplemental/genera_50_tangent_intercept.pdf", width = 12, height = 14)
par(mfrow = c(3,1))
boxplot(plotData$occsValues ~ plotData$cats, ylim = c(0,900), ylab = "Number of occurrences at rarefaction curve intercept", xlab = "", yaxs = "i", xaxt = "n", main = "Number of occurrences at rarefaction curve intercept for 50km grid cells", col = "lightblue")
axis(side = 1, at = c(1, 2, 3), labels = levels(cats))
abline(h = 50, lty = "dashed")
abline(h = 100, lty = "dashed")
boxplot(plotData$richValues ~ plotData$cats, ylim = c(0, 210), ylab = "Richness at rarefaction curve intercept", xlab = "", yaxs = "i", xaxt = "n", main = "Richness at rarefaction curveintercept for 50km grid cells", col = "pink")
axis(side = 1, at = c(1, 2, 3), labels = levels(cats))
boxplot(plotData$OccsPerCellvalues ~ plotData$cats, ylim = c(0, 1000), ylab = "Number of occurrences in cell", xlab = "", yaxs = "i", xaxt = "n", main = "Number of occurrences in 50km grid cells with rarefaction curve intercepts\n(capped at 1000 occurrences)", col = "orange")
axis(side = 1, at = c(1, 2, 3), labels = levels(cats))
abline(h = 50, lty = "dashed")
abline(h = 100, lty = "dashed")
dev.off()

#### 100km ####
## Clean up
rm(list=ls())

## Read in functions
source("functions/get_intercepts.R")
source("functions/get_line_values.R")

## Read in rarefaction curve data and proceed!
master_100_RCs <- readRDS("data/sensitivity_testing/genera_100_rarefactionCurves.Rds")

## Read in standardised data
master_100 <- readRDS("data/final/master_100_2_2_min3gen.Rds")

## Get intercepts
master_100_intercepts2.5 <- get_intercepts(data = master_100_RCs[,-1], angle = 2.5)
master_100_intercepts5 <- get_intercepts(data = master_100_RCs[,-1], angle = 5)
master_100_intercepts10 <- get_intercepts(data = master_100_RCs[,-1], angle = 10)

## 2.5 degree intercept
GCs_2.5_100 <- master_100_intercepts2.5[which(!is.na(master_100_intercepts2.5[,"x_value"])),"cells"]
IntOccs_2.5_100 <- as.numeric(master_100_intercepts2.5[which(!is.na(master_100_intercepts2.5[,"x_value"])),"x_value"])
IntRich_2.5_100 <- as.numeric(master_100_intercepts2.5[which(!is.na(master_100_intercepts2.5[,"x_value"])),"y_value"])
Occs_2.5_100 <- master_100[which(master_100$stage_cell %in% GCs_2.5_100),"combined_name"]
OccsPerCell_2.5_100 <- as.vector(table(master_100[which(master_100$stage_cell %in% GCs_2.5_100),"stage_cell"]))
length(Occs_2.5_100)

## 5 degree intercept
GCs_5_100 <- master_100_intercepts5[which(!is.na(master_100_intercepts5[,"x_value"])),"cells"]
IntOccs_5_100 <- as.numeric(master_100_intercepts5[which(!is.na(master_100_intercepts5[,"x_value"])),"x_value"])
IntRich_5_100 <- as.numeric(master_100_intercepts5[which(!is.na(master_100_intercepts5[,"x_value"])),"y_value"])
Occs_5_100 <- master_100[which(master_100$stage_cell %in% GCs_5_100),"combined_name"]
OccsPerCell_5_100 <- as.vector(table(master_100[which(master_100$stage_cell %in% GCs_5_100),"stage_cell"]))
length(Occs_5_100)

## 10 degree intercept
GCs_10_100 <- master_100_intercepts10[which(!is.na(master_100_intercepts10[,"x_value"])),"cells"]
IntOccs_10_100 <- as.numeric(master_100_intercepts10[which(!is.na(master_100_intercepts10[,"x_value"])),"x_value"])
IntRich_10_100 <- as.numeric(master_100_intercepts10[which(!is.na(master_100_intercepts10[,"x_value"])),"y_value"])
Occs_10_100 <- master_100[which(master_100$stage_cell %in% GCs_10_100),"combined_name"]
OccsPerCell_10_100 <- as.vector(table(master_100[which(master_100$stage_cell %in% GCs_10_100),"stage_cell"]))
length(Occs_10_100)

## Plot grouped boxplot for each
cats <- c(rep(paste0("2.5 degree tangent (", length(GCs_2.5_100), " grid cells with intercepts)"), times = length(GCs_2.5_100)),
          rep(paste0("5 degree tangent (", length(GCs_5_100), " grid cells with intercepts)"), times = length(GCs_5_100)),
          rep(paste0("10 degree tangent (", length(GCs_10_100), " grid cells with intercepts)"), times = length(GCs_10_100)))
cats <- factor(cats, levels = c(paste0("2.5 degree tangent (", length(GCs_2.5_100), " grid cells with intercepts)"),
                                paste0("5 degree tangent (", length(GCs_5_100), " grid cells with intercepts)"),
                                paste0("10 degree tangent (", length(GCs_10_100), " grid cells with intercepts)")))
occsValues <- c(IntOccs_2.5_100, IntOccs_5_100, IntOccs_10_100)
richValues <- c(IntRich_2.5_100, IntRich_5_100, IntRich_10_100)
OccsPerCellvalues <- c(OccsPerCell_2.5_100, OccsPerCell_5_100, OccsPerCell_10_100)

plotData <- data.frame(cbind(cats, occsValues, richValues, OccsPerCellvalues))

## Plot occurrences
pdf("figures/final/supplemental/genera_100_tangent_intercept.pdf", width = 12, height = 14)
par(mfrow = c(3,1))
boxplot(plotData$occsValues ~ plotData$cats, ylim = c(0,900), ylab = "Number of occurrences at rarefaction curve intercept", xlab = "", yaxs = "i", xaxt = "n", main = "Number of occurrences at rarefaction curve intercept for 100km grid cells", col = "lightblue")
axis(side = 1, at = c(1, 2, 3), labels = levels(cats))
abline(h = 50, lty = "dashed")
abline(h = 100, lty = "dashed")
boxplot(plotData$richValues ~ plotData$cats, ylim = c(0, 210), ylab = "Richness at rarefaction curve intercept", xlab = "", yaxs = "i", xaxt = "n", main = "Richness at rarefaction curveintercept for 100km grid cells", col = "pink")
axis(side = 1, at = c(1, 2, 3), labels = levels(cats))
boxplot(plotData$OccsPerCellvalues ~ plotData$cats, ylim = c(0, 1000), ylab = "Number of occurrences in cell", xlab = "", yaxs = "i", xaxt = "n", main = "Number of occurrences in 100km grid cells with rarefaction curve intercepts\n(capped at 1000 occurrences)", col = "orange")
axis(side = 1, at = c(1, 2, 3), labels = levels(cats))
abline(h = 50, lty = "dashed")
abline(h = 100, lty = "dashed")
dev.off()

#### 150km ####
## Clean up
rm(list=ls())

## Read in functions
source("functions/get_intercepts.R")
source("functions/get_line_values.R")

## Read in rarefaction curve data and proceed!
master_150_RCs <- readRDS("data/sensitivity_testing/genera_150_rarefactionCurves.Rds")

## Read in standardised data
master_150 <- readRDS("data/final/master_150_2_2_min3gen.Rds")

## Get intercepts
master_150_intercepts2.5 <- get_intercepts(data = master_150_RCs[,-1], angle = 2.5)
master_150_intercepts5 <- get_intercepts(data = master_150_RCs[,-1], angle = 5)
master_150_intercepts10 <- get_intercepts(data = master_150_RCs[,-1], angle = 10)

## 2.5 degree intercept
GCs_2.5_150 <- master_150_intercepts2.5[which(!is.na(master_150_intercepts2.5[,"x_value"])),"cells"]
IntOccs_2.5_150 <- as.numeric(master_150_intercepts2.5[which(!is.na(master_150_intercepts2.5[,"x_value"])),"x_value"])
IntRich_2.5_150 <- as.numeric(master_150_intercepts2.5[which(!is.na(master_150_intercepts2.5[,"x_value"])),"y_value"])
Occs_2.5_150 <- master_150[which(master_150$stage_cell %in% GCs_2.5_150),"combined_name"]
OccsPerCell_2.5_150 <- as.vector(table(master_150[which(master_150$stage_cell %in% GCs_2.5_150),"stage_cell"]))
length(Occs_2.5_150)

## 5 degree intercept
GCs_5_150 <- master_150_intercepts5[which(!is.na(master_150_intercepts5[,"x_value"])),"cells"]
IntOccs_5_150 <- as.numeric(master_150_intercepts5[which(!is.na(master_150_intercepts5[,"x_value"])),"x_value"])
IntRich_5_150 <- as.numeric(master_150_intercepts5[which(!is.na(master_150_intercepts5[,"x_value"])),"y_value"])
Occs_5_150 <- master_150[which(master_150$stage_cell %in% GCs_5_150),"combined_name"]
OccsPerCell_5_150 <- as.vector(table(master_150[which(master_150$stage_cell %in% GCs_5_150),"stage_cell"]))
length(Occs_5_150)

## 10 degree intercept
GCs_10_150 <- master_150_intercepts10[which(!is.na(master_150_intercepts10[,"x_value"])),"cells"]
IntOccs_10_150 <- as.numeric(master_150_intercepts10[which(!is.na(master_150_intercepts10[,"x_value"])),"x_value"])
IntRich_10_150 <- as.numeric(master_150_intercepts10[which(!is.na(master_150_intercepts10[,"x_value"])),"y_value"])
Occs_10_150 <- master_150[which(master_150$stage_cell %in% GCs_10_150),"combined_name"]
OccsPerCell_10_150 <- as.vector(table(master_150[which(master_150$stage_cell %in% GCs_10_150),"stage_cell"]))
length(Occs_10_150)

## Plot grouped boxplot for each
cats <- c(rep(paste0("2.5 degree tangent (", length(GCs_2.5_150), " grid cells with intercepts)"), times = length(GCs_2.5_150)),
          rep(paste0("5 degree tangent (", length(GCs_5_150), " grid cells with intercepts)"), times = length(GCs_5_150)),
          rep(paste0("10 degree tangent (", length(GCs_10_150), " grid cells with intercepts)"), times = length(GCs_10_150)))
cats <- factor(cats, levels = c(paste0("2.5 degree tangent (", length(GCs_2.5_150), " grid cells with intercepts)"),
                                paste0("5 degree tangent (", length(GCs_5_150), " grid cells with intercepts)"),
                                paste0("10 degree tangent (", length(GCs_10_150), " grid cells with intercepts)")))
occsValues <- c(IntOccs_2.5_150, IntOccs_5_150, IntOccs_10_150)
richValues <- c(IntRich_2.5_150, IntRich_5_150, IntRich_10_150)
OccsPerCellvalues <- c(OccsPerCell_2.5_150, OccsPerCell_5_150, OccsPerCell_10_150)

plotData <- data.frame(cbind(cats, occsValues, richValues, OccsPerCellvalues))

## Plot occurrences
pdf("figures/final/supplemental/genera_150_tangent_intercept.pdf", width = 12, height = 14)
par(mfrow = c(3,1))
boxplot(plotData$occsValues ~ plotData$cats, ylim = c(0,900), ylab = "Number of occurrences at rarefaction curve intercept", xlab = "", yaxs = "i", xaxt = "n", main = "Number of occurrences at rarefaction curve intercept for 150km grid cells", col = "lightblue")
axis(side = 1, at = c(1, 2, 3), labels = levels(cats))
abline(h = 50, lty = "dashed")
abline(h = 100, lty = "dashed")
boxplot(plotData$richValues ~ plotData$cats, ylim = c(0, 210), ylab = "Richness at rarefaction curve intercept", xlab = "", yaxs = "i", xaxt = "n", main = "Richness at rarefaction curveintercept for 150km grid cells", col = "pink")
axis(side = 1, at = c(1, 2, 3), labels = levels(cats))
boxplot(plotData$OccsPerCellvalues ~ plotData$cats, ylim = c(0, 1000), ylab = "Number of occurrences in cell", xlab = "", yaxs = "i", xaxt = "n", main = "Number of occurrences in 150km grid cells with rarefaction curve intercepts\n(capped at 1000 occurrences)", col = "orange")
axis(side = 1, at = c(1, 2, 3), labels = levels(cats))
abline(h = 50, lty = "dashed")
abline(h = 100, lty = "dashed")
dev.off()

#### 200km ####
## Clean up
rm(list=ls())

## Read in functions
source("functions/get_intercepts.R")
source("functions/get_line_values.R")

## Read in rarefaction curve data and proceed!
master_200_RCs <- readRDS("data/sensitivity_testing/genera_200_rarefactionCurves.Rds")

## Read in standardised data
master_200 <- readRDS("data/final/master_200_2_2_min3gen.Rds")

## Get intercepts
master_200_intercepts2.5 <- get_intercepts(data = master_200_RCs[,-1], angle = 2.5)
master_200_intercepts5 <- get_intercepts(data = master_200_RCs[,-1], angle = 5)
master_200_intercepts10 <- get_intercepts(data = master_200_RCs[,-1], angle = 10)

## 2.5 degree intercept
GCs_2.5_200 <- master_200_intercepts2.5[which(!is.na(master_200_intercepts2.5[,"x_value"])),"cells"]
IntOccs_2.5_200 <- as.numeric(master_200_intercepts2.5[which(!is.na(master_200_intercepts2.5[,"x_value"])),"x_value"])
IntRich_2.5_200 <- as.numeric(master_200_intercepts2.5[which(!is.na(master_200_intercepts2.5[,"x_value"])),"y_value"])
Occs_2.5_200 <- master_200[which(master_200$stage_cell %in% GCs_2.5_200),"combined_name"]
OccsPerCell_2.5_200 <- as.vector(table(master_200[which(master_200$stage_cell %in% GCs_2.5_200),"stage_cell"]))
length(Occs_2.5_200)

## 5 degree intercept
GCs_5_200 <- master_200_intercepts5[which(!is.na(master_200_intercepts5[,"x_value"])),"cells"]
IntOccs_5_200 <- as.numeric(master_200_intercepts5[which(!is.na(master_200_intercepts5[,"x_value"])),"x_value"])
IntRich_5_200 <- as.numeric(master_200_intercepts5[which(!is.na(master_200_intercepts5[,"x_value"])),"y_value"])
Occs_5_200 <- master_200[which(master_200$stage_cell %in% GCs_5_200),"combined_name"]
OccsPerCell_5_200 <- as.vector(table(master_200[which(master_200$stage_cell %in% GCs_5_200),"stage_cell"]))
length(Occs_5_200)

## 10 degree intercept
GCs_10_200 <- master_200_intercepts10[which(!is.na(master_200_intercepts10[,"x_value"])),"cells"]
IntOccs_10_200 <- as.numeric(master_200_intercepts10[which(!is.na(master_200_intercepts10[,"x_value"])),"x_value"])
IntRich_10_200 <- as.numeric(master_200_intercepts10[which(!is.na(master_200_intercepts10[,"x_value"])),"y_value"])
Occs_10_200 <- master_200[which(master_200$stage_cell %in% GCs_10_200),"combined_name"]
OccsPerCell_10_200 <- as.vector(table(master_200[which(master_200$stage_cell %in% GCs_10_200),"stage_cell"]))
length(Occs_10_200)

## Plot grouped boxplot for each
cats <- c(rep(paste0("2.5 degree tangent (", length(GCs_2.5_200), " grid cells with intercepts)"), times = length(GCs_2.5_200)),
          rep(paste0("5 degree tangent (", length(GCs_5_200), " grid cells with intercepts)"), times = length(GCs_5_200)),
          rep(paste0("10 degree tangent (", length(GCs_10_200), " grid cells with intercepts)"), times = length(GCs_10_200)))
cats <- factor(cats, levels = c(paste0("2.5 degree tangent (", length(GCs_2.5_200), " grid cells with intercepts)"),
                                paste0("5 degree tangent (", length(GCs_5_200), " grid cells with intercepts)"),
                                paste0("10 degree tangent (", length(GCs_10_200), " grid cells with intercepts)")))
occsValues <- c(IntOccs_2.5_200, IntOccs_5_200, IntOccs_10_200)
richValues <- c(IntRich_2.5_200, IntRich_5_200, IntRich_10_200)
OccsPerCellvalues <- c(OccsPerCell_2.5_200, OccsPerCell_5_200, OccsPerCell_10_200)

plotData <- data.frame(cbind(cats, occsValues, richValues, OccsPerCellvalues))

## Plot occurrences
pdf("figures/final/supplemental/genera_200_tangent_intercept.pdf", width = 12, height = 14)
par(mfrow = c(3,1))
boxplot(plotData$occsValues ~ plotData$cats, ylim = c(0,900), ylab = "Number of occurrences at rarefaction curve intercept", xlab = "", yaxs = "i", xaxt = "n", main = "Number of occurrences at rarefaction curve intercept for 200km grid cells", col = "lightblue")
axis(side = 1, at = c(1, 2, 3), labels = levels(cats))
abline(h = 50, lty = "dashed")
abline(h = 100, lty = "dashed")
boxplot(plotData$richValues ~ plotData$cats, ylim = c(0, 210), ylab = "Richness at rarefaction curve intercept", xlab = "", yaxs = "i", xaxt = "n", main = "Richness at rarefaction curveintercept for 200km grid cells", col = "pink")
axis(side = 1, at = c(1, 2, 3), labels = levels(cats))
boxplot(plotData$OccsPerCellvalues ~ plotData$cats, ylim = c(0, 1000), ylab = "Number of occurrences in cell", xlab = "", yaxs = "i", xaxt = "n", main = "Number of occurrences in 200km grid cells with rarefaction curve intercepts\n(capped at 1000 occurrences)", col = "orange")
axis(side = 1, at = c(1, 2, 3), labels = levels(cats))
abline(h = 50, lty = "dashed")
abline(h = 100, lty = "dashed")
dev.off()

#### Assessing grid cell quality - 5 degree tangent line, 20 occurrence max####
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

## Get 5 degree and 10 degree intercepts for these data
## Read in functions
source("functions/get_intercepts.R")
source("functions/get_line_values.R")

## Get intercepts
master_50_intercepts5 <- get_intercepts(data = master_50_RCs[,-1], angle = 5)
master_50_intercepts10 <- get_intercepts(data = master_50_RCs[,-1], angle = 10)
master_100_intercepts5 <- get_intercepts(data = master_100_RCs[,-1], angle = 5)
master_100_intercepts10 <- get_intercepts(data = master_100_RCs[,-1], angle = 10)
master_150_intercepts5 <- get_intercepts(data = master_150_RCs[,-1], angle = 5)
master_150_intercepts10 <- get_intercepts(data = master_150_RCs[,-1], angle = 10)
master_200_intercepts5 <- get_intercepts(data = master_200_RCs[,-1], angle = 5)
master_200_intercepts10 <- get_intercepts(data = master_200_RCs[,-1], angle = 10)

## Drop NAs
master_50_intercepts5 <- master_50_intercepts5[-which(is.na(master_50_intercepts5$x_value)),]
master_50_intercepts10 <- master_50_intercepts10[-which(is.na(master_50_intercepts10$x_value)),]
master_100_intercepts5 <- master_100_intercepts5[-which(is.na(master_100_intercepts5$x_value)),]
master_100_intercepts10 <- master_100_intercepts10[-which(is.na(master_100_intercepts10$x_value)),]
master_150_intercepts5 <- master_150_intercepts5[-which(is.na(master_150_intercepts5$x_value)),]
master_150_intercepts10 <- master_150_intercepts10[-which(is.na(master_150_intercepts10$x_value)),]
master_200_intercepts5 <- master_200_intercepts5[-which(is.na(master_200_intercepts5$x_value)),]
master_200_intercepts10 <- master_200_intercepts10[-which(is.na(master_200_intercepts10$x_value)),]

## Apply x-axis filters of 20 (for 5 degree tangent) and 50 (for 10 degree tangent)
master_50_final_5deg_max20 <- master_50[which(master_50$stage_cell %in% master_50_intercepts5[which(master_50_intercepts5$x_value <= 20),"cells"]),]
master_50_final_10deg_max50 <- master_50[which(master_50$stage_cell %in% master_50_intercepts10[which(master_50_intercepts10$x_value <= 50),"cells"]),]
master_100_final_5deg_max20 <- master_100[which(master_100$stage_cell %in% master_100_intercepts5[which(master_100_intercepts5$x_value <= 20),"cells"]),]
master_100_final_10deg_max50 <- master_100[which(master_100$stage_cell %in% master_100_intercepts10[which(master_100_intercepts10$x_value <= 50),"cells"]),]
master_150_final_5deg_max20 <- master_150[which(master_150$stage_cell %in% master_150_intercepts5[which(master_150_intercepts5$x_value <= 20),"cells"]),]
master_150_final_10deg_max50 <- master_150[which(master_150$stage_cell %in% master_150_intercepts10[which(master_150_intercepts10$x_value <= 50),"cells"]),]
master_200_final_5deg_max20 <- master_200[which(master_200$stage_cell %in% master_200_intercepts5[which(master_200_intercepts5$x_value <= 20),"cells"]),]
master_200_final_10deg_max50 <- master_200[which(master_200$stage_cell %in% master_200_intercepts10[which(master_200_intercepts10$x_value <= 50),"cells"]),]

## Get number of cells in these datasets
length(unique(master_50_final_5deg_max20$stage_cell))
length(unique(master_50_final_10deg_max50$stage_cell))
length(unique(master_100_final_5deg_max20$stage_cell))
length(unique(master_100_final_10deg_max50$stage_cell))
length(unique(master_150_final_5deg_max20$stage_cell))
length(unique(master_150_final_10deg_max50$stage_cell))
length(unique(master_200_final_5deg_max20$stage_cell))
length(unique(master_200_final_10deg_max50$stage_cell))

## Get number of occurrences in each of these datasets
nrow(master_50_final_5deg_max20)
nrow(master_50_final_10deg_max50)
nrow(master_100_final_5deg_max20)
nrow(master_100_final_10deg_max50)
nrow(master_150_final_5deg_max20)
nrow(master_150_final_10deg_max50)
nrow(master_200_final_5deg_max20)
nrow(master_200_final_10deg_max50)

## Assessing grid cell quality - start with stricter setup (5 degree tangent, max 20 intercept)
## Isolate cells and coordinates
master_50_final_5deg_max20_uniqGC <- master_50_final_5deg_max20[,c(41,42,44)]
master_100_final_5deg_max20_uniqGC <- master_100_final_5deg_max20[,c(41,42,44)]
master_150_final_5deg_max20_uniqGC <- master_150_final_5deg_max20[,c(41,42,44)]
master_200_final_5deg_max20_uniqGC <- master_200_final_5deg_max20[,c(41,42,44)]

## Drop duplicated rows
master_50_final_5deg_max20_uniqGC <- master_50_final_5deg_max20[which(!duplicated(master_50_final_5deg_max20_uniqGC)),]
master_100_final_5deg_max20_uniqGC <- master_100_final_5deg_max20[which(!duplicated(master_100_final_5deg_max20_uniqGC)),]
master_150_final_5deg_max20_uniqGC <- master_150_final_5deg_max20[which(!duplicated(master_150_final_5deg_max20_uniqGC)),]
master_200_final_5deg_max20_uniqGC <- master_200_final_5deg_max20[which(!duplicated(master_200_final_5deg_max20_uniqGC)),]

## Read in functions to check stages and cells
source("functions/stage.check.R")
source("functions/cov.check.R")

## Run functions - first, get stages
length(stage.check(master_50_final_5deg_max20_uniqGC, output = "stages"))
length(stage.check(master_100_final_5deg_max20_uniqGC, output = "stages"))
length(stage.check(master_150_final_5deg_max20_uniqGC, output = "stages"))
length(stage.check(master_200_final_5deg_max20_uniqGC, output = "stages"))

## Then get grid cells
length(stage.check(master_50_final_5deg_max20_uniqGC, output = "cells"))
length(stage.check(master_100_final_5deg_max20_uniqGC, output = "cells"))
length(stage.check(master_150_final_5deg_max20_uniqGC, output = "cells"))
length(stage.check(master_200_final_5deg_max20_uniqGC, output = "cells"))

##Get occurrences associated with these grid cells
master_50_final_5deg_max20_min2cellStage <- master_50_final_5deg_max20[which(master_50_final_5deg_max20$stage_cell %in% stage.check(master_50_final_5deg_max20_uniqGC, output = "cells")),]
master_100_final_5deg_max20_min2cellStage <- master_100_final_5deg_max20[which(master_100_final_5deg_max20$stage_cell %in% stage.check(master_100_final_5deg_max20_uniqGC, output = "cells")),]
master_150_final_5deg_max20_min2cellStage <- master_150_final_5deg_max20[which(master_150_final_5deg_max20$stage_cell %in% stage.check(master_150_final_5deg_max20_uniqGC, output = "cells")),]
master_200_final_5deg_max20_min2cellStage <- master_200_final_5deg_max20[which(master_200_final_5deg_max20$stage_cell %in% stage.check(master_200_final_5deg_max20_uniqGC, output = "cells")),]
nrow(master_50_final_5deg_max20_min2cellStage)
nrow(master_100_final_5deg_max20_min2cellStage)
nrow(master_150_final_5deg_max20_min2cellStage)
nrow(master_200_final_5deg_max20_min2cellStage)

## Individual covariates
length(cov.check(data = master_50_final_5deg_max20_uniqGC, covVar = "cellLith"))
length(cov.check(data = master_100_final_5deg_max20_uniqGC, covVar = "cellLith"))
length(cov.check(data = master_150_final_5deg_max20_uniqGC, covVar = "cellLith"))
length(cov.check(data = master_200_final_5deg_max20_uniqGC, covVar = "cellLith"))

length(cov.check(data = master_50_final_5deg_max20_uniqGC, covVar = "cellBath"))
length(cov.check(data = master_100_final_5deg_max20_uniqGC, covVar = "cellBath"))
length(cov.check(data = master_150_final_5deg_max20_uniqGC, covVar = "cellBath"))
length(cov.check(data = master_200_final_5deg_max20_uniqGC, covVar = "cellBath"))

length(cov.check(data = master_50_final_5deg_max20_uniqGC, covVar = "cellReef"))
length(cov.check(data = master_100_final_5deg_max20_uniqGC, covVar = "cellReef"))
length(cov.check(data = master_150_final_5deg_max20_uniqGC, covVar = "cellReef"))
length(cov.check(data = master_200_final_5deg_max20_uniqGC, covVar = "cellReef"))

length(cov.check(data = master_50_final_5deg_max20_uniqGC, covVar = c("cellLith", "cellBath", "cellReef")))
length(cov.check(data = master_100_final_5deg_max20_uniqGC, covVar = c("cellLith", "cellBath", "cellReef")))
length(cov.check(data = master_150_final_5deg_max20_uniqGC, covVar = c("cellLith", "cellBath", "cellReef")))
length(cov.check(data = master_200_final_5deg_max20_uniqGC, covVar = c("cellLith", "cellBath", "cellReef")))

## Trim dataset to get occurrences
master_50_final_5deg_max20_covData <- master_50_final_5deg_max20[which(master_50_final_5deg_max20$stage_cell %in% cov.check(data = master_50_final_5deg_max20_uniqGC, covVar = c("cellLith", "cellBath", "cellReef"))),]
master_100_final_5deg_max20_covData <- master_100_final_5deg_max20[which(master_100_final_5deg_max20$stage_cell %in% cov.check(data = master_100_final_5deg_max20_uniqGC, covVar = c("cellLith", "cellBath", "cellReef"))),]
master_150_final_5deg_max20_covData <- master_150_final_5deg_max20[which(master_150_final_5deg_max20$stage_cell %in% cov.check(data = master_150_final_5deg_max20_uniqGC, covVar = c("cellLith", "cellBath", "cellReef"))),]
master_200_final_5deg_max20_covData <- master_200_final_5deg_max20[which(master_200_final_5deg_max20$stage_cell %in% cov.check(data = master_200_final_5deg_max20_uniqGC, covVar = c("cellLith", "cellBath", "cellReef"))),]
nrow(master_50_final_5deg_max20_covData)
nrow(master_100_final_5deg_max20_covData)
nrow(master_150_final_5deg_max20_covData)
nrow(master_200_final_5deg_max20_covData)

## Get final version of dataset where each stage is represented by at least two grid cells
## First, get unique version of dataset
## Isolate cells and coordinates
master_50_final_5deg_max20_covData_uniqGC <- master_50_final_5deg_max20_covData[,c(41,42,44)]
master_100_final_5deg_max20_covData_uniqGC <- master_100_final_5deg_max20_covData[,c(41,42,44)]
master_150_final_5deg_max20_covData_uniqGC <- master_150_final_5deg_max20_covData[,c(41,42,44)]
master_200_final_5deg_max20_covData_uniqGC <- master_200_final_5deg_max20_covData[,c(41,42,44)]

## Drop duplicated rows
master_50_final_5deg_max20_covData_uniqGC <- master_50_final_5deg_max20_covData[which(!duplicated(master_50_final_5deg_max20_covData_uniqGC)),]
master_100_final_5deg_max20_covData_uniqGC <- master_100_final_5deg_max20_covData[which(!duplicated(master_100_final_5deg_max20_covData_uniqGC)),]
master_150_final_5deg_max20_covData_uniqGC <- master_150_final_5deg_max20_covData[which(!duplicated(master_150_final_5deg_max20_covData_uniqGC)),]
master_200_final_5deg_max20_covData_uniqGC <- master_200_final_5deg_max20_covData[which(!duplicated(master_200_final_5deg_max20_covData_uniqGC)),]

## Run functions - first, get stages
length(stage.check(master_50_final_5deg_max20_covData_uniqGC, output = "stages"))
length(stage.check(master_100_final_5deg_max20_covData_uniqGC, output = "stages"))
length(stage.check(master_150_final_5deg_max20_covData_uniqGC, output = "stages"))
length(stage.check(master_200_final_5deg_max20_covData_uniqGC, output = "stages"))

## Then get grid cells
length(stage.check(master_50_final_5deg_max20_covData_uniqGC, output = "cells"))
length(stage.check(master_100_final_5deg_max20_covData_uniqGC, output = "cells"))
length(stage.check(master_150_final_5deg_max20_covData_uniqGC, output = "cells"))
length(stage.check(master_200_final_5deg_max20_covData_uniqGC, output = "cells"))

##Get occurrences associated with these grid cells
master_50_final_min2cellStage_covData <- master_50_final_5deg_max20_covData[which(master_50_final_5deg_max20_covData$stage_cell %in% stage.check(master_50_final_5deg_max20_covData_uniqGC, output = "cells")),]
master_100_final_min2cellStage_covData <- master_100_final_5deg_max20_covData[which(master_100_final_5deg_max20_covData$stage_cell %in% stage.check(master_100_final_5deg_max20_covData_uniqGC, output = "cells")),]
master_150_final_min2cellStage_covData <- master_150_final_5deg_max20_covData[which(master_150_final_5deg_max20_covData$stage_cell %in% stage.check(master_150_final_5deg_max20_covData_uniqGC, output = "cells")),]
master_200_final_min2cellStage_covData <- master_200_final_5deg_max20_covData[which(master_200_final_5deg_max20_covData$stage_cell %in% stage.check(master_200_final_5deg_max20_covData_uniqGC, output = "cells")),]
nrow(master_50_final_min2cellStage_covData)
nrow(master_100_final_min2cellStage_covData)
nrow(master_150_final_min2cellStage_covData)
nrow(master_200_final_min2cellStage_covData)

#### Assessing grid cell quality - Now do more relaxed interpretation (10 degree tangent, 50 occurrence intercept) ####
## Isolate cells and coordinates
master_50_final_10deg_max50_uniqGC <- master_50_final_10deg_max50[,c(41,42,44)]
master_100_final_10deg_max50_uniqGC <- master_100_final_10deg_max50[,c(41,42,44)]
master_150_final_10deg_max50_uniqGC <- master_150_final_10deg_max50[,c(41,42,44)]
master_200_final_10deg_max50_uniqGC <- master_200_final_10deg_max50[,c(41,42,44)]

## Drop duplicated rows
master_50_final_10deg_max50_uniqGC <- master_50_final_10deg_max50[which(!duplicated(master_50_final_10deg_max50_uniqGC)),]
master_100_final_10deg_max50_uniqGC <- master_100_final_10deg_max50[which(!duplicated(master_100_final_10deg_max50_uniqGC)),]
master_150_final_10deg_max50_uniqGC <- master_150_final_10deg_max50[which(!duplicated(master_150_final_10deg_max50_uniqGC)),]
master_200_final_10deg_max50_uniqGC <- master_200_final_10deg_max50[which(!duplicated(master_200_final_10deg_max50_uniqGC)),]

## Get number of grid cells before applying stage filter
nrow(master_50_final_10deg_max50_uniqGC)
nrow(master_100_final_10deg_max50_uniqGC)
nrow(master_150_final_10deg_max50_uniqGC)
nrow(master_200_final_10deg_max50_uniqGC)

## Read in functions to check stages and cells
source("functions/stage.check.R")
source("functions/cov.check.R")

## Run functions - first, get stages
length(stage.check(master_50_final_10deg_max50_uniqGC, output = "stages"))
length(stage.check(master_100_final_10deg_max50_uniqGC, output = "stages"))
length(stage.check(master_150_final_10deg_max50_uniqGC, output = "stages"))
length(stage.check(master_200_final_10deg_max50_uniqGC, output = "stages"))

## Then get grid cells
length(stage.check(master_50_final_10deg_max50_uniqGC, output = "cells"))
length(stage.check(master_100_final_10deg_max50_uniqGC, output = "cells"))
length(stage.check(master_150_final_10deg_max50_uniqGC, output = "cells"))
length(stage.check(master_200_final_10deg_max50_uniqGC, output = "cells"))

##Get occurrences associated with these grid cells
master_50_final_10deg_max50_min2cellStage <- master_50_final_10deg_max50[which(master_50_final_10deg_max50$stage_cell %in% stage.check(master_50_final_10deg_max50_uniqGC, output = "cells")),]
master_100_final_10deg_max50_min2cellStage <- master_100_final_10deg_max50[which(master_100_final_10deg_max50$stage_cell %in% stage.check(master_100_final_10deg_max50_uniqGC, output = "cells")),]
master_150_final_10deg_max50_min2cellStage <- master_150_final_10deg_max50[which(master_150_final_10deg_max50$stage_cell %in% stage.check(master_150_final_10deg_max50_uniqGC, output = "cells")),]
master_200_final_10deg_max50_min2cellStage <- master_200_final_10deg_max50[which(master_200_final_10deg_max50$stage_cell %in% stage.check(master_200_final_10deg_max50_uniqGC, output = "cells")),]
nrow(master_50_final_10deg_max50_min2cellStage)
nrow(master_100_final_10deg_max50_min2cellStage)
nrow(master_150_final_10deg_max50_min2cellStage)
nrow(master_200_final_10deg_max50_min2cellStage)

## Individual covariates
length(cov.check(data = master_50_final_10deg_max50_uniqGC, covVar = "cellLith"))
length(cov.check(data = master_100_final_10deg_max50_uniqGC, covVar = "cellLith"))
length(cov.check(data = master_150_final_10deg_max50_uniqGC, covVar = "cellLith"))
length(cov.check(data = master_200_final_10deg_max50_uniqGC, covVar = "cellLith"))

length(cov.check(data = master_50_final_10deg_max50_uniqGC, covVar = "cellBath"))
length(cov.check(data = master_100_final_10deg_max50_uniqGC, covVar = "cellBath"))
length(cov.check(data = master_150_final_10deg_max50_uniqGC, covVar = "cellBath"))
length(cov.check(data = master_200_final_10deg_max50_uniqGC, covVar = "cellBath"))

length(cov.check(data = master_50_final_10deg_max50_uniqGC, covVar = "cellReef"))
length(cov.check(data = master_100_final_10deg_max50_uniqGC, covVar = "cellReef"))
length(cov.check(data = master_150_final_10deg_max50_uniqGC, covVar = "cellReef"))
length(cov.check(data = master_200_final_10deg_max50_uniqGC, covVar = "cellReef"))

length(cov.check(data = master_50_final_10deg_max50_uniqGC, covVar = c("cellLith", "cellBath", "cellReef")))
length(cov.check(data = master_100_final_10deg_max50_uniqGC, covVar = c("cellLith", "cellBath", "cellReef")))
length(cov.check(data = master_150_final_10deg_max50_uniqGC, covVar = c("cellLith", "cellBath", "cellReef")))
length(cov.check(data = master_200_final_10deg_max50_uniqGC, covVar = c("cellLith", "cellBath", "cellReef")))

## Trim dataset to get occurrences
master_50_final_10deg_max50_covData <- master_50_final_10deg_max50[which(master_50_final_10deg_max50$stage_cell %in% cov.check(data = master_50_final_10deg_max50_uniqGC, covVar = c("cellLith", "cellBath", "cellReef"))),]
master_100_final_10deg_max50_covData <- master_100_final_10deg_max50[which(master_100_final_10deg_max50$stage_cell %in% cov.check(data = master_100_final_10deg_max50_uniqGC, covVar = c("cellLith", "cellBath", "cellReef"))),]
master_150_final_10deg_max50_covData <- master_150_final_10deg_max50[which(master_150_final_10deg_max50$stage_cell %in% cov.check(data = master_150_final_10deg_max50_uniqGC, covVar = c("cellLith", "cellBath", "cellReef"))),]
master_200_final_10deg_max50_covData <- master_200_final_10deg_max50[which(master_200_final_10deg_max50$stage_cell %in% cov.check(data = master_200_final_10deg_max50_uniqGC, covVar = c("cellLith", "cellBath", "cellReef"))),]
nrow(master_50_final_10deg_max50_covData)
nrow(master_100_final_10deg_max50_covData)
nrow(master_150_final_10deg_max50_covData)
nrow(master_200_final_10deg_max50_covData)

## Get final version of dataset where each stage is represented by at least two grid cells
## First, get unique version of dataset
## Isolate cells and coordinates
master_50_final_10deg_max50_covData_uniqGC <- master_50_final_10deg_max50_covData[,c(41,42,44)]
master_100_final_10deg_max50_covData_uniqGC <- master_100_final_10deg_max50_covData[,c(41,42,44)]
master_150_final_10deg_max50_covData_uniqGC <- master_150_final_10deg_max50_covData[,c(41,42,44)]
master_200_final_10deg_max50_covData_uniqGC <- master_200_final_10deg_max50_covData[,c(41,42,44)]

## Drop duplicated rows
master_50_final_10deg_max50_covData_uniqGC <- master_50_final_10deg_max50_covData[which(!duplicated(master_50_final_10deg_max50_covData_uniqGC)),]
master_100_final_10deg_max50_covData_uniqGC <- master_100_final_10deg_max50_covData[which(!duplicated(master_100_final_10deg_max50_covData_uniqGC)),]
master_150_final_10deg_max50_covData_uniqGC <- master_150_final_10deg_max50_covData[which(!duplicated(master_150_final_10deg_max50_covData_uniqGC)),]
master_200_final_10deg_max50_covData_uniqGC <- master_200_final_10deg_max50_covData[which(!duplicated(master_200_final_10deg_max50_covData_uniqGC)),]

## Run functions - first, get stages
length(stage.check(master_50_final_10deg_max50_covData_uniqGC, output = "stages"))
length(stage.check(master_100_final_10deg_max50_covData_uniqGC, output = "stages"))
length(stage.check(master_150_final_10deg_max50_covData_uniqGC, output = "stages"))
length(stage.check(master_200_final_10deg_max50_covData_uniqGC, output = "stages"))

## Then get grid cells
length(stage.check(master_50_final_10deg_max50_covData_uniqGC, output = "cells"))
length(stage.check(master_100_final_10deg_max50_covData_uniqGC, output = "cells"))
length(stage.check(master_150_final_10deg_max50_covData_uniqGC, output = "cells"))
length(stage.check(master_200_final_10deg_max50_covData_uniqGC, output = "cells"))

##Get occurrences associated with these grid cells
master_50_final_min2cellStage_covData <- master_50_final_10deg_max50_covData[which(master_50_final_10deg_max50_covData$stage_cell %in% stage.check(master_50_final_10deg_max50_covData_uniqGC, output = "cells")),]
master_100_final_min2cellStage_covData <- master_100_final_10deg_max50_covData[which(master_100_final_10deg_max50_covData$stage_cell %in% stage.check(master_100_final_10deg_max50_covData_uniqGC, output = "cells")),]
master_150_final_min2cellStage_covData <- master_150_final_10deg_max50_covData[which(master_150_final_10deg_max50_covData$stage_cell %in% stage.check(master_150_final_10deg_max50_covData_uniqGC, output = "cells")),]
master_200_final_min2cellStage_covData <- master_200_final_10deg_max50_covData[which(master_200_final_10deg_max50_covData$stage_cell %in% stage.check(master_200_final_10deg_max50_covData_uniqGC, output = "cells")),]
nrow(master_50_final_min2cellStage_covData)
nrow(master_100_final_min2cellStage_covData)
nrow(master_150_final_min2cellStage_covData)
nrow(master_200_final_min2cellStage_covData)

