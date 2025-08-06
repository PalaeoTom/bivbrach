#### 2.4 Plotting distribution of occurrences ####
## Plot occurrences retained in 100km grid cell analysis

## If packages aren't installed, install them, then load them
packages <- c("palaeoverse", "maps")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(palaeoverse)
library(maps)

## Clean directory
rm(list = ls())

## Load data
setwd("~/R_packages/bivbrach")
genera_covInt <- readRDS("data/final/final_100_genera_covsInt.Rds")
genera_covPrun <- readRDS("data/final/final_100_genera_covsPruned.Rds")
species_covInt <- readRDS("data/final/final_100_species_covsInt.Rds")
species_covPrun <- readRDS("data/final/final_100_species_covsPruned.Rds")
home <- getwd()

#### Plot distributions of occurrences across world maps ####
## Brachiopods, genera, covariates original + interpolated
pdf(file = "figures/final/supplemental/OccMap_genera_100_covsInt_brachiopod.pdf")
map("world",col="lightgrey",fill = T)
title(main = "Occurrences of brachiopod genera in 100km grid cells\n(original and interpolated covariate data)")
points(x = genera_covInt[which(genera_covInt$phylum=="Brachiopoda"),"longitude"],y = genera_covInt[which(genera_covInt$phylum=="Brachiopoda"),"latitude"], cex = 0.5, pch = 15, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
dev.off()

## Bivalves, genera, covariates original + interpolated
pdf(file = "figures/final/supplemental/OccMap_genera_100_covsInt_bivalve.pdf")
map("world",col="lightgrey",fill = T)
title(main = "Occurrences of bivalve genera in 100km grid cells\n(original and interpolated covariate data)")
points(x = genera_covInt[which(genera_covInt$phylum=="Mollusca"),"longitude"],y = genera_covInt[which(genera_covInt$phylum=="Mollusca"),"latitude"], cex = 0.5, pch = 15, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
dev.off()

## Brachiopods, genera, covariates original
pdf(file = "figures/final/supplemental/OccMap_genera_100_covsPruned_brachiopod.pdf")
map("world",col="lightgrey",fill = T)
title(main = "Occurrences of brachiopod genera in 100km grid cells\n(original covariate data only)")
points(x = genera_covPrun[which(genera_covPrun$phylum=="Brachiopoda"),"longitude"],y = genera_covPrun[which(genera_covPrun$phylum=="Brachiopoda"),"latitude"], cex = 0.5, pch = 15, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
dev.off()

## Bivalves, genera, covariates interpolated
pdf(file = "figures/final/supplemental/OccMap_genera_100_covsPruned_bivalve.pdf")
map("world",col="lightgrey",fill = T)
title(main = "Occurrences of bivalve genera in 100km grid cells\n(original covariate data only)")
points(x = genera_covPrun[which(genera_covPrun$phylum=="Mollusca"),"longitude"],y = genera_covPrun[which(genera_covPrun$phylum=="Mollusca"),"latitude"], cex = 0.5, pch = 15, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
dev.off()

## Brachiopods, species, covariates original + interpolated
pdf(file = "figures/final/supplemental/OccMap_species_100_covsInt_brachiopod.pdf")
map("world",col="lightgrey",fill = T)
title(main = "Occurrences of brachiopod species in 100km grid cells\n(original and interpolated covariate data)")
points(x = species_covInt[which(species_covInt$phylum=="Brachiopoda"),"longitude"],y = species_covInt[which(species_covInt$phylum=="Brachiopoda"),"latitude"], cex = 0.5, pch = 15, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
dev.off()

## Bivalves, species, covariates original + interpolated
pdf(file = "figures/final/supplemental/OccMap_species_100_covsInt_bivalve.pdf")
map("world",col="lightgrey",fill = T)
title(main = "Occurrences of bivalve species in 100km grid cells\n(original and interpolated covariate data)")
points(x = species_covInt[which(species_covInt$phylum=="Mollusca"),"longitude"],y = species_covInt[which(species_covInt$phylum=="Mollusca"),"latitude"], cex = 0.5, pch = 15, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
dev.off()

## Brachiopods, species, covariates original
pdf(file = "figures/final/supplemental/OccMap_species_100_covsPruned_brachiopod.pdf")
map("world",col="lightgrey",fill = T)
title(main = "Occurrences of brachiopod species in 100km grid cells\n(original covariate data only)")
points(x = species_covPrun[which(species_covPrun$phylum=="Brachiopoda"),"longitude"],y = species_covPrun[which(species_covPrun$phylum=="Brachiopoda"),"latitude"], cex = 0.5, pch = 15, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
dev.off()

## Bivalves, species, covariates interpolated
pdf(file = "figures/final/supplemental/OccMap_species_100_covsPruned_bivalve.pdf")
map("world",col="lightgrey",fill = T)
title(main = "Occurrences of bivalve species in 100km grid cells\n(original covariate data only)")
points(x = species_covPrun[which(species_covPrun$phylum=="Mollusca"),"longitude"],y = species_covPrun[which(species_covPrun$phylum=="Mollusca"),"latitude"], cex = 0.5, pch = 15, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
dev.off()

