#### 3.2 Plotting distribution of occurrences and raw richness ####
## Plot occurrences used for final setup (i.e., grid cell size)
## If packages aren't installed, install them, then load them
packages <- c("palaeoverse", "maps")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(palaeoverse)
library(maps)

## Clean directory
rm(list = ls())

## Load data - using 50km grid cells
setwd("~/R_packages/bivbrach")
genera <- readRDS("data/final/master_50_2_2_uniq.Rds")
species <- readRDS("data/final/master_50_uniq_species.Rds")
home <- getwd()

#### Plot raw richness ####
## Read in binning
time_bins <- read.csv("data/metadata/binning_timescale.csv", row.names = 1)
time_bins$number <- seq(1,nrow(time_bins),1)

## Get generic richness
bivalves <- rep(0,92)
brachiopods <- rep(0,92)
richness <- data.frame(cbind(time_bins$number, time_bins$midpoint, bivalves, brachiopods))
colnames(richness) <- c("stage","midpoint","bivalves","brachiopods")
for(b in 1:nrow(richness)){
  ## Isolate relevant occurrences
  bin <- genera[which(genera$stage==b),]
  ## If more than 0, split
  if(nrow(bin)>0){
    richness[b,"bivalves"] <- length(which(bin$phylum=="Mollusca"))
    richness[b,"brachiopods"] <- length(which(bin$phylum=="Brachiopoda"))
  }
}

## Plot
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_50km_raw_richness.pdf")
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = richness$midpoint, y = richness$bivalves, axes = FALSE, xlim = c(540, 0), ylim = c(0,7500), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Generic richness", type = "l", col = rgb(red = 1, green = 0, blue = 0, alpha = 0.75))
lines(x = richness$midpoint, y = richness$brachiopods, xlim = c(540, 0), ylim = c(0,7500),
     xlab = NA, ylab = "Generic richness", type = "l", col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75))
box()
axis(2)
legend("topleft",legend = c("Bivalves", "Brachiopods"),col = c(rgb(red = 1, green = 0, blue = 0, alpha = 0.75),col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75)),lty = 1)
axis_geo(side = 1, intervals = "international periods")
dev.off()

## Get species richness
bivalves <- rep(0,92)
brachiopods <- rep(0,92)
richness <- data.frame(cbind(time_bins$number, time_bins$midpoint, bivalves, brachiopods))
colnames(richness) <- c("stage","midpoint","bivalves","brachiopods")
for(b in 1:nrow(richness)){
  ## Isolate relevant occurrences
  bin <- species[which(species$stage==b),]
  ## If more than 0, split
  if(nrow(bin)>0){
    richness[b,"bivalves"] <- length(which(bin$phylum=="Mollusca"))
    richness[b,"brachiopods"] <- length(which(bin$phylum=="Brachiopoda"))
  }
}

## Plot
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/species_50km_raw_richness.pdf")
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = richness$midpoint, y = richness$bivalves, axes = FALSE, xlim = c(540, 0), ylim = c(0,6500), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Species richness", type = "l", col = rgb(red = 1, green = 0, blue = 0, alpha = 0.75))
lines(x = richness$midpoint, y = richness$brachiopods, xlim = c(540, 0), ylim = c(0,6500),
      xlab = NA, ylab = "Species richness", type = "l", col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75))
box()
axis(2)
legend("topleft",legend = c("Bivalves", "Brachiopods"),col = c(rgb(red = 1, green = 0, blue = 0, alpha = 0.75),col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75)),lty = 1)
axis_geo(side = 1, intervals = "international periods")
dev.off()

#### Plot world map ####
## Plot generic occurrences
pdf(file = "figures/final/supplemental/genera_50km_brachiopod_map.pdf")
map("world",col="lightgrey",fill = T)
points(x = genera[which(genera$phylum=="Brachiopoda"),"longitude"],y = genera[which(genera$phylum=="Brachiopoda"),"latitude"], cex = 0.5, pch = 15, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
dev.off()

pdf(file = "figures/final/supplemental/genera_50km_bivalve_map.pdf")
map("world",col="lightgrey",fill = T)
points(x = genera[which(genera$phylum=="Mollusca"),"longitude"],y = genera[which(genera$phylum=="Mollusca"),"latitude"], cex = 0.5, pch = 16, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
dev.off()

pdf(file = "figures/final/supplemental/species_50km_brachiopod_map.pdf")
map("world",col="lightgrey",fill = T)
points(x = species[which(species$phylum=="Brachiopoda"),"longitude"],y = species[which(species$phylum=="Brachiopoda"),"latitude"], cex = 0.5, pch = 15, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
dev.off()

pdf(file = "figures/final/supplemental/species_50km_bivalve_map.pdf")
map("world",col="lightgrey",fill = T)
points(x = species[which(species$phylum=="Mollusca"),"longitude"],y = species[which(species$phylum=="Mollusca"),"latitude"], cex = 0.5, pch = 16, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
dev.off()


