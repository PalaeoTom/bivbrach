## 3.3 Adding temperature and bathymetry data to cells
## Started by TJS on 06/08/2025

#### Set up - run before any subsection ####
## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("terra", "stringr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(terra)
library(stringr)

## Move to home directory and set
setwd("~/R_packages/bivbrach")
home <- getwd()

## Read in data to be analysed - moving forward with just the noCov data
genera_CRV <- read.csv("data/analysis_data/genera_NC_CRV.csv", header = T, row.names = 1)
species_CRV <- read.csv("data/analysis_data/species_NC_CRV.csv", header = T, row.names = 1)
genera_CR20 <- read.csv("data/analysis_data/genera_NC_CR20.csv", header = T, row.names = 1)
species_CR20 <- read.csv("data/analysis_data/species_NC_CR20.csv", header = T, row.names = 1)

## Read in time binning scheme
bins <- read.csv("data/metadata/binning_timescale.csv", row.names = 1, header = T)
bins$ID <- seq(1,nrow(bins),1)
bins$interval <- str_replace(bins$interval, " ", "_")
bins$interval <- str_replace(bins$interval, "-", "_")

## Read in climate metadata
clim <- lapply(1:nrow(bins), function(x){
  dat <- read.csv(paste0("data/metadata/clim_data_cleaned/",bins[x,"interval"],".csv"), header = T)
})
names(clim) <- bins$ID

#### Rasterise climate data ####
## Define grid used to rasterise occurrence data
rPrj <- terra::project(x = terra::rast(), y = 'EPSG:8857', res = 100000)
terra::values(rPrj) <- 1:terra::ncell(rPrj)

# Add cell numbers to climate data
for(x in 1:length(clim)){
  clim[[x]]$cell <- NA
  climSV <- terra::vect(clim[[x]], geom = c('paleolongitude','paleolatitude'), crs = 'EPSG:4326')
  climSVp <- terra::project(climSV, 'EPSG:8857')
  clim[[x]][,"cell"] <- terra::cells(rPrj, climSVp, )[,"cell"]
}

#### Map climate data to genera and species grid cells ####
## Create containers
genera_CRV$temp <- NA
genera_CRV$bath <- NA
species_CRV$temp <- NA
species_CRV$bath <- NA

genera_CR20$temp <- NA
genera_CR20$bath <- NA
species_CR20$temp <- NA
species_CR20$bath <- NA

## Pass over each stage and add data
for(i in unique(genera_CRV$stage)){
  ## find index of cells in this stage
  cell.ind <- which(genera_CRV$stage == i)
  ## populate
  genera_CRV[cell.ind,"temp"] <- clim[[i]][match(genera_CRV[cell.ind,"cell"],clim[[i]][,"cell"]),"temp"]
  genera_CRV[cell.ind,"bath"] <- clim[[i]][match(genera_CRV[cell.ind,"cell"],clim[[i]][,"cell"]),"depth"]
}

for(i in unique(species_CRV$stage)){
  ## find index of cells in this stage
  cell.ind <- which(species_CRV$stage == i)
  ## populate
  species_CRV[cell.ind,"temp"] <- clim[[i]][match(species_CRV[cell.ind,"cell"],clim[[i]][,"cell"]),"temp"]
  species_CRV[cell.ind,"bath"] <- clim[[i]][match(species_CRV[cell.ind,"cell"],clim[[i]][,"cell"]),"depth"]
}

## Now do the same for CR20 data
for(i in unique(genera_CR20$stage)){
  ## find index of cells in this stage
  cell.ind <- which(genera_CR20$stage == i)
  ## populate
  genera_CR20[cell.ind,"temp"] <- clim[[i]][match(genera_CR20[cell.ind,"cell"],clim[[i]][,"cell"]),"temp"]
  genera_CR20[cell.ind,"bath"] <- clim[[i]][match(genera_CR20[cell.ind,"cell"],clim[[i]][,"cell"]),"depth"]
}

for(i in unique(species_CR20$stage)){
  ## find index of cells in this stage
  cell.ind <- which(species_CR20$stage == i)
  ## populate
  species_CR20[cell.ind,"temp"] <- clim[[i]][match(species_CR20[cell.ind,"cell"],clim[[i]][,"cell"]),"temp"]
  species_CR20[cell.ind,"bath"] <- clim[[i]][match(species_CR20[cell.ind,"cell"],clim[[i]][,"cell"]),"depth"]
}

## Check for percentage of populated cells
length(which(!is.na(genera_CRV$temp)))/nrow(genera_CRV) ## 48.5% of cells have information.
length(which(!is.na(species_CRV$temp)))/nrow(species_CRV) ## 44.5% of cells have information

## Drop cells with no information
genera_CRV_OCovO <- genera_CRV[intersect(which(!is.na(genera_CRV$temp)), which(!is.na(genera_CRV$bath))),]
species_CRV_OCovO <- species_CRV[intersect(which(!is.na(species_CRV$temp)), which(!is.na(species_CRV$bath))),]
genera_CR20_OCovO <- genera_CR20[intersect(which(!is.na(genera_CR20$temp)), which(!is.na(genera_CR20$bath))),]
species_CR20_OCovO <- species_CR20[intersect(which(!is.na(species_CR20$temp)), which(!is.na(species_CR20$bath))),]

## Export
write.csv(genera_CRV_OCovO, file = "data/analysis_data/genera_MCO_CRV.csv")
write.csv(species_CRV_OCovO, file = "data/analysis_data/species_MCO_CRV.csv")
write.csv(genera_CR20_OCovO, file = "data/analysis_data/genera_MCO_CR20.csv")
write.csv(species_CR20_OCovO, file = "data/analysis_data/species_MCO_CR20.csv")

#### Use bilinear interpolation to fill the gaps ####
## Do it separately for temperature and bathymetry, for each stage
## Create vector of NAs the length of number of cells in rPrj
## Assign temp/bath values to copy of rPrj
## Get coordinates for each cell in rPrj (as.points function on rPrj)
## Use extract on copy of rPrj, trying to get values for each cell.
## Create data frame of cell numbers from rPrj and temp/bath values from copy for each stage
## Map again using function above. Double check there is a match across cells with data in both MCO and MAICO to be sure.

## Get coordinates as points
coords <- as.points(rPrj)

## Make a list of rPrj copies, one for each stage, where each cell is assigned a value if possible.
## Then use extract to fill in the gaps
clim2 <- lapply(1:length(clim), function(x){
  ## Get copy of rPrj
  rastT <- rastB <- rPrj
  ## Get clim of reference
  ref <- clim[[x]]
  ## get temp and bath data
  values(rastT) <- ref[match(values(rPrj),ref[,"cell"]),"temp"]
  values(rastB) <- ref[match(values(rPrj),ref[,"cell"]),"depth"]
  ## Try extract
  temp <- extract(rastT, coords, ID = F, method = "bilinear")
  bath <- extract(rastB, coords, ID = F, method = "bilinear")
  ## create output
  output <- cbind(values(rPrj), temp, bath)
  colnames(output) <- c("cell", "temp", "bath")
  return(output)
})

## Now to map - first, lets get a clean copy of each data frame
genera_CRV_BI <- read.csv("data/analysis_data/genera_NC_CRV.csv", header = T, row.names = 1)
species_CRV_BI <- read.csv("data/analysis_data/species_NC_CRV.csv", header = T, row.names = 1)
genera_CR20_BI <- read.csv("data/analysis_data/genera_NC_CR20.csv", header = T, row.names = 1)
species_CR20_BI <- read.csv("data/analysis_data/species_NC_CR20.csv", header = T, row.names = 1)

## Create containers
genera_CRV_BI$temp <- NA
genera_CRV_BI$bath <- NA
species_CRV_BI$temp <- NA
species_CRV_BI$bath <- NA

genera_CR20_BI$temp <- NA
genera_CR20_BI$bath <- NA
species_CR20_BI$temp <- NA
species_CR20_BI$bath <- NA

## Pass over each stage and use clim 2
for(i in unique(genera_CRV_BI$stage)){
  print(i)
  ## find index of cells in this stage
  cell.ind <- which(genera_CRV_BI$stage == i)
  ## populate
  genera_CRV_BI[cell.ind,"temp"] <- clim2[[i]][match(genera_CRV_BI[cell.ind,"cell"],clim2[[i]][,"cell"]),"temp"]
  genera_CRV_BI[cell.ind,"bath"] <- clim2[[i]][match(genera_CRV_BI[cell.ind,"cell"],clim2[[i]][,"cell"]),"bath"]
}

for(i in unique(species_CRV_BI$stage)){
  ## find index of cells in this stage
  cell.ind <- which(species_CRV_BI$stage == i)
  ## populate
  species_CRV_BI[cell.ind,"temp"] <- clim2[[i]][match(species_CRV_BI[cell.ind,"cell"],clim2[[i]][,"cell"]),"temp"]
  species_CRV_BI[cell.ind,"bath"] <- clim2[[i]][match(species_CRV_BI[cell.ind,"cell"],clim2[[i]][,"cell"]),"bath"]
}

## Now do the same for CR20 data
for(i in unique(genera_CR20_BI$stage)){
  ## find index of cells in this stage
  cell.ind <- which(genera_CR20_BI$stage == i)
  ## populate
  genera_CR20_BI[cell.ind,"temp"] <- clim2[[i]][match(genera_CR20_BI[cell.ind,"cell"],clim2[[i]][,"cell"]),"temp"]
  genera_CR20_BI[cell.ind,"bath"] <- clim2[[i]][match(genera_CR20_BI[cell.ind,"cell"],clim2[[i]][,"cell"]),"bath"]
}

for(i in unique(species_CR20_BI$stage)){
  ## find index of cells in this stage
  cell.ind <- which(species_CR20_BI$stage == i)
  ## populate
  species_CR20_BI[cell.ind,"temp"] <- clim2[[i]][match(species_CR20_BI[cell.ind,"cell"],clim2[[i]][,"cell"]),"temp"]
  species_CR20_BI[cell.ind,"bath"] <- clim2[[i]][match(species_CR20_BI[cell.ind,"cell"],clim2[[i]][,"cell"]),"bath"]
}

## Check for percentage of populated cells
length(which(!is.na(genera_CRV_BI$temp)))/nrow(genera_CRV_BI) ## 63.48% of cells have information
length(which(!is.na(species_CRV_BI$temp)))/nrow(species_CRV_BI) ## 61.1% of cells have information

## Drop cells with no information
genera_CRV_BI_out <- genera_CRV_BI[intersect(which(!is.na(genera_CRV_BI$temp)), which(!is.na(genera_CRV_BI$bath))),]
species_CRV_BI_out<- species_CRV_BI[intersect(which(!is.na(species_CRV_BI$temp)), which(!is.na(species_CRV_BI$bath))),]
genera_CR20_BI_out <- genera_CR20_BI[intersect(which(!is.na(genera_CR20_BI$temp)), which(!is.na(genera_CR20_BI$bath))),]
species_CR20_BI_out <- species_CR20_BI[intersect(which(!is.na(species_CR20_BI$temp)), which(!is.na(species_CR20_BI$bath))),]

## Export
write.csv(genera_CRV_BI_out, file = "data/analysis_data/genera_BIntC_CRV.csv")
write.csv(species_CRV_BI_out, file = "data/analysis_data/species_BIntC_CRV.csv")
write.csv(genera_CR20_BI_out, file = "data/analysis_data/genera_BIntC_CR20.csv")
write.csv(species_CR20_BI_out, file = "data/analysis_data/species_BIntC_CR20.csv")


