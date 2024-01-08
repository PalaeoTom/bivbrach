#### Initial analysis ####
## Started by TJS on 08/01/2024 ##

## Clean directory
rm(list = ls())

## If Divvy isn't installed, install it, then load it
if(length(c("divvy", "terra")[!c("divvy", "terra") %in% installed.packages()[,"Package"]]) > 0){
  install.packages(c("divvy", "terra")[!c("divvy", "terra") %in% installed.packages()[,"Package"]])
}
library(divvy)
library(terra)

## Load data
brach <- readRDS("data/brach.Rds")
biv <- readRDS("data/biv.Rds")

## Load package data to assess structure
library(divvy)
data('bivalves')

# initialise Equal Earth projected coordinates
library(terra)
rWorld <- rast()
prj <- 'EPSG:8857'
rPrj <- project(rWorld, prj, res = 200000) # 200,000m is approximately 2 degrees
values(rPrj) <- 1:ncell(rPrj)

# coordinate column names for the current and target coordinate reference system
xyCartes <- c('paleolng','paleolat')
xyCell   <- c('cellX','cellY')

# extract cell number and centroid coordinates associated with each occurrence
llOccs <- vect(bivalves, geom = xyCartes, crs = 'epsg:4326')
prjOccs <- project(llOccs, prj)
bivalves$cell <- cells(rPrj, prjOccs)[,'cell']
bivalves[, xyCell] <- xyFromCell(rPrj, bivalves$cell)

# get unique occurences within each cell - needs to be unique per time bin
nrow(bivalves)
bivalves <- uniqify(bivalves, taxVar = 'genus', xy = xyCell)
nrow(bivalves)

# summarise spatial distribution
sdSumry(bivalves, taxVar = 'genus', xy = xyCell, crs = prj)

# calculate range size
myti <- bivalves[bivalves$genus == 'Mytilus',    xyCell]
yabe <- bivalves[bivalves$genus == 'Yabepecten', xyCell]

rangeSize(myti, crs = prj)
rangeSize(yabe, crs = prj)

# back transformation
yabeVect <- vect(yabe, geom = xyCell, crs = prj)
project(yabeVect, 'epsg:4326')

# cookie cutter
set.seed(1)
circLocs <- cookies(dat = bivalves, xy = xyCell,
                    iter = 10, nSite = 12, r = 1500,
                    crs = prj, output = 'full')
