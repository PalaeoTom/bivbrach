## Initial analysis
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("divvy", "terra", "rnaturalearth", "rnaturalearthdata", "ggplot2", "sf", "dplyr", "RColorBrewer")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divvy)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)

## Load data
brach <- readRDS("data/brach.Rds")
biv <- readRDS("data/biv.Rds")

#### Example analysis - 1 time bin ####
# initialise Equal Earth projected coordinates
rWorld <- rast()
prj <- 'EPSG:8857'
rPrj <- project(rWorld, prj, res = 200000) # 200,000m is approximately 2 degrees
values(rPrj) <- 1:ncell(rPrj)

# coordinate column names for the current and target coordinate reference system
xyCartes <- c('paleolng','paleolat')
xyCell   <- c('cellX','cellY')

# extract cell number and centroid coordinates associated with each occurrence
llOccs_brach <- vect(brach, geom = xyCartes, crs = 'epsg:4326')
llOccs_biv <- vect(biv, geom = xyCartes, crs = 'epsg:4326')
prjOccs_brach <- project(llOccs_brach, prj)
prjOccs_biv <- project(llOccs_biv, prj)

# add cell number and coordinate for each occurrence
brach$cell <- cells(rPrj, prjOccs_brach)[,'cell']
brach[, xyCell] <- xyFromCell(rPrj, brach$cell)
biv$cell <- cells(rPrj, prjOccs_biv)[,'cell']
biv[, xyCell] <- xyFromCell(rPrj, biv$cell)

# Get Permian and Triassic data in 10ma time bins (from 300 to 200)
# By default, function will include occurrences dated to MA.start but not MA.end
# Will add ability to flip/change later.
extract.time.bin <- function(data, MA.start, MA.end){
  # get all occurrences that originate in interval
  orig <- data[which(between(data[,"newFAD"], MA.end, MA.start)),]
  # get all occurrences that terminate in interval
  term <- data[which(between(data[,"newLAD"], MA.end, MA.start)),]
  # get all occurrences that span interval
  raw <- c(which(data[,"newFAD"] > MA.start),which(data[,"newLAD"] < MA.end))
  span <- data[raw[duplicated(raw)],]
  # concatenate
  output <- rbind(orig, term, span)
  return(output)
}

# set time intervals
bins <- list(c(300,290.01),
             c(290,280.01),
             c(280,270.01),
             c(270,260.01),
             c(260,250.01),
             c(250,240.01),
             c(240,230.01),
             c(230,220.01),
             c(220,210.01),
             c(210,200.01))

# apply function to get subsets and get unique occurrences
binned.biv <- lapply(1:length(bins), function(x){
  out <- uniqify(extract.time.bin(data = biv, MA.start = bins[[x]][1], MA.end = bins[[x]][2]), taxVar = "genus", xy = xyCell)
})

binned.brach <- lapply(1:length(bins), function(x){
  out <- uniqify(extract.time.bin(data = brach, MA.start = bins[[x]][1], MA.end = bins[[x]][2]), taxVar = "genus", xy = xyCell)
})

#### using cookies  - sample brachiopods and bivalves separately ####
reps <- 500
siteQuota <- 15
r <- 1000

## Sample each time bin
set.seed(8)
samp.biv <- lapply(1:length(binned.biv), function(x){
  samp <- cookies(dat = binned.biv[[x]],
                  xy = xyCell, iter = reps,
                  nSite = siteQuota,
                  r = r, weight = TRUE,
                  crs = prj, output = 'full')
})

set.seed(10)
samp.brach <- lapply(1:length(binned.brach), function(x){
  samp <- cookies(dat = binned.brach[[x]],
                  xy = xyCell, iter = reps,
                  nSite = siteQuota,
                  r = r, weight = TRUE,
                  crs = prj, output = 'full')
})

#### Cookies - sampling brachiopods and bivalves together, plot richness against one another ####
## First, combine time bins
binned.all <- lapply(1:length(bins), function(x) rbind(binned.biv[[x]],binned.brach[[x]]))

## Same parameters
reps <- 1000
siteQuota <- 15
r <- 1000

## Run subsampling
set.seed(2)
samp.all <- lapply(1:length(binned.all), function(x){
  samp <- cookies(dat = binned.all[[x]],
                  xy = xyCell, iter = reps,
                  nSite = siteQuota,
                  r = r, weight = TRUE,
                  crs = prj, output = 'full')
})

## Partition into Brachiopods and Bivalves
samp.all.brach <- lapply(1:length(samp.all), function(x){
  time.bin <- lapply(1:length(samp.all[[1]]), function(y){
    rep <- samp.all[[x]][[y]][which(samp.all[[x]][[y]][,12] %in% "Brachiopoda"),]
  })
})

samp.all.biv <- lapply(1:length(samp.all), function(x){
  time.bin <- lapply(1:length(samp.all[[1]]), function(y){
    rep <- samp.all[[x]][[y]][which(samp.all[[x]][[y]][,13] %in% "Bivalvia"),]
  })
})

## Get richness data for each time bin
brach.rich <- lapply(1:length(samp.all.brach), function(x){
  time.bin <- sapply(1:length(samp.all.brach[[x]]), function(y){
    richness <- length(unique(samp.all.brach[[x]][[y]][["genus"]]))
  })
})

biv.rich <- lapply(1:length(samp.all.biv), function(x){
  time.bin <- sapply(1:length(samp.all.biv[[x]]), function(y){
    richness <- length(unique(samp.all.biv[[x]][[y]][["genus"]]))
  })
})

input1 <- brach.rich
input2 <- biv.rich
threshold = 1

## OPTIONAL - clean out reps in which either taxon is 0 or 1
clean.reps <- function(input1, input2, threshold = 1)
  ## if row in input 1 = 0, drop from both inputs
  input1.index <- lapply(1:length(input), function(x){
    index <- which()
  })

for (i in 1:length(brach.rich)){
which(brach.rich[[i]] == 0)
}

## Plotting ####
## Get axes and color palette
axes <- c(0, max(c(unlist(brach.rich),unlist(biv.rich))))
pal <- colorRampPalette(brewer.pal(9, "PRGn"))(10)

## Plot figure
par(family = "Verdana")
pdf(paste0("biv_brach_richness.pdf"))
par(xpd = F)
plot(x = NULL, y = NULL, xlim = axes, ylim = axes, axes = F, xaxs ="i", yaxs = "i", ylab = "Bivalve generic richness", xlab = "Brachiopod generic richness")
## Points for each time bin
for (i in 1:length(brach.rich)){
  points(x = brach.rich[[i]], y = biv.rich[[i]], col = pal[i])
}
## Plot axes
axis(side = 2, las = 2, seq(axes[1], axes[2], 50))
axis(side = 1, las = 1, seq(axes[1], axes[2], 50))
## Plot legend
legend("topright", legend = c("300-290Ma", "290-280Ma", "280-270Ma", "270-260Ma", "260-250Ma",
                              "250-240Ma", "240-230Ma", "230-220Ma", "220-210Ma", "210-200Ma"),
       fill = pal)
mtext(text = "Cookies: 1000 reps, 1000km radius, 15 sites")
dev.off()

