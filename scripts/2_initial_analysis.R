## 2. Initial exploratory analysis
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("divvy", "terra", "rnaturalearth", "rnaturalearthdata", "ggplot2", "sf", "dplyr", "plyr", "RColorBrewer", "velociraptr", "lwgeom")
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
library(velociraptr)
library(plyr)
library(lwgeom)

## Load data
setwd("~/R_packages/R_projects/bivbrach")
genera <- readRDS("data/PBDB_BB_genera.Rds")
families <- readRDS("data/PBDB_BB_families.Rds")
orders <- readRDS("data/PBDB_BB_orders.Rds")

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
llOccs_genera <- vect(genera, geom = xyCartes, crs = 'epsg:4326')
llOccs_families <- vect(families, geom = xyCartes, crs = 'epsg:4326')
llOccs_orders <- vect(orders, geom = xyCartes, crs = 'epsg:4326')
prjOccs_genera <- project(llOccs_genera, prj)
prjOccs_families <- project(llOccs_families, prj)
prjOccs_orders <- project(llOccs_orders, prj)

# add cell number and coordinate for each occurrence
genera$cell <- cells(rPrj, prjOccs_genera)[,'cell']
genera[, xyCell] <- xyFromCell(rPrj, genera$cell)

families$cell <- cells(rPrj, prjOccs_families)[,'cell']
families[, xyCell] <- xyFromCell(rPrj, families$cell)

orders$cell <- cells(rPrj, prjOccs_orders)[,'cell']
orders[, xyCell] <- xyFromCell(rPrj, orders$cell)

# Get Permian and Triassic data in 10ma time bins (from 300 to 200)
# Function samples occurrences that fit within bins (won't include those that exist before or after)
source("functions/extract.time.bin.R")

#### Start here! ####
## First, derive Antell binning scheme
## Download raw stage data and create names column
stages <- downloadTime('international ages')
stages$name <- row.names(stages)

## Re-order stages by age, oldest first
stages <- stages[order(stages$b_age, decreasing=TRUE), ]

## Create columns for new rounded ages
stages$b_round <- stages$t_round <- 0

## Define stages to omit (for various reasons)
stages2omit <- c('Stage 2','Stage 3','Stage 4','Wuliuan',
                 'Drumian','Guzhangian','Paibian','Jiangshanian',
                 'Stage 10',
                 'Floian','Darriwilian',
                 'Katian', # otherwise no seed cells for Sandbian
                 'Aeronian', # otherwise no Rhuddanian or Aeronian seed cells
                 'Homerian','Ludfordian','Pragian','Eifelian','Bashkirian','Kasimovian',
                 'Sakmarian','Kungurian', # no Artinskian species records
                 'Olenekian', # otherwise only 1 abundance datum for Olenekian
                 'Sinemurian', # Hettangian is too poorly sampled
                 'Bajocian', # otherwise no Aalenian seed cells
                 'Hauterivian','Barremian', # no seed cells for Haut., Barremian or Valanginian alone
                 'Santonian', # otherwise nothing survives from Coniacian
                 'Thanetian',
                 'Bartonian', # otherwise no environmental data for Bartonian
                 'Aquitanian', # otherwise no seeds here or in Chattian
                 'Serravallian', # otherwise no seed cells for Langhian
                 'Messinian', # otherwise no seed cells for Messinian
                 'Calabrian','Middle Pleistocene','Late Pleistocene', # otherwise weird extinction rates
                 'Northgrippian','Meghalayan') # lump all Holocene records so they're easy to remove later

## Create stages object with listed stages omitted
stages_trunc <- stages[!(stages$name %in% stages2omit),] #remove lumped stages

## Read in function for defining rounded stage ages
source("functions/round.age.R")

## Define groupings for rounding
groupings <- list(u10 <- which(stages_trunc$b_age < 10),
                  u150 <- which(stages_trunc$b_age < 150 & stages_trunc$b_age > 10),
                  old <- which(stages_trunc$b_age > 150))

## Round ages
for (group in 1:length(groupings)){
  bins <- groupings[[group]]
  digits <- c(2, 1, 0)[group]

  # round down younger boundary (terminus) and up older boundary (beginning) per stage
  for (i in bins){
    b <- stages_trunc$b_age[i]
    t <- stages_trunc$b_age[i+1]
    stages_trunc$b_round[i] <- round.age(b, digits=digits, round_up=TRUE)
    stages_trunc$t_round[i] <- round.age(t, digits=digits, round_up=FALSE)
  }
}

## Bin data by stage for genera, families, and orders
source("functions/extract.stage.bin.R")
source("functions/extract.time.bin.R")
source("functions/get.bins.R")
source("functions/bin.data.R")

## Uniqifying by default
stages.genera <- bin.data(occs = genera, trunc.stages = stages_trunc, complete.stages = stages)
stages.families <- bin.data(occs = families, trunc.stages = stages_trunc, complete.stages = stages, uniqify.taxVar = "family")
stages.orders <- bin.data(occs = orders, trunc.stages = stages_trunc, complete.stages = stages, uniqify.taxVar = "order")

## Get min/max for datasets
source("functions/get.min.max.R")
genera.mm <- get.min.max(data = genera)
families.mm <- get.min.max(data = families)
orders.mm <- get.min.max(data = orders)

## Get 10Ma time bins
bin10.genera <- bin.data(occs = genera, max_time = genera.mm[1], min_time = genera.mm[2], bin_size = 10)
bin10.families <- bin.data(occs = families, max_time = families.mm[1], min_time = families.mm[2], bin_size = 10)
bin10.orders <- bin.data(occs = orders, max_time = orders.mm[1], min_time = orders.mm[2], bin_size = 10)

## Get richness values
source("functions/findPool2.R")
source("functions/findSeeds2.R")
source("functions/getOverlap.R")
source("functions/cookie.R")
source("functions/cut.cookies.R")


stages.g.rich <- cut.cookies(data = stages.genera, reps = 10, siteQuota = 15, r = 1000, c.crs = prj, taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))
stages.f.rich <- cut.cookies(data = stages.families, reps = 10, siteQuota = 15, r = 1000, c.crs = prj, taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))
stages.o.rich <- cut.cookies(data = stages.orders, reps = 10, siteQuota = 15, r = 1000, c.crs = prj, taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))

bin10.g.rich <- cut.cookies(data = bin10.genera, reps = 10, siteQuota = 15, r = 1000, c.crs = prj, taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))
bin10.f.rich <- cut.cookies(data = bin10.families, reps = 10, siteQuota = 15, r = 1000, c.crs = prj, taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))
bin10.o.rich <- cut.cookies(data = bin10.orders, reps = 10, siteQuota = 15, r = 1000, c.crs = prj, taxa = c("Brachiopoda","Bivalvia"), taxa.level = c("phylum","class"))

## Get bins as data frames
genera.bins <- t(data.frame(get.bins(max.t = genera.mm[1], min.t = genera.mm[2], bin.s = 10)))
families.bins <- t(data.frame(get.bins(max.t = families.mm[1], min.t = families.mm[2], bin.s = 10)))
orders.bins <- t(data.frame(get.bins(max.t = orders.mm[1], min.t = orders.mm[2], bin.s = 10)))

## isolate stage times as a data frame
stage.bins <- data.frame(stages_trunc$b_round, stages_trunc$t_round)

## get midpoints
source("functions/get.midpoints.R")

data = stages.g.rich
midpoints = get.midpoints(stage.bins)
taxa = c("Brachiopoda","Bivalvia")

## Function for re-formatting data into data frame with three columns: time (midpoint Ma of interval), brachiopod richness, bivalve richness
format.cookies <- function(data, midpoints, taxa = NULL){
  ## if box of cookies is partitioned
  if(!is.null(taxa)){


  }

}



## Then, small function for getting correlating during each bin

## Then plot correlation coefficients for each bin, highlighting significant ones



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

