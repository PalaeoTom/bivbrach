## 2.1 Rasterisation

## If packages aren't installed, install them, then load them
packages <- c("remotes")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(remotes)

## install divvyCompanion from github and load
#remotes::install_github("PalaeoTom/divvyCompanion")
library(divvyCompanion)

## Clean
rm(list=ls())

#### Read in data ####
master <- readRDS("data/final/master.Rds")

## Rasterise data using divvyCompanion function
master_200 <- rasterOccData(occData = master, res = 200000, xyCoords = c("paleolong_average","paleolat_average"), xyCell = c("cellx_200km","celly_200km"), uniqID = "cell_200km")[,c(40:42)]
master_500 <- rasterOccData(occData = master, res = 500000, xyCoords = c("paleolong_average","paleolat_average"), xyCell = c("cellx_500km","celly_500km"), uniqID = "cell_500km")[,c(40:42)]
master_1000 <- rasterOccData(occData = master, res = 1000000, xyCoords = c("paleolong_average","paleolat_average"), xyCell = c("cellx_1000km","celly_1000km"), uniqID = "cell_1000km")[,c(40:42)]

## Recombine
master_out <- cbind(master,master_200,master_500,master_1000)

## Export rasterised data
saveRDS(master_out, file = "data/final/master_2_1.Rds")
