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
master_50 <- rasterOccData(occData = master, res = 50000, xyCoords = c("paleolong_average","paleolat_average"), xyCell = c("cellx_50km","celly_50km"), uniqID = "cell_50km")[,c(40:42)]
master_100 <- rasterOccData(occData = master, res = 100000, xyCoords = c("paleolong_average","paleolat_average"), xyCell = c("cellx_100km","celly_100km"), uniqID = "cell_100km")[,c(40:42)]
master_150 <- rasterOccData(occData = master, res = 150000, xyCoords = c("paleolong_average","paleolat_average"), xyCell = c("cellx_150km","celly_150km"), uniqID = "cell_150km")[,c(40:42)]
master_200 <- rasterOccData(occData = master, res = 200000, xyCoords = c("paleolong_average","paleolat_average"), xyCell = c("cellx_200km","celly_200km"), uniqID = "cell_200km")[,c(40:42)]

## Combine
master <- cbind(master,master_50, master_100, master_150, master_200)

## Export rasterised data
saveRDS(master, file = "data/final/master_2_1.Rds")
