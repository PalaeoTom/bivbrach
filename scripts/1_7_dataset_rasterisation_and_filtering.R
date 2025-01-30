## 1.7 Rasterisation and combination

## If packages aren't installed, install them, then load them
packages <- c("rnaturalearth", "rnaturalearthdata")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(rnaturalearth)
library(rnaturalearthdata)
library(divvyCompanion)

#### Set up ####


#### Rasterisation ####
## Rasterise data using function
genera_200 <- rasterOccData(occData = PBDB, res = 200000)

genera_eco_200 <- rasterOccData(occData = PBDB_eco, res = 200000)

genera_200_RefRef <- rasterOccData(occData = genera_RefRef, res = 200000)

## Export polished files
saveRDS(genera_200, file = "data/PBDB/genera_200.Rds")

