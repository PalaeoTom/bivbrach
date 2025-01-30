## 1.5 Non-PBDB formation and genera cleaning

## Load libraries
packages <- c("")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}

## Clean directory
rm(list = ls())

#### Load formations ####
## Start with PBDB formations for reference
PBDB_cleaned <- read.csv("data/metadata/AC_cleaned_PBDB_formations.csv", row.names = 1)[,c(2,4)]

## Add PBDB label
PBDB_cleaned$origin <- "PBDB"
PBDB_cleaned <- PBDB_cleaned[,c(3,1,2)]


