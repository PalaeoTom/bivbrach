#### Not used - filtering PBDB data to remove richest/most depauperate references and those without environmental data ####

## Filter out entries with no ecology data
PBDB_eco <- PBDB[which(!is.na(PBDB[,"ecological_cat"])),]

##### Pruning out bottom 5%/top 5% richness references for reference sensitivity test ####
## Get quantiles of richness (genus and unique_name)
source("functions/refine.references.R")

## Run the function
genera_RefRef <- refine.references(data = PBDB, level = "genus", quantiles = c(0.05, 0.95), drop.ref.singletons.first = T)
