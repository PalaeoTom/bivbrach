#### Initial data processing ####
## Started by TJS on 08/01/2024 ##

## Load raw data
raw_PBDB <- readRDS("data/PBDB_Nov23.Rds")

## Isolate bivalve and brachiopod data
brach_PBDB <- raw_PBDB[which(raw_PBDB[,12] %in% "Brachiopoda"),]
biv_PBDB <- raw_PBDB[which(raw_PBDB[,13] %in% "Bivalvia"),]

## Combine with other data sources for final data files
brach <- brach_PBDB
biv <- biv_PBDB

## Export polished files
saveRDS(brach, file = "data/brach.Rds")
saveRDS(biv, file = "data/biv.Rds")
