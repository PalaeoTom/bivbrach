## 1. Initial data processing
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## Set working directory
setwd("~/R_packages/R_projects/bivbrach")
home <- getwd()

## If packages aren't installed, install them, then load them
packages <- c("fossilbrush", "velociraptr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(fossilbrush)
library(velociraptr)

## Load raw data
raw_PBDB <- readRDS("data/PBDB_Nov23.Rds")

## Isolate bivalve and brachiopod data
raw_PBDB <- raw_PBDB[c(which(raw_PBDB$phylum == "Brachiopoda"),which(raw_PBDB$class == "Bivalvia")),]

#### Cleaning time data ####
## use fossilbrush to update Chronostratigraphy
PBDB <- chrono_scale(raw_PBDB,  tscale = "GTS2020", srt = "early_interval", end = "late_interval",
                           max_ma = "max_ma", min_ma = "min_ma", verbose = FALSE)

## set new chronostratigraphy as "max_ma" and "min_ma", then remove added columns newFAD and newLAD
PBDB$max_ma <- PBDB$newFAD
PBDB$min_ma <- PBDB$newLAD
PBDB <- PBDB[,-c(29,30)]

## check for and remove any entries with nonsensical entries (LAD older than FAD)
if(any(PBDB$max_ma < PBDB$min_ma)){
  PBDB <- PBDB[-which(PBDB$max_ma < PBDB$min_ma),]
}

#### Cleaning taxonomy ####
## Replace PBDB default missing data entry with NA
PBDB[grep("NO_", PBDB[,"phylum"]), "phylum"] <- NA
PBDB[grep("NO_", PBDB[,"class"]), "class"] <- NA
PBDB[grep("NO_", PBDB[,"order"]), "order"] <- NA
PBDB[grep("NO_", PBDB[,"family"]), "family"] <- NA
PBDB[grep("NO_", PBDB[,"genus"]), "genus"] <- NA

## Cleaning previously-identified (using divDyn cleansp function - when it didn't work!) special characters - umlaut
## Get all binomials beginning with Inoceramus sch
inoceramus_sch <- grep("Inoceramus sch",PBDB[,5], useBytes = T)

## Remove other 3 then replace remaining binomials with R friendly spelling
inoceramus_sch <- inoceramus_sch[!inoceramus_sch %in% grep("Inoceramus schluetheri",PBDB[,5], useBytes = T)]
inoceramus_sch <- inoceramus_sch[!inoceramus_sch %in% grep("Inoceramus schoendorfi",PBDB[,5], useBytes = T)]
inoceramus_sch <- inoceramus_sch[!inoceramus_sch %in% grep("Inoceramus schmidti",PBDB[,5], useBytes = T)]
PBDB[inoceramus_sch,5] <- "Inoceramus schoendorfi"

## Use misspell function to update genus, family, and order designations
source("functions/misspell.R")
PBDB$genus <- misspell(PBDB$genus)
PBDB$family <- misspell(PBDB$family)
PBDB$order <- misspell(PBDB$order)

## Set ranks for cleaning and acceptable suffixes to be used in dataset
b_ranks <- c("phylum", "class", "order", "family", "genus")
b_suff = list(NULL, NULL, NULL, NULL, c("ina", "ella", "etta"))

## Now check for taxonomy issues using check_taxonomy
fb_breakdown <- check_taxonomy(PBDB, suff_set = b_suff, ranks = b_ranks, clean_name = TRUE, resolve_duplicates = TRUE, jump = 5)

## Synonyms not a problem
## No cross-rank names
## 9 duplicate taxonomies will be handled by check_taxonomy

## Now identified issues have been corrected, produce final dataset
PBDB <- check_taxonomy(PBDB, suff_set = b_suff, ranks = b_ranks, clean_name = TRUE, resolve_duplicates = TRUE, jump = 5)$data

## Note: using check_taxonomy to resolve conflicting higher taxonomies for genera is easy but seems to produce nonsense taxonomies
## If retention of this structure is important, advisable to switch this feature off
## However, for richness/rates analysis, not a problem.

## Can check frequency density distributions (rest of fossilbrush package functionality)
## However, should speak Erin with first for her opinion.

#### Preparing datasets for export ####
## Drop entries missing relevant taxonomy: codes as gaps or NAs.
## For genera
no_phy_gen <- unique(c(which(PBDB$phylum == ""),
                           which(PBDB$genus == ""),
                           which(is.na(PBDB$phylum)),
                           which(is.na(PBDB$genus))))
if(length(no_phy_gen) > 0){
  PBDB_genera <- PBDB[-no_phy_gen,]
} else {
  PBDB_genera <- PBDB
}

## For families
no_phy_fam <- unique(c(which(PBDB$phylum == ""),
                       which(PBDB$family == ""),
                       which(is.na(PBDB$phylum)),
                       which(is.na(PBDB$family))))
if(length(no_phy_fam) > 0){
  PBDB_families <- PBDB[-no_phy_fam,]
} else {
  PBDB_families <- PBDB
}

## For orders
no_phy_ord <- unique(c(which(PBDB$phylum == ""),
                       which(PBDB$order == ""),
                       which(is.na(PBDB$phylum)),
                       which(is.na(PBDB$order))))
if(length(no_phy_ord) > 0){
  PBDB_orders <- PBDB[-no_phy_ord,]
} else {
  PBDB_orders <- PBDB
}

#### Other data sources ####
## TBC

#### Final dataset preparation ####
## TBC

## Export polished files
saveRDS(PBDB_genera, file = "data/PBDB_BB_genera.Rds")
saveRDS(PBDB_families, file = "data/PBDB_BB_families.Rds")
saveRDS(PBDB_orders, file = "data/PBDB_BB_orders.Rds")

