## 1. Initial data processing
## Started by TJS on 08/01/2024

## GBIF access citation
# GBIF.org (20 May 2024) GBIF Occurrence Download  https://doi.org/10.15468/dl.3xb65c

## Clean directory
rm(list = ls())

## Set working directory
setwd("~/R_packages/bivbrach")
home <- getwd()

## If packages aren't installed, install them, then load them
packages <- c("fossilbrush", "rnaturalearth", "rnaturalearthdata", "terra", "divDyn", "iNEXT", "divvy", "rgbif", "usethis", "bit64")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(fossilbrush)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
library(divDyn)
library(iNEXT)
library(divvy)
library(rgbif)
library(bit64)

## install divvyCompanion from github and load
library(remotes)
install_github("PalaeoTom/divvyCompanion")
library(divvyCompanion)

## Load raw data
raw_PBDB <- readRDS("data/PBDB_Nov23.Rds")

## Set GBIF username
#library(usethis)
#usethis::edit_r_environ()

## download and then save raw GBIF data
## get taxon keys
#bivalve_key <- as.integer(name_backbone("Bivalvia")[,1])
#brachiopod_key <- as.integer(name_backbone("Brachiopoda")[,1])

## register download request
raw_GBIF_bivalves <- occ_download(pred('taxonKey', bivalve_key))
raw_GBIF_brachiopoda <- occ_download(pred('taxonKey', brachiopod_key))

## check if complete
#occ_download_wait(raw_GBIF_brachiopoda, status_ping = 5, curlopts = list(), quiet = FALSE)
#occ_download_wait(raw_GBIF_bivalves, status_ping = 5, curlopts = list(), quiet = FALSE)

## download when complete
brachiopod.download.path <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/GBIF/brachiopods"
bivalve.download.path <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/GBIF/bivalves"
#occ_download_get(raw_GBIF_bivalves, path = bivalve.download.path)
#occ_download_get(raw_GBIF_brachiopoda, path = brachiopod.download.path)

## read GBIF data into R
raw_GBIF_biv <- occ_download_import(as.download(path = paste0(bivalve.download.path, "/May_24.zip")))
raw_GBIF_brach <- occ_download_import(as.download(path = paste0(brachiopod.download.path, "/May_24.zip")))

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

#### Screening data and removing subquality entries ####
## Both data frames
## Drop entries missing relevant taxonomy: codes as gaps or NAs.
## For phyla, classes, and genera
no_phy_gen <- unique(c(which(PBDB$phylum == ""),
                           which(PBDB$class == ""),
                           which(PBDB$genus == ""),
                           which(is.na(PBDB$phylum)),
                           which(is.na(PBDB$class)),
                           which(is.na(PBDB$genus))))
if(length(no_phy_gen) > 0){
  PBDB_genera <- PBDB[-no_phy_gen,]
} else {
  PBDB_genera <- PBDB
}

## For phyla, classes, and species
no_phy_spec <- unique(c(which(PBDB$phylum == ""),
                       which(PBDB$class == ""),
                       which(PBDB$species == ""),
                       which(is.na(PBDB$phylum)),
                       which(is.na(PBDB$class)),
                       which(is.na(PBDB$species))))
if(length(no_phy_spec) > 0){
  PBDB_species <- PBDB[-no_phy_spec,]
} else {
  PBDB_species <- PBDB
}

## Species only
## Check accepted ranks
unique(PBDB$accepted_rank)

## Data only contains genus and species level entries. Start with identified rank
PBDB_species <- PBDB_species[PBDB_species$identified_rank=="species",]

## Clean up identified names, drop those not identified to species level
PBDB_species$short_name <- cleansp(PBDB_species$identified_name, misspells=T, stems=T)
PBDB_species <- PBDB_species[!is.na(PBDB_species$short_name),]

## Clean up accepted names, drop points not accepted
PBDB_species$accepted_name <- cleansp(PBDB_species$accepted_name, misspells=T, stems=T)
PBDB_species <- PBDB_species[!is.na(PBDB_species$accepted_name),]

## Define unique name combining phyla and accepted name. Should be complete as only working with accepted names, so no need to use short names
PBDB_species$unique_name <- paste(PBDB_species$phylum, PBDB_species$accepted_name)
#unac <- which(!PBDB_species$accepted_rank %in% "species")
#PBDB_species$unique_name[unac] <- paste(PBDB_species$phylum[unac], PBDB_species$short_name[unac])

#### Add reference and collection composition identifiers ####
source("functions/add.collection.IDs.R")
source("functions/add.reference.IDs.R")

PBDB_species <- add.collection.IDs(data = PBDB_species)
PBDB_species <- add.reference.IDs(data = PBDB_species)

PBDB_genera <- add.collection.IDs(data = PBDB_genera)
PBDB_genera <- add.reference.IDs(data = PBDB_genera)


#### Determine environment of each cell ####
## Doing this before standardising data in each cell (and losing more data)
## Define variables for search
shallow <- c("coastal indet.", "delta front", "delta plain",
             "deltaic indet.", "estuary/bay", "foreshore", "interdistributary bay",
             "lagoonal", "lagoonal/restricted shallow subtidal",
             "marginal marine indet.", "open shallow subtidal", "fluvial-deltaic indet.",
             "paralic indet.", "peritidal", "prodelta", "sand shoal",
             "shallow subtidal indet.", "shoreface", "transition zone/lower shoreface",
             "intrashelf/intraplatform reef", "reef, buildup or bioherm",
             "perireef or subreef", "platform/shelf-margin reef")
deep <- c("basinal (carbonate)", "basinal (siliceous)", "basinal (siliciclastic)",
          "deep-water indet.", "deep subtidal indet.", "deep subtidal ramp",
          "deep subtidal shelf", "offshore", "offshore indet.",
          "offshore shelf", "slope", "submarine fan", "offshore ramp",
          "basin reef", "slope/ramp reef")

## categorise each record for each enviro type
b.species <- rep(NA, nrow(PBDB_species))
b.genera <- rep(NA, nrow(PBDB_genera))

### Genera
## Bathymetry (p = proximal = shallow)
b.genera[PBDB_genera$environment %in% shallow] <- "p"
b.genera[PBDB_genera$environment %in% deep] <- "d"

env_axes <- "bathnow"
PBDB_genera <- cbind(PBDB_genera, data.frame('bathnow'=b.genera, stringsAsFactors=TRUE))

### Species
## Bathymetry (p = proximal = shallow)
b.species[PBDB_species$environment %in% shallow] <- "p"
b.species[PBDB_species$environment %in% deep] <- "d"

env_axes <- 'bathnow'
PBDB_species <- cbind(PBDB_species, data.frame('bathnow'=b.species, stringsAsFactors=TRUE))

#### Rasterising data ####
## Rasterise data using function
genera_200 <- rasterOccData(occData = PBDB_genera, res = 200000)
genera_100 <- rasterOccData(occData = PBDB_genera, res = 100000)
species_200 <- rasterOccData(occData = PBDB_species, res = 200000)
species_100 <- rasterOccData(occData = PBDB_species, res = 100000)

## Export polished files
saveRDS(genera_200, file = "data/genera_200.Rds")
saveRDS(species_200, file = "data/species_200.Rds")
saveRDS(genera_100, file = "data/genera_100.Rds")
saveRDS(species_100, file = "data/species_100.Rds")

