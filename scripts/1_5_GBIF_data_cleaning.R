## 1. Initial data processing - GBIF cleaning
## Started by TJS on 08/01/2024

## GBIF access citation
# GBIF.org (20 May 2024) GBIF Occurrence Download  https://doi.org/10.15468/dl.3xb65c

## If packages aren't installed, install them, then load them
packages <- c("fossilbrush", "rnaturalearth", "rnaturalearthdata", "terra", "divDyn", "iNEXT", "divvy", "rgbif", "usethis", "bit64", "dismo", "dplyr", "stringr")
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
library(dismo)
library(dplyr)
library(stringr)

## install divvyCompanion from github and load
#library(remotes)
#install_github("PalaeoTom/divvyCompanion")
library(divvyCompanion)

## Clean directory
rm(list = ls())

#### Load GBIF data ####
## Set GBIF username
#library(usethis)
#usethis::edit_r_environ()

## download and then save raw GBIF data
## get taxon keys
#bivalve_key <- as.integer(name_backbone("Bivalvia")[,1])
#brachiopod_key <- as.integer(name_backbone("Brachiopoda")[,1])

## register download request
#raw_GBIF_bivalves <- occ_download(pred('taxonKey', bivalve_key))
#raw_GBIF_brachiopoda <- occ_download(pred('taxonKey', brachiopod_key))

## check if complete
#occ_download_wait(raw_GBIF_brachiopoda, status_ping = 5, curlopts = list(), quiet = FALSE)
#occ_download_wait(raw_GBIF_bivalves, status_ping = 5, curlopts = list(), quiet = FALSE)

## download when complete
#brachiopod.download.path <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/GBIF/brachiopods"
#bivalve.download.path <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/GBIF/bivalves"
#occ_download_get(raw_GBIF_bivalves, path = bivalve.download.path)
#occ_download_get(raw_GBIF_brachiopoda, path = brachiopod.download.path)

## define complete paths
#brachiopod.download.path <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/GBIF/brachiopods/Sept_24.zip"
#bivalve.download.path <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/GBIF/bivalves/Sept_24.zip"

## set columns to be retained - will refine as we go
#columns.TBR <- c("kingdom", "phylum", "class", "order", "family", "genus", "species", "taxonRank", "taxonomicStatus", "acceptedScientificName",
#                 "decimalLatitude", "decimalLongitude", "hasGeospatialIssues",
#                 "occurrenceStatus", "coordinateUncertaintyInMeters", "issue",
#                 "basisOfRecord",
#                 "institutionCode",
#                 "countryCode", "stateProvince", "county", "municipality", "higherGeography", "locality", "verbatimLocality",
#                 "group", "formation", "member", "bed",
#                 "earliestAgeOrLowestStage", "latestAgeOrHighestStage",
#                 "collectionCode", "gbifID", "bibliographicCitation", "references", "publisher")

## define output directory
#out.dir <- "/Users/tjs/R_packages/bivbrach/data"

## Load function
## Cleans data, retaining needed rows
#source("functions/import.raw.GBIF.R")

## Run function
#GBIF_biv <- data.frame(import.raw.GBIF(bivalve.download.path, columns.TBR, out.dir, export = T, export.name = "GBIF_biv_Oct24"))
#GBIF_brach <- data.frame(import.raw.GBIF(brachiopod.download.path, columns.TBR, out.dir, export = T, export.name = "GBIF_brach_Oct24"))

## Export raw GBIF
#saveRDS(GBIF_biv, file = "data/raw_GBIF_biv_18Oct24.Rds")
#saveRDS(GBIF_brach, file = "data/raw_GBIF_brach_18Oct24.Rds")

## Load raw GBIF
GBIF_biv <- readRDS("data/raw_GBIF_biv_18Oct24.Rds")
GBIF_brach <- readRDS("data/raw_GBIF_brach_18Oct24.Rds")

#### Initial cleaning ####
## Drop useless columns
GBIF_biv <- GBIF_biv[,-c(9, 10, 13, 14, 15, 18, 35, 37)]
GBIF_brach <- GBIF_brach[,-c(9, 10, 13, 14, 15, 18, 35, 37)]

#### Dropping unusable record ####
## Drop data with taxonRank not genus, species, or lower
GBIF_biv <- GBIF_biv[-union(union(union(union(which(GBIF_biv[,"taxonRank"] == "ORDER"),which(GBIF_biv[,"taxonRank"] == "CLASS")),which(GBIF_biv[,"taxonRank"] == "PHYLUM")), which(GBIF_biv[,"taxonRank"] == "FAMILY")), which(GBIF_biv[,"taxonRank"] == "UNRANKED")),]
GBIF_brach <- GBIF_brach[-union(union(union(union(which(GBIF_brach[,"taxonRank"] == "ORDER"),which(GBIF_brach[,"taxonRank"] == "CLASS")),which(GBIF_brach[,"taxonRank"] == "PHYLUM")), which(GBIF_brach[,"taxonRank"] == "FAMILY")), which(GBIF_brach[,"taxonRank"] == "UNRANKED")),]

## Isolate formation information from locality and add if necessary
fms <- str_extract(GBIF_biv$locality, regex("(\\w+ fm)", ignore_case = T))
fms[is.na(fms)] <- ""
#View(data.frame(table(fms)))
## need to prune some nonsense formations out of fm
droppers <- c()
droppers <- c(droppers, which(fms == "and FM"))
droppers <- c(droppers, which(fms == "below FM"))
droppers <- c(droppers, which(fms == "5564 FM"))
droppers <- c(droppers, which(fms == "along FM"))
droppers <- c(droppers, which(fms == "at FM"))
droppers <- c(droppers, which(fms == "corner FM"))
droppers <- c(droppers, which(fms == "cuts FM"))
droppers <- c(droppers, which(fms == "E FM"))
droppers <- c(droppers, which(fms == "form FM"))
droppers <- c(droppers, which(fms == "from FM"))
droppers <- c(droppers, which(fms == "of fm"))
droppers <- c(droppers, which(fms == "of FM"))
droppers <- c(droppers, which(fms == "off FM"))
droppers <- c(droppers, which(fms == "on FM"))
droppers <- c(droppers, which(fms == "S FM"))
droppers <- c(droppers, which(fms == "the FM"))
droppers <- c(droppers, which(fms == "to FM"))
droppers <- c(droppers, which(fms == "TX FM"))
droppers <- c(droppers, which(fms == "with FM"))
fms[droppers] <- ""
formations <- str_extract(GBIF_biv$locality, regex("(\\w+ formation)", ignore_case = T))
## all formations in formations are legit.
formations[is.na(formations)] <- ""

## If gap in formation, add
forms <- paste(fms, formations, sep = " ")
forms[which(forms == " ")] <- ""
tbAdded <- intersect(which(GBIF_biv$formation == ""), which(!forms == ""))
GBIF_biv$formation[tbAdded] <- forms[tbAdded]

## Now for brachiopods
## Isolate formation information from locality and add if necessary
fms <- str_extract(GBIF_brach$locality, regex("(\\w+ fm)", ignore_case = T))
fms[is.na(fms)] <- ""
## need to prune some nonsense formations out of fm
#View(data.frame(table(fms)))
droppers <- c()
droppers <- c(droppers, which(fms == "11 fm"))
droppers <- c(droppers, which(fms == "20 fm"))
droppers <- c(droppers, which(fms == "238 fm"))
droppers <- c(droppers, which(fms == "660 fm"))
droppers <- c(droppers, which(fms == "from FM"))
droppers <- c(droppers, which(fms == "of fm"))
droppers <- c(droppers, which(fms == "of Fm"))
droppers <- c(droppers, which(fms == "the fm"))
droppers <- c(droppers, which(fms == "with FM"))
fms[droppers] <- ""
formations <- str_extract(GBIF_brach$locality, regex("(\\w+ formation)", ignore_case = T))
formations[is.na(formations)] <- ""
#View(data.frame(table(formations)))
droppers <- c()
droppers <- c(droppers, which(formations == "of formation"))
droppers <- c(droppers, which(formations == "the formation"))
droppers <- c(droppers, which(formations == "this formation"))
formations[droppers] <- ""

## If gap in formation, add
forms <- paste(fms, formations, sep = " ")
forms[which(forms == " ")] <- ""
tbAdded <- intersect(which(GBIF_brach$formation == ""), which(!forms == ""))
GBIF_brach$formation[tbAdded] <- forms[tbAdded]

## Starting with bivalves
## Dropping genera
#View(data.frame(table(GBIF_biv$genus)))
## None to drop. No affs, cfs, periods, or question marks.

## Drop miscategorised phyla and classes
#View(data.frame(table(GBIF_biv$phylum)))
#View(data.frame(table(GBIF_biv$class)))
## None to drop! All Mollusca/Bivalvia

## Check for lat/long and locality
droppers <- c()
noLL <- which(is.na(GBIF_biv$decimalLatitude) | is.na(GBIF_biv$decimalLatitude))
noLoc <- which(GBIF_biv$locality == "")
droppers <- c(droppers, intersect(noLL, noLoc))
GBIF_biv <- GBIF_biv[-droppers,]

## Check for formations or stage or age
## First, check for incomplete age categories
#View(data.frame(table(GBIF_biv$earliestAgeOrLowestStage)))
GBIF_biv$earliestAgeOrLowestStage[which(GBIF_biv$earliestAgeOrLowestStage == "Recent")] <- "Holocene"
noEarly <-c()
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == ""))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "Early"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "late"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "Late"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "Lower/Early"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "middle"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "Middle"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "OIS 5"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "OIS 7"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "Unnamed"))
noEarly <- unique(noEarly)
#View(data.frame(table(GBIF_biv$latestAgeOrHighestStage)))
noLate <- c()
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == ""))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "early"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "Early"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "early late"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "early to middle"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "early-middle"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "late"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "Late"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "middle"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "Middle"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "middle-late"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "OIS 5"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "OIS 7"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "Upper"))
noLate <- unique(noLate)

## If missing formations, add to vector
View(data.frame(table(GBIF_biv$formation)))
noForm <- c()
noForm <- c(noForm, which(GBIF_biv$formation == ""))
noForm <- c(noForm, which(GBIF_biv$formation == ""))
noForm <- c(noForm, which(GBIF_biv$formation == ""))
noForm <- c(noForm, which(GBIF_biv$formation == ""))
noForm <- c(noForm, which(GBIF_biv$formation == ""))
noForm <- c(noForm, which(GBIF_biv$formation == ""))
noForm <- c(noForm, which(GBIF_biv$formation == ""))
noForm <- c(noForm, which(GBIF_biv$formation == ""))
noForm <- c(noForm, which(GBIF_biv$formation == ""))

## Use misspell function to update genus designations before checking of taxonomy
source("functions/misspell.R")
GBIF_biv$genus <- misspell(GBIF_biv$genus)
GBIF_brach$genus <- misspell(GBIF_brach$genus)

## Set ranks for cleaning and acceptable suffixes to be used in dataset
b_ranks <- c("phylum", "class", "order", "family", "genus")
b_suff = list(NULL, NULL, NULL, NULL, c("ina", "ella", "etta"))

## Now check for taxonomy issues using check_taxonomy
biv_breakdown <- check_taxonomy(GBIF_biv, suff_set = b_suff, ranks = b_ranks, clean_name = TRUE, resolve_duplicates = TRUE, jump = 5)
## Synonyms detected. Only concerned with genera.
## No cross-rank names detected
## Duplicates - will be resolved.
## Export synonyms for exploration
write.csv(biv_breakdown$synonyms, "data/biv_synonyms.csv")

## Import synonyms to be corrected manually

## Correct synonyms manually

## Now check for taxonomy issues using check_taxonomy
brach_breakdown <- check_taxonomy(GBIF_brach, suff_set = b_suff, ranks = b_ranks, clean_name = TRUE, resolve_duplicates = TRUE, jump = 5)
## Synonyms detected
## No cross-rank names detected
## Duplicates - will be resolved.
## Export synonyms for exploration
write.csv(brach_breakdown$synonyms, "data/brach_synonyms.csv")

## Import synonyms to be corrected manually

## Correct synonyms manually

## Now identified issues have been corrected, produce final datasets
GBIF_biv <- check_taxonomy(GBIF_biv, suff_set = b_suff, ranks = b_ranks, clean_name = TRUE, resolve_duplicates = TRUE, jump = 5)$data
GBIF_brach <- check_taxonomy(GBIF_brach, suff_set = b_suff, ranks = b_ranks, clean_name = TRUE, resolve_duplicates = TRUE, jump = 5)$data

## Note: using check_taxonomy to resolve conflicting higher taxonomies for genera is easy but seems to produce nonsense taxonomies
## If retention of this structure is important, advisable to switch this feature off
## However, for richness/rates analysis, not a problem.

## Screening data and removing subquality entries ##
## Drop entries missing relevant taxonomy: codes as gaps or NAs.
## For phyla, classes, and genera
no_phy_gen <- unique(c(which(is.na(GBIF_biv$phylum)),
                       which(is.na(GBIF_biv$class)),
                       which(is.na(GBIF_biv$genus))))
if(length(no_phy_gen) > 0){
  GBIF_biv_genera <- GBIF_biv[-no_phy_gen,]
} else {
  GBIF_biv_genera <- GBIF_biv
}

## For phyla, classes, and species
no_phy_spec <- unique(c(which(is.na(GBIF_biv$phylum)),
                        which(is.na(GBIF_biv$class)),
                        which(is.na(GBIF_biv$species))))
if(length(no_phy_spec) > 0){
  GBIF_biv_species <- GBIF_biv[-no_phy_spec,]
} else {
  GBIF_biv_species <- GBIF_biv
}

## Now for brachiopods
no_phy_gen <- unique(c(which(is.na(GBIF_brach$phylum)),
                       which(is.na(GBIF_brach$class)),
                       which(is.na(GBIF_brach$genus))))
if(length(no_phy_gen) > 0){
  GBIF_brach_genera <- GBIF_brach[-no_phy_gen,]
} else {
  GBIF_brach_genera <- GBIF_brach
}

## For phyla, classes, and species
no_phy_spec <- unique(c(which(is.na(GBIF_brach$phylum)),
                        which(is.na(GBIF_brach$class)),
                        which(is.na(GBIF_brach$species))))
if(length(no_phy_spec) > 0){
  GBIF_brach_species <- GBIF_brach[-no_phy_spec,]
} else {
  GBIF_brach_species <- GBIF_brach
}

## Next, prune out remaining entries not identified to species rank or lower (just in case we are missing any)
GBIF_biv_species <- GBIF_biv_species[which(GBIF_biv_species[,"taxonRank"] %in% c("SPECIES", "SUBSPECIES", "VARIETY", "FORM")),]
GBIF_brach_species <- GBIF_brach_species[which(GBIF_brach_species[,"taxonRank"] %in% c("SPECIES", "SUBSPECIES", "VARIETY", "FORM")),]

## Clean up species names, drop those not identified to species level
GBIF_biv_species$short_name <- cleansp(GBIF_biv_species$species, misspells=T, stems=T)
GBIF_biv_species <- GBIF_biv_species[!is.na(GBIF_biv_species$species),]

GBIF_brach_species$short_name <- cleansp(GBIF_brach_species$species, misspells=T, stems=T)
GBIF_brach_species <- GBIF_brach_species[!is.na(GBIF_brach_species$species),]

## Define unique name combining phyla and short name
GBIF_biv_species$unique_name <- paste(GBIF_biv_species$phylum, GBIF_biv_species$short_name)
GBIF_brach_species$unique_name <- paste(GBIF_brach_species$phylum, GBIF_brach_species$short_name)

#### Pruning out entries without essential data ####
## Time calibration - requires either: earliestAgeOrLowestStage+latestAgeOrHighestStage OR formation
## Georeferencing - requires either: locality OR verbatimLocality OR decimalLatitude+decimalLongitude+coordinateUncertaintyInMeters+passes checks

## Read in ISOconversion (go from 2 digit codes to 3)
ISO.codes <- read.csv("data/ISO_conversion.csv", row.names = 1)
colnames(ISO.codes) <- c("lang.code", "ISO2", "ISO3")

## Check and clean essential metadata
check.clean.metaData <- function(GBIF_data, ISO.codes){
  ## time calibration - empty cells if not present
  notTimed <- union(which(GBIF_data[,"earliestAgeOrLowestStage"]==""), which(GBIF_data[,"latestAgeOrHighestStage"]==""))
  if(length(notTimed)>0){
    ## get intersect between those missing time and missing formation
    dropTime <- intersect(notTimed, which(GBIF_data[,"formation"] == ""))
  }
  if(length(dropTime) > 0){
    GBIF_data <- GBIF_data[-dropTime,]
  }
  ## georeferencing - NA if not present
  noCoords <- union(union(union(which(is.na(GBIF_data[,"decimalLatitude"])), which(is.na(GBIF_data[,"decimalLongitude"]))), which(is.na(GBIF_data[,"coordinateUncertaintyInMeters"]))), which(GBIF_data[,"countryCode"]==""))
  if(length(noCoords)>0){
    ## get intersect with those missing locality or verbatimLocality
    noLoc <- intersect(which(GBIF_data[,"locality"]==""), which(GBIF_data[,"verbatimLocality"]==""))
    dropGeo <- intersect(noCoords, noLoc)
  }
  if(length(dropGeo) > 0){
    GBIF_data <- GBIF_data[-dropGeo,]
  }
  ## Now to check quality of coordinate data
  split.errors <- str_split(GBIF_data$issue, pattern = ";")
  fatal.issues <- c("COORDINATE_INVALID", "COORDINATE_OUT_OF_RANGE", "COORDINATE_REPROJECTION_FAILED", "COORDINATE_REPROJECTION_SUSPICIOUS", "COORDINATE_UNCERTAINTY_METERS_INVALID",
                    "COUNTRY_COORDINATE_MISMATCH", "GEODETIC_DATUM_INVALID", "PRESUMED_NEGATIVE_LATITUDE", "PRESUMED_NEGATIVE_LONGITUDE", "PRESUMED_SWAPPED_COORDINATES",
                    "ZERO_COORDINATE")
  ## pass over list of split errors, check for fatal errors
  checkers <- c()
  for(x in 1:length(split.errors)){
    if(any(fatal.issues %in% split.errors[[x]])){
     checkers <- c(checkers, x)
    }
  }
  ## Check coordinate uncertainty
  for(x in 1:nrow(GBIF_data)){
    if(!is.na(GBIF_data[x,"coordinateUncertaintyInMeters"])){
      if(GBIF_data[x,"coordinateUncertaintyInMeters"] > 50000){
        checkers <- c(checkers, x)
      }
    }
  }
  if(length(checkers) > 0){
    droppers <- intersect(intersect(which(GBIF_data[,"locality"]==""), which(GBIF_data[,"verbatimLocality"]=="")), unique(checkers))
    nulls <- checkers[!checkers %in% droppers]
    GBIF_data <- GBIF_data[-droppers,]
    GBIF_data[nulls, c("decimalLongitude", "decimalLatitude", "coordinateUncertaintyInMeters")] <- NA
  }
  ## add 3 digit ISO codes
  GBIF_data <- merge(GBIF_data, ISO.codes, by.x = "countryCode", by.y = "ISO2", all.x = T)
  ## get rows without blanks in country code or NA in lat and long. Already determined to keep these due to locality information.
  cc_rows <- sort(unique(intersect(intersect(which(!GBIF_data$countryCode == ""), which(!is.na(GBIF_data$decimalLatitude))), which(!is.na(GBIF_data$decimalLongitude)))))
  ## Check for intersect between cc_rows and flagged vector populated by cc cleaner functions
  f1 <- CoordinateCleaner::cc_val(GBIF_data[cc_rows,], lat = "decimalLatitude", lon = "decimalLongitude", value = "flagged")
  ## Create a vector of row IDs to compare against cc_rows
  flagged <- which(!f1)
  # equal latitude and longitudes
  f2 <- CoordinateCleaner::cc_equ(GBIF_data[cc_rows,], lat = "decimalLatitude", lon = "decimalLongitude", value = "flagged")
  flagged <- c(flagged,which(!f2))
  # country centroids
  f3 <- CoordinateCleaner::cc_cen(GBIF_data[cc_rows,], lat = "decimalLatitude", lon = "decimalLongitude", value = "flagged")
  flagged <- c(flagged,which(!f3))
  # institution coordinates
  f4 <- CoordinateCleaner::cc_inst(GBIF_data[cc_rows,], lat = "decimalLatitude", lon = "decimalLongitude", value = "flagged")
  flagged <- c(flagged,which(!f4))
  # GBIF headquarters
  f5 <- CoordinateCleaner::cc_gbif(GBIF_data[cc_rows,], lat = "decimalLatitude", lon = "decimalLongitude", value = "flagged")
  flagged <- c(flagged,which(!f5))
  # country capitals (within 1km)
  f6 <- CoordinateCleaner::cc_cap(GBIF_data[cc_rows,], lat = "decimalLatitude", lon = "decimalLongitude", value = "flagged", buffer = 1000)
  flagged <- c(flagged,which(!f6))
  # country lookup (25km buffer)
  f7 <- CoordinateCleaner::cc_coun(GBIF_data[cc_rows,], lat = "decimalLatitude", lon = "decimalLongitude", value = "flagged", iso3 = "ISO3", buffer = 25000)
  flagged <- c(flagged,which(!f7))
  ## refine to unique, sort, then convert to the rows in the data. frame
  flagged <- cc_rows[sort(unique(flagged))]
  ## now we have cc_rows that are problematic.
  if(length(flagged) > 0){
    droppers <- intersect(intersect(which(GBIF_data[,"locality"]==""), which(GBIF_data[,"verbatimLocality"]=="")), flagged)
    nulls <- flagged[!flagged %in% droppers]
    GBIF_data <- GBIF_data[-droppers,]
    GBIF_data[nulls, c("decimalLongitude", "decimalLatitude", "coordinateUncertaintyInMeters")] <- NA
  }
  return(GBIF_data)
}

## Run the function
GBIF_biv_genera <- check.clean.metaData(GBIF_data = GBIF_biv_genera, ISO.codes)
GBIF_biv_species <- check.clean.metaData(GBIF_data = GBIF_biv_species, ISO.codes)
GBIF_brach_genera <- check.clean.metaData(GBIF_data = GBIF_brach_genera, ISO.codes)
GBIF_brach_species <- check.clean.metaData(GBIF_data = GBIF_brach_species, ISO.codes)

## Export for a checkpoint
saveRDS(GBIF_biv_genera, file = "data/GBIF_biv_genera.Rds")
saveRDS(GBIF_biv_species, file = "data/GBIF_biv_species.Rds")
saveRDS(GBIF_brach_genera, file = "data/GBIF_brach_genera.Rds")
saveRDS(GBIF_brach_species, file = "data/GBIF_brach_species.Rds")

## Isolate formations for Ian/to check
formations <- c(GBIF_biv_genera$formation, GBIF_biv_species$formation, GBIF_brach_genera$formation, GBIF_brach_species$formation)
formations <- data.frame(sort(unique(formations)))
formations <- cbind(formations, formations)
colnames(formations) <- c("original","updated")
write.csv(formations, file = "data/TS_cleaned_GBIF_formations.csv")

#### Time calibration ####
## Split data into with time and without.


#### Georeferencing GBIF ####
## Register google maps API keys
gMAPIKey <- "AIzaSyAeUFGhS8Inob5ByMIPTokWg076qmStEV0"

## First, partition datasets. Those with lat/long, those without.

## Drop rows with empty locality or verbatim locality strings


## Concatenate higherGeography [19] and locality [26]
biv_genera_locations <- data.frame(apply(, 1, function(x) paste0(x[26], ", ", x[19])))
colnames(biv_genera_locations) <- "locations"
brach_genera_locations <- data.frame(apply(, 1, function(x) paste0(x[26], ", ", x[19])))
colnames(brach_genera_locations) <- "locations"

## Get lat/long using mutate_geocode - DO NOT RUN UNTIL BILLING SORTED FOR GOOGLE CLOUD
#biv_locations_geo <- mutate_geocode(biv_locations, locations)
#brach_locations_geo <- mutate_geocode(brach_locations, locations)

## Get rid of nearest named place
test_set <- biv_locations[1:10,]
test_set <- gsub("Nearest Named Place:", "", test_set)

## Run test with dismo - works!
test_dismo <- dismo::geocode(test_set, oneRecord = F, geocode_key = gMAPIKey)
test_dismo_2 <- dismo::geocode(test_set, oneRecord = T, geocode_key = gMAPIKey)

#### Cleaning PBDB ####
## Load raw PBDB data
raw_PBDB <- readRDS("data/PBDB_Nov23.Rds")

## Clean time data
## Isolate bivalve and brachiopod data
raw_PBDB <- raw_PBDB[c(which(raw_PBDB$phylum == "Brachiopoda"),which(raw_PBDB$class == "Bivalvia")),]

## Update formations using Ali's key
#formations <- read.csv("formation_sorting.csv")
#write.csv(formations, file = "data/AC_cleaned_PBDB_formations.csv")
formations <- read.csv("data/AC_cleaned_PBDB_formations.csv", row.names = 1)[,c(2,4)]
formations <- formations[which(apply(formations, 1, function(x) !x[1] == x[2])),]

## Correct
for(i in 1:nrow(formations)){
  if(any(raw_PBDB[,"formation"] %in% formations[i,1])){
    raw_PBDB[which(raw_PBDB[,"formation"] %in% formations[i,1]),"formation"] <- formations[i,2]
  }
}

## Test that it has worked
for(i in 1:nrow(formations)){
  if(any(raw_PBDB[,"formation"] %in% formations[i,1])){
    print(i)
    print(which(raw_PBDB[,"formation"] %in% formations[i,1]))
  }
}

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

## Export formation data for regex searching
PBDB_formations <- data.frame(unique(PBDB[,"formation"]))
colnames(PBDB_formations) <- "formation"
write.csv(PBDB_formations, "data/PBDB_formations.csv")

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

#### Assign environmental, reefal, and lithological categories to each occurrence ####
## Will use these values to assign cell values
## First, change mixed lithologies to NA, so they can be ignored
PBDB_genera$lithology1[grep('mixed', PBDB_genera$lithology1)] <- NA
PBDB_species$lithology1[grep('mixed', PBDB_species$lithology1)] <- NA

## Define categories
carb <- c("\"carbonate\"", "\"limestone\"", "\"reef rocks\"", "bafflestone", "bindstone", "dolomite",
          "framestone", "grainstone", "lime mudstone", "packstone", "rudstone", "floatstone",
          "wackestone")
clast <- c("\"shale\"", "\"siliciclastic\"", "\"volcaniclastic\"", "claystone", "conglomerate",
           "mudstone", "phyllite", "quartzite", "sandstone", "siltstone", "slate", "schist")
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
reefal <- c("intrashelf/intraplatform reef", "reef, buildup or bioherm",
            "perireef or subreef", "platform/shelf-margin reef", "basin reef", "slope/ramp reef")
nonreefal <- c("coastal indet.", "delta front", "delta plain",
               "deltaic indet.", "estuary/bay", "foreshore", "interdistributary bay",
               "lagoonal", "lagoonal/restricted shallow subtidal",
               "marginal marine indet.", "open shallow subtidal", "fluvial-deltaic indet.",
               "paralic indet.", "peritidal", "prodelta", "sand shoal",
               "shallow subtidal indet.", "shoreface", "transition zone/lower shoreface",
               "basinal (carbonate)", "basinal (siliceous)", "basinal (siliciclastic)",
               "deep-water indet.", "deep subtidal indet.", "deep subtidal ramp",
               "deep subtidal shelf", "offshore", "offshore indet.",
               "offshore shelf", "slope", "submarine fan", "offshore ramp")

## Function for add covariates
source("functions/add.occ.covariate.R")

## Add covariate values to both datasets
PBDB_genera <- add.occ.covariate(PBDB_genera, name = "occLith", ref = "lithology1", varsLabs = c("carb", "sili"), var1 = carb, var2 = clast)
PBDB_genera <- add.occ.covariate(PBDB_genera, name = "occEnv", ref = "environment", varsLabs = c("shal", "deep"), var1 = shallow, var2 = deep)
PBDB_genera <- add.occ.covariate(PBDB_genera, name = "occReef", ref = "environment", varsLabs = c("reef", "noRf"), var1 = reefal, var2 = nonreefal)

PBDB_species <- add.occ.covariate(PBDB_species, name = "occLith", ref = "lithology1", varsLabs = c("carb", "sili"), var1 = carb, var2 = clast)
PBDB_species <- add.occ.covariate(PBDB_species, name = "occEnv", ref = "environment", varsLabs = c("shal", "deep"), var1 = shallow, var2 = deep)
PBDB_species <- add.occ.covariate(PBDB_species, name = "occReef", ref = "environment", varsLabs = c("reef", "noRf"), var1 = reefal, var2 = nonreefal)

#### Add a depth category based on Guo et al. (2023) ####
## Read in depth categories
#setwd("/Users/tjs/Library/CloudStorage/OneDrive-Nexus365/Bivalve_brachiopod/data")
#bivalve.ecology <- read.csv("Bivalvia_ecology_Guo2023.csv")
#brachiopod.ecology <- read.csv("Brachiopoda_ecology_Guo2023.csv")
#setwd(home)

## Convert to ecology and add infaunal/epifaunal supercategories
#bivalve.ecology[,"ecology"] <- NA
#brachiopod.ecology[,"ecology"] <- NA
#bivalve.ecology[,"category"] <- NA
#brachiopod.ecology[,"category"] <- NA
#biv.code <- c("epibyssate", "cemented", "recliner", "shallowI", "deepI", "unknownE")
#brach.code <- c("pedicle", "cemented", "recliner", "infaunal", "unknownE")
#biv.cat.code <- c("epifaunal", "epifaunal", "epifaunal", "infaunal", "infaunal", "epifaunal")
#brach.cat.code <- c("epifaunal", "epifaunal", "epifaunal", "infaunal", "epifaunal")
#for(i in 1:length(biv.code)){
#  bivalve.ecology[which(bivalve.ecology[,"lifestyle"] == i),"ecology"] <- biv.code[i]
#  bivalve.ecology[which(bivalve.ecology[,"lifestyle"] == i),"category"] <- biv.cat.code[i]
#}
#for(i in 1:length(brach.code)){
#  brachiopod.ecology[which(brachiopod.ecology[,"lifestyle"] == i),"ecology"] <- brach.code[i]
#  brachiopod.ecology[which(brachiopod.ecology[,"lifestyle"] == i),"category"] <- brach.cat.code[i]
#}
#bivalve.ecology <- bivalve.ecology[,-2]
#brachiopod.ecology <- brachiopod.ecology[,-2]
#
### Export
#write.csv(brachiopod.ecology, "data/Guo2023_brachiopod_ecology.csv")
#write.csv(bivalve.ecology, "data/Guo2023_bivalve_ecology.csv")

#### Assign depth categories to PBDB data
## Define key
brachiopod.ecology <- read.csv("data/Guo2023_brachiopod_ecology.csv", row.names = 1)
bivalve.ecology <- read.csv("data/Guo2023_bivalve_ecology.csv", row.names = 1)
key <- rbind(brachiopod.ecology, bivalve.ecology)

## Read in function
source("functions/add.ecology.IDs.R")
PBDB_genera <- add.ecology.IDs(data = PBDB_genera, key)
PBDB_species <- add.ecology.IDs(data = PBDB_species, key)

## Filter out entries with no ecology data
PBDB_genera_eco <- PBDB_genera[which(!is.na(PBDB_genera[,"ecological_cat"])),]
PBDB_species_eco <- PBDB_species[which(!is.na(PBDB_species[,"ecological_cat"])),]

##### Pruning out bottom 5%/top 5% richness references for reference sensitivity test ####
## Get quantiles of richness (genus and unique_name)
source("functions/refine.references.R")

## Run the function
genera_RefRef <- refine.references(data = PBDB_genera, level = "genus", quantiles = c(0.05, 0.95), drop.ref.singletons.first = T)
species_RefRef <- refine.references(data = PBDB_species, level = "unique_name", quantiles = c(0.05, 0.95), drop.ref.singletons.first = T)

#### Rasterising data ####
## Rasterise data using function
genera_200 <- rasterOccData(occData = PBDB_genera, res = 200000)
species_200 <- rasterOccData(occData = PBDB_species, res = 200000)

genera_eco_200 <- rasterOccData(occData = PBDB_genera_eco, res = 200000)
species_eco_200 <- rasterOccData(occData = PBDB_species_eco, res = 200000)

genera_200_RefRef <- rasterOccData(occData = genera_RefRef, res = 200000)
species_200_RefRef <- rasterOccData(occData = species_RefRef, res = 200000)

## Export polished files
saveRDS(genera_200, file = "data/genera_200.Rds")
saveRDS(species_200, file = "data/species_200.Rds")

saveRDS(genera_eco_200, file = "data/genera_eco_200.Rds")
saveRDS(species_eco_200, file = "data/species_eco_200.Rds")

saveRDS(genera_200_RefRef, file = "data/genera_RefRef_200.Rds")
saveRDS(species_200_RefRef, file = "data/species_RefRef_200.Rds")

