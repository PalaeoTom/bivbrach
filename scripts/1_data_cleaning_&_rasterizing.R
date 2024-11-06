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
packages <- c("fossilbrush", "rnaturalearth", "rnaturalearthdata", "terra", "divDyn", "iNEXT", "divvy", "rgbif", "usethis", "bit64", "dismo", "dplyr")
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

## install divvyCompanion from github and load
library(remotes)
install_github("PalaeoTom/divvyCompanion")
library(divvyCompanion)

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
#                 "decimalLatitude", "decimalLongitude", "hasCoordinate", "hasGeospatialIssues",
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

#### Cleaning GBIF taxonomy ####
## Drop data with taxonRank not genus or species
GBIF_biv <- GBIF_biv[-union(union(union(union(which(GBIF_biv[,"taxonRank"] == "ORDER"),which(GBIF_biv[,"taxonRank"] == "CLASS")),which(GBIF_biv[,"taxonRank"] == "PHYLUM")), which(GBIF_biv[,"taxonRank"] == "FAMILY")), which(GBIF_biv[,"taxonRank"] == "UNRANKED")),]
GBIF_brach <- GBIF_brach[-union(union(union(union(which(GBIF_brach[,"taxonRank"] == "ORDER"),which(GBIF_brach[,"taxonRank"] == "CLASS")),which(GBIF_brach[,"taxonRank"] == "PHYLUM")), which(GBIF_brach[,"taxonRank"] == "FAMILY")), which(GBIF_brach[,"taxonRank"] == "UNRANKED")),]

## Replace blanks with NA
if(length(which(GBIF_biv[,"phylum"] == "")) > 0){
  GBIF_biv[which(GBIF_biv[,"phylum"] == ""), "phylum"] <- NA
}
if(length(which(GBIF_biv[,"class"] == "")) > 0){
  GBIF_biv[which(GBIF_biv[,"class"] == ""), "class"] <- NA
}
if(length(which(GBIF_biv[,"order"] == "")) > 0){
  GBIF_biv[which(GBIF_biv[,"order"] == ""), "order"] <- NA
}
if(length(which(GBIF_biv[,"family"] == "")) > 0){
  GBIF_biv[which(GBIF_biv[,"family"] == ""), "family"] <- NA
}
if(length(which(GBIF_biv[,"genus"] == "")) > 0){
  GBIF_biv[which(GBIF_biv[,"genus"] == ""), "genus"] <- NA
}
if(length(which(GBIF_biv[,"species"] == "")) > 0){
  GBIF_biv[which(GBIF_biv[,"species"] == ""), "species"] <- NA
}
if(length(which(GBIF_brach[,"phylum"] == "")) > 0){
  GBIF_brach[which(GBIF_brach[,"phylum"] == ""), "phylum"] <- NA
}
if(length(which(GBIF_brach[,"class"] == "")) > 0){
  GBIF_brach[which(GBIF_brach[,"class"] == ""), "class"] <- NA
}
if(length(which(GBIF_brach[,"order"] == "")) > 0){
  GBIF_brach[which(GBIF_brach[,"order"] == ""), "order"] <- NA
}
if(length(which(GBIF_brach[,"family"] == "")) > 0){
  GBIF_brach[which(GBIF_brach[,"family"] == ""), "family"] <- NA
}
if(length(which(GBIF_brach[,"genus"] == "")) > 0){
  GBIF_brach[which(GBIF_brach[,"genus"] == ""), "genus"] <- NA
}
if(length(which(GBIF_brach[,"species"] == "")) > 0){
  GBIF_brach[which(GBIF_brach[,"species"] == ""), "species"] <- NA
}

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

#### Time calibration ####
## Exploring data. Separate data with stage-level time data, and that without.
biv_timed <- intersect(which(!(GBIF_biv[,"earliestAgeOrLowestStage"]=="")), which(!(GBIF_biv[,"latestAgeOrHighestStage"]=="")))
brach_timed <- intersect(which(!(GBIF_brach[,"earliestAgeOrLowestStage"]=="")), which(!(GBIF_brach[,"latestAgeOrHighestStage"]=="")))
GBIF_biv_timed <- GBIF_biv[biv_timed,]
GBIF_biv_nontimed <- GBIF_biv[-biv_timed,]
GBIF_brach_timed <- GBIF_brach[brach_timed,]
GBIF_brach_nontimed <- GBIF_brach[-brach_timed,]

## Of the nontimed, how many have formation data
biv_nontimed_forms <- which(!(GBIF_biv_nontimed[,"formation"]==""))
brach_nontimed_forms <- which(!(GBIF_brach_nontimed[,"formation"]==""))

#### Georeferencing GBIF ####
## Register google maps API keys
gMAPIKey <- "AIzaSyAeUFGhS8Inob5ByMIPTokWg076qmStEV0"

## First, partition datasets. 1) Data with coordinates and georeferenceVerificationstatus verified.
biv_keepers <- intersect(intersect(which((GBIF_biv[,"hasCoordinate"])), which(!(GBIF_biv[,"hasGeospatialIssues"]))), grep("verified by", GBIF_biv[,"georeferenceVerificationStatus"]))
GBIF_biv_geo <- GBIF_biv[biv_keepers,]
GBIF_biv_nogeo <- GBIF_biv[-biv_keepers,]

brach_keepers <- intersect(intersect(which((GBIF_brach[,"hasCoordinate"])), which(!(GBIF_brach[,"hasGeospatialIssues"]))), grep("verified by", GBIF_brach[,"georeferenceVerificationStatus"]))
GBIF_brach_geo <- GBIF_brach[brach_keepers,]
GBIF_brach_nogeo <- GBIF_brach[-brach_keepers,]

## Drop rows with empty locality + country strings
GBIF_biv_nogeo <- GBIF_biv_nogeo[-union(which(GBIF_biv_nogeo[,"higherGeography"] == ""), which(GBIF_biv_nogeo[,"locality"] == "")),]
GBIF_brach_nogeo <- GBIF_brach_nogeo[-union(which(GBIF_brach_nogeo[,"higherGeography"] == ""), which(GBIF_brach_nogeo[,"locality"] == "")),]

## Concatenate higherGeography [19] and locality [26]
biv_locations <- data.frame(apply(GBIF_biv_nogeo, 1, function(x) paste0(x[26], ", ", x[19])))
colnames(biv_locations) <- "locations"
brach_locations <- data.frame(apply(GBIF_brach_nogeo, 1, function(x) paste0(x[26], ", ", x[19])))
colnames(brach_locations) <- "locations"

## Get lat/long using mutate_geocode - DO NOT RUN UNTIL BILLING SORTED FOR GOOGLE CLOUD
#biv_locations_geo <- mutate_geocode(biv_locations, locations)
#brach_locations_geo <- mutate_geocode(brach_locations, locations)

test_set <- biv_locations[1:10,]
## Get rid of nearest named place
test_set <- gsub("Nearest Named Place:", "", test_set)
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

