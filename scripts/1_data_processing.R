## 1. Initial data processing
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## Set working directory
setwd("~/R_packages/R_projects/bivbrach")
home <- getwd()

## If packages aren't installed, install them, then load them
packages <- c("fossilbrush", "velociraptr", "rnaturalearth", "rnaturalearthdata", "terra", "divDyn", "iNEXT", "divvy")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(fossilbrush)
library(velociraptr)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
library(divDyn)
library(iNEXT)
library(divvy)

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

#### Rasterising data ####
# initialise Equal Earth projected coordinates
rWorld <- rast()
prj <- 'EPSG:8857'
rPrj <- project(rWorld, prj, res = 200000) # 200,000m is approximately 2 degrees
values(rPrj) <- 1:ncell(rPrj)

# coordinate column names for the current and target coordinate reference system
xyCartes <- c('paleolng','paleolat')
xyCell   <- c('cellX','cellY')

# extract cell number and centroid coordinates associated with each occurrence
llOccs_genera <- vect(PBDB_genera, geom = xyCartes, crs = 'epsg:4326')
prjOccs_genera <- project(llOccs_genera, prj)

llOccs_species <- vect(PBDB_species, geom = xyCartes, crs = 'epsg:4326')
prjOccs_species <- project(llOccs_species, prj)

# add cell number and coordinate for each occurrence
PBDB_genera$cell <- cells(rPrj, prjOccs_genera)[,'cell']
PBDB_genera[, xyCell] <- xyFromCell(rPrj, PBDB_genera$cell)

PBDB_species$cell <- cells(rPrj, prjOccs_species)[,'cell']
PBDB_species[, xyCell] <- xyFromCell(rPrj, PBDB_species$cell)

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

## Not able to fill in gaps without new environmental data. Need to run other function at some point.

#### Standardise data
source("functions/standardiseCells.R")
coll.min <- 10
ref.min <- 5
multiton.min <- 0.3

## Standardise for collection number, reference number, and multiton ratio ##
PBDB_genera_s <- standardiseCells(PBDB_genera, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "genera")
PBDB_species_s <- standardiseCells(PBDB_species, collMinimum = coll.min, refMinimum = ref.min, multitonRatioMin = multiton.min, level = "species")

#### Standaredise to equal coverage of the taxon occurrence-frequency distribution (quorum of 9) ###
## Need to speak with Erin

## Export polished files
saveRDS(PBDB_genera_s, file = "data/PBDB_BB_genera.Rds")
saveRDS(PBDB_species_s, file = "data/PBDB_BB_species.Rds")

