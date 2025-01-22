## 1. Initial data processing - GBIF cleaning
## Started by TJS on 08/01/2024

## GBIF access citation
# GBIF.org (20 May 2024) GBIF Occurrence Download  https://doi.org/10.15468/dl.3xb65c

## Load libraries
packages <- c("fossilbrush", "stringr", "CoordinateCleaner", "rgbif")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(stringr)
library(fossilbrush)
library(CoordinateCleaner)
library(rgbif)

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

#### Dropping useless columns ####
GBIF_biv <- GBIF_biv[,-c(9, 10, 13, 14, 15, 18, 35, 37)]
GBIF_brach <- GBIF_brach[,-c(9, 10, 13, 14, 15, 18, 35, 37)]

#### Dropping unusable rows ####
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

#### Bivalves ####
## Starting with bivalves
## Dropping genera
#View(data.frame(table(GBIF_biv$genus)))
## None to drop. No affs, cfs, periods, or question marks.

## Drop miscategorised phyla and classes
#View(data.frame(table(GBIF_biv$phylum)))
#View(data.frame(table(GBIF_biv$class)))
## None to drop! All Mollusca/Bivalvia

## Check for lat/long and locality
## First, which entries are missing locality data
noLoc <- c()
noLoc <- c(noLoc,which(GBIF_biv$locality == ""))
noLoc <- c(noLoc,which(GBIF_biv$locality == "?"))
noLoc <- unique(noLoc)

## Use coordinate cleaner to identify problematic coordinates
source("functions/cleanCoordinates.R")

## Read in ISOconversion (go from 2 digit codes to 3)
ISO.codes <- read.csv("data/ISO_conversion.csv", row.names = 1)
colnames(ISO.codes) <- c("lang.code", "ISO2", "ISO3")

## Run the function
GBIF_biv <- cleanCoordinates(GBIF_data = GBIF_biv, ISO.codes)

## Identify lat/longs with NAs
noLL <- which(is.na(GBIF_biv$decimalLatitude) | is.na(GBIF_biv$decimalLatitude))

## Drop intersection of noLL and noLoc
droppers <- c(intersect(noLL, noLoc))
GBIF_biv <- GBIF_biv[-droppers,]

## Check for formations or stage or age
## First, check for incomplete age categories
#View(data.frame(table(GBIF_biv$earliestAgeOrLowestStage)))
## Change recent to Holocene
GBIF_biv$earliestAgeOrLowestStage[which(GBIF_biv$earliestAgeOrLowestStage == "Recent")] <- "Holocene"
GBIF_biv$earliestAgeOrLowestStage[which(GBIF_biv$earliestAgeOrLowestStage == "recent")] <- "Holocene"
GBIF_biv$latestAgeOrHighestStage[which(GBIF_biv$latestAgeOrHighestStage == "Recent")] <- "Holocene"
GBIF_biv$latestAgeOrHighestStage[which(GBIF_biv$latestAgeOrHighestStage == "recent")] <- "Holocene"

## Check ages
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

## No stage is intersect
noStage <- intersect(noEarly, noLate)

## If missing formations, add to vector
#View(data.frame(table(GBIF_biv$formation)))
noForm <- c()
noForm <- c(noForm, which(GBIF_biv$formation == ""))
noForm <- c(noForm, which(GBIF_biv$formation == "C"))
noForm <- c(noForm, which(GBIF_biv$formation == "Chalk"))
noForm <- c(noForm, which(GBIF_biv$formation == "Chalk Formation"))
noForm <- c(noForm, which(GBIF_biv$formation == "Etage Dd1delta"))
noForm <- c(noForm, which(GBIF_biv$formation == "Etage Dd5"))
noForm <- c(noForm, which(GBIF_biv$formation == "Etage Ee2"))
noForm <- c(noForm, which(GBIF_biv$formation == "Etage F"))
noForm <- c(noForm, which(GBIF_biv$formation == "Etage Ff1"))
noForm <- c(noForm, which(GBIF_biv$formation == "Etage Ff2"))
noForm <- c(noForm, which(GBIF_biv$formation == "Etage Gg1"))
noForm <- c(noForm, which(GBIF_biv$formation == "relavant to Kasamori Formation"))
noForm <- c(noForm, which(GBIF_biv$formation == "Shale above #7 coal"))
noForm <- c(noForm, which(GBIF_biv$formation == "unknown"))
noForm <- c(noForm, which(GBIF_biv$formation == "unknown formation"))
noForm <- c(noForm, which(GBIF_biv$formation == "unnamed formation"))
noForm <- c(noForm, which(GBIF_biv$formation == "Unnamed Marine Terrace Overlying Pliocene Sisquoc Formation"))
noForm <- c(noForm, which(GBIF_biv$formation == "Unrecorded"))
noForm <- unique(noForm)
droppers <- c(intersect(noStage, noForm))
GBIF_biv <- GBIF_biv[-droppers,]

#### Splitting ages ####
## Update specific age entry
## With formation
GBIF_biv[which(GBIF_biv$earliestAgeOrLowestStage == "Upper Ludlow, Pragian, Zlichovian"), "earliestAgeOrLowestStage"] <- "Pragian-Zlichovian"
GBIF_biv[which(GBIF_biv$latestAgeOrHighestStage == "Upper Ludlow, Pragian, Zlichovian"), "latestAgeOrHighestStage"] <- "Pragian-Zlichovian"

## With non-punctuation
## None!

## Now unusuable records have been dropped, split stages and formations
## Start with stages
latePunct <- unique(GBIF_biv$latestAgeOrHighestStage[which(str_detect(GBIF_biv$latestAgeOrHighestStage, pattern = "[:punct:]"))])
earlyPunct <- unique(GBIF_biv$earliestAgeOrLowestStage[which(str_detect(GBIF_biv$earliestAgeOrLowestStage, pattern = "[:punct:]"))])

## Wittle down to stages to be split

##### Start from here! NEED TO CHECK INDEXING! ####

#View(data.frame(latePunct))
latePunct <- latePunct[c(5, 9, 12, 13, 33, 35, 36, 37, 38, 39, 40, 42, 45)]

#View(data.frame(earlyPunct))
earlyPunct <- earlyPunct[c(5, 7, 10, 11, 13, 15, 17, 18, 19, 20, 21, 24, 25, 26, 28, 31, 32, 39, 40, 41, 42, 43, 44, 46, 48, 49, 50, 63, 64, 65, 66, 67, 68, 69, 70, 71)]

## get ages to be split
splitLate <- c()
for(i in 1:length(latePunct)){
  splitLate <- c(splitLate, which(GBIF_biv$latestAgeOrHighestStage == latePunct[i]))
}

splitEarly <- c()
for(i in 1:length(earlyPunct)){
  splitEarly <- c(splitEarly, which(GBIF_biv$earliestAgeOrLowestStage == earlyPunct[i]))
}

## Isolate stages
lateStage <- data.frame(GBIF_biv$latestAgeOrHighestStage)
colnames(lateStage) <- "latestAgeOrHighestStage1"

earlyStage <- data.frame(GBIF_biv$earliestAgeOrLowestStage)
colnames(earlyStage) <- "earliestAgeOrLowestStage1"

## Only 2 stages
lateStage$latestAgeOrHighestStage2 <- ""
earlyStage$earliestAgeOrLowestStage2 <- ""

## Specify punctuation
p <- c('-', ',' , '/')

## Split stages
for(i in splitEarly){
  for(m in p){
    if(str_detect(earlyStage$earliestAgeOrLowestStage1[i], pattern = fixed(m))){
      ## extract stages
      ageVec <- unlist(str_split(earlyStage$earliestAgeOrLowestStage1[i], pattern = fixed(m)))
      ## assign to new columns
      for(f in 1:length(ageVec)){
        earlyStage[i,f] <- ageVec[f]
      }
    }
  }
}

for(i in splitLate){
  for(m in p){
    if(str_detect(lateStage$latestAgeOrHighestStage1[i], pattern = fixed(m))){
      ## extract stages
      ageVec <- unlist(str_split(lateStage$latestAgeOrHighestStage1[i], pattern = fixed(m)))
      ## assign to new columns
      for(f in 1:length(ageVec)){
        lateStage[i,f] <- ageVec[f]
      }
    }
  }
}

## delete original stage data columns
GBIF_biv$earliestAgeOrLowestStage <- NULL
GBIF_biv$latestAgeOrHighestStage <- NULL

## Attach to dataset
GBIF_biv <- cbind(GBIF_biv, earlyStage, lateStage)

#### Splitting formations ####
## Now to do the same for formations
#View(data.frame(table(GBIF_biv$formation)))
forms <- unique(GBIF_biv$formation[which(str_detect(GBIF_biv$formation, pattern = "[:punct:]"))])
forms <- c(forms, GBIF_biv$formation[which(str_detect(GBIF_biv$formation, pattern = " and "))])
forms <- c(forms, GBIF_biv$formation[which(str_detect(GBIF_biv$formation, pattern = " & "))])
forms <- c(forms, GBIF_biv$formation[which(str_detect(GBIF_biv$formation, pattern = " or "))])
forms <- unique(forms)

## Wittle down to formations to be split
#View(data.frame(forms))
forms <- forms[c(9, 53, 75, 110, 117, 121, 123, 128, 129, 130, 138, 139, 140, 148, 151, 153, 172, 178, 179, 192, 201, 210, 211, 212,
                 238, 241, 243, 244, 247, 248, 249, 250, 251, 252, 254, 255, 257, 268, 269, 272, 278, 279, 285, 286, 291, 292, 295, 296,
                 297, 298, 300, 301, 304, 305, 309, 312, 322, 323, 327, 328, 329, 342:361)]

## get ages to be split
splitForms <- c()
for(i in 1:length(forms)){
  splitForms <- c(splitForms, which(GBIF_biv$formation == forms[i]))
}

## Isolate formation
formations <- data.frame(GBIF_biv$formation)
colnames(formations) <- "formation1"

## max 3 formations
formations$formation2 <- ""
formations$formation3 <- ""
formations$formation4 <- ""

## Specify string to split by
p <- c('-', ',' , '/', ' or ', ' and ', ' & ')

## define new formations list
newForms <- as.list(formations$formation1)

## Split stages
for(i in splitForms){
  for(m in p){
    if(length(newForms[[i]]) == 1){
      if(str_detect(newForms[[i]], pattern = fixed(m))){
        ## get new formation
        newForms[[i]] <- unlist(str_split(newForms[[i]], pattern = fixed(m)))
      }
    } else {
      if(any(str_detect(newForms[[i]], pattern = fixed(m)))){
        ## get which
        tbSplit <- newForms[[i]][which(str_detect(newForms[[i]], pattern = fixed(m)))]
        ## to keep
        tbKept <- newForms[[i]][which(!str_detect(newForms[[i]], pattern = fixed(m)))]
        ## split tbSplit
        split <- unlist(str_split(tbSplit, pattern = fixed(m)))
        ## if any half equals "", drop
        if(any(split == "")){
          split <- split[-which(split == "")]
        }
        newForms[[i]] <- c(tbKept, split)
      }
    }
  }
}

## populations formations object for attachment
for(r in 1:nrow(formations)){
  for(c in 1:length(newForms[[r]])){
    formations[r,c] <- newForms[[r]][c]
  }
}

## delete original stage data columns
GBIF_biv$formation <- NULL

## Attach to dataset
GBIF_biv <- cbind(GBIF_biv, formations)

#### Clean up taxonomy ####
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

## Export for a checkpoint
saveRDS(GBIF_biv_genera, file = "data/GBIF/GBIF_biv.Rds")
saveRDS(GBIF_brach_genera, file = "data/GBIF/GBIF_brach.Rds")

#### Time calibration ####
## Split data into with time and without.


#### Georeferencing GBIF ####
## Register google maps API keys
gMAPIKey <- "AIzaSyAeUFGhS8Inob5ByMIPTokWg076qmStEV0"

## First, partition datasets. Those with lat/long, those without.

## Concatenate higherGeography [19] and locality [26]. Potentially do this with country, county, and township strings?
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

