## 1.4 Initial data processing - GBIF cleaning
## Started by TJS on 08/01/2024

## GBIF access citation
# GBIF.org (20 May 2024) GBIF Occurrence Download  https://doi.org/10.15468/dl.3xb65c

## Load libraries
packages <- c("fossilbrush", "stringr", "CoordinateCleaner", "rgbif", "divDyn", "velociraptr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(stringr)
library(fossilbrush)
library(CoordinateCleaner)
library(rgbif)
library(divDyn)
library(velociraptr)

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
GBIF_biv <- readRDS("data/unclean_data/raw_GBIF_biv_18Oct24.Rds")
GBIF_brach <- readRDS("data/unclean_data/raw_GBIF_brach_18Oct24.Rds")

#### Dropping useless columns ####
GBIF_biv <- GBIF_biv[,-c(9, 10, 13, 14, 15, 18, 35)]
GBIF_brach <- GBIF_brach[,-c(9, 10, 13, 14, 15, 18, 35)]

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
## manually checked uncertain, indeterminate, unknown, locality, and redacted
## no uncertain or indeterminate. All redacted and unknown to be discounted. Locality requires specific approach.
#View(data.frame(table(GBIF_biv$locality)))
noLoc <- c()
noLoc <- c(noLoc,which(GBIF_biv$locality == ""))
noLoc <- c(noLoc,which(GBIF_biv$locality == "?"))
noLoc <- c(noLoc,which(GBIF_biv$locality == "[redacted]"))
noLoc <- c(noLoc, which(str_detect(GBIF_biv$locality, pattern = "unknown")))

## refine hits for 'locality'
localityHits <- unique(GBIF_biv$locality[which(str_detect(GBIF_biv$locality, pattern = "locality"))])
#View(data.frame(localityHits))
localityHits <- localityHits[-c(8, 10, 12, 14, 17, 20, 23, 26, 31, 33, 34, 35, 36, 39, 40, 41, 42, 50,
                                111, 131, 136, 137, 172, 182, 183, 199, 287, 293, 295)]
## Add to noLoc vector
noLoc <- c(noLoc, which(GBIF_biv$locality %in% localityHits))
noLoc <- unique(noLoc)

## Use coordinate cleaner to identify problematic coordinates
source("functions/cleanCoordinates.R")

## Read in ISOconversion (go from 2 digit codes to 3)
ISO.codes <- read.csv("data/metadata/ISO_conversion.csv", row.names = 1)
colnames(ISO.codes) <- c("lang.code", "ISO2", "ISO3")

## Run the function
GBIF_biv <- cleanCoordinates(GBIF_data = GBIF_biv, ISO.codes)

## Identify lat/longs with NAs
noLL <- which(is.na(GBIF_biv$decimalLatitude) | is.na(GBIF_biv$decimalLatitude))

## Drop intersection of noLL and noLoc
droppers <- intersect(noLL, noLoc)
GBIF_biv <- GBIF_biv[-droppers,]

## Check for formations or stage or age
## First, check for incomplete age categories
#View(data.frame(table(GBIF_biv$earliestAgeOrLowestStage)))
## Check ages
noEarly <-c()
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == ""))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "Early"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "late"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "Late"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "Lower/Early"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "middle"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "Middle"))
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

##### Splitting ages #####
## Update specific age entry
## With formation
GBIF_biv[which(GBIF_biv$earliestAgeOrLowestStage == "Upper Ludlow, Pragian, Zlichovian"), "earliestAgeOrLowestStage"] <- "Pragian-Zlichovian"
GBIF_biv[which(GBIF_biv$latestAgeOrHighestStage == "Upper Ludlow, Pragian, Zlichovian"), "latestAgeOrHighestStage"] <- "Pragian-Zlichovian"

## With non-punctuation
## None!

#### Read in bivalve milestone data ####
## Wittle down to stages to be split
## First, save progress
#saveRDS(GBIF_biv, file = "data/GBIF/GBIF_biv_milestone.Rds")
GBIF_biv <- readRDS("data/GBIF/GBIF_biv_milestone.Rds")

## Parition out original stage data for record
orig_biv_stages <- GBIF_biv[,c(25,26)]
colnames(orig_biv_stages) <- paste0(colnames(orig_biv_stages), "_OLD")

## Clean up stages
noLate <- c()
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "early"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "Early"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "early late"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "early to middle"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "early Middle"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "Early to Middle"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "early-middle"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "late"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "Late"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "middle"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "Middle"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "middle-late"))
noLate <-c(noLate, which(GBIF_biv$latestAgeOrHighestStage == "Upper"))
noLate <- unique(noLate)
GBIF_biv$latestAgeOrHighestStage[noLate] <- ""

noEarly <-c()
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "Early"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "late"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "Late"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "Lower/Early"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "middle"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "Middle"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "Unnamed"))
noEarly <-c(noEarly, which(GBIF_biv$earliestAgeOrLowestStage == "Early to Middle"))
noEarly <- unique(noEarly)
GBIF_biv$earliestAgeOrLowestStage[noEarly] <- ""

## Check OIS formations
OIS.stages <- c()
OIS.stages <- c(OIS.stages,which(GBIF_biv$earliestAgeOrLowestStage == "OIS 5"))
OIS.stages <- c(OIS.stages,which(GBIF_biv$earliestAgeOrLowestStage == "OIS 7"))
OIS.stages <- c(OIS.stages,which(GBIF_biv$latestAgeOrHighestStage == "OIS 5"))
OIS.stages <- c(OIS.stages,which(GBIF_biv$latestAgeOrHighestStage == "OIS 7"))
OIS.stages <- unique(OIS.stages)
GBIF_biv[OIS.stages,"formation"] <- "Red Sea Coastal Plain"

## Fixing dogger alpha stages
GBIF_biv[which(GBIF_biv$earliestAgeOrLowestStage == "Dogger alpha"),"formation"] <- "Brown Jura"
GBIF_biv[which(GBIF_biv$earliestAgeOrLowestStage == "Dogger alpha"),"earliestAgeOrLowestStage"] <- "Aalenian-Callovian"

## Fixing location for problem entries spotted along the way
GBIF_biv[which(GBIF_biv$earliestAgeOrLowestStage == "Headonian"),"locality"] <- "Thorness Bay, Isle of Wight, UK"

## Clean stages - might move this to main cleaning scripts
## First, read in stages
stages <- read.csv("data/metadata/macrostrat_raw.csv", row.names = 1, header = T)
stages <- stages[!duplicated(stages[,c("name", "t_age", "b_age")]),]
stage_names <- stages$name
#View(data.frame(stage_names))

## Now clean
source("functions/clean.stage.names.R")
GBIF_biv <- clean.stage.names(data = GBIF_biv, columns = c("latestAgeOrHighestStage", "earliestAgeOrLowestStage"))

## Now check bivalves against Macrostrat names
## Run bulk.update.stages (aggregation of all specific name changes identified thus far), then check for other errors
source("functions/bulk.update.stages.R")
GBIF_biv$earliestAgeOrLowestStage <- bulk.update.stages(GBIF_biv$earliestAgeOrLowestStage)

## Inspect and update function
#View(data.frame(table(GBIF_biv$earliestAgeOrLowestStage)))

## Load function and run again
#source("functions/bulk.update.stages.R")
#GBIF_biv$earliestAgeOrLowestStage <- bulk.update.stages(GBIF_biv$earliestAgeOrLowestStage)

## Now run for late stages, then check
GBIF_biv$latestAgeOrHighestStage <- bulk.update.stages(GBIF_biv$latestAgeOrHighestStage)

## Then inspect and add to function
#View(data.frame(table(GBIF_biv$latestAgeOrHighestStage)))

## re-load function and run again
#source("functions/bulk.update.stages.R")
#GBIF_biv$latestAgeOrHighestStage <- bulk.update.stages(GBIF_biv$latestAgeOrHighestStage)

## Now unusuable records have been dropped, split stages and formations
## Start with stages
latePunct <- c()
latePunct <- c(latePunct, unique(GBIF_biv$latestAgeOrHighestStage[which(str_detect(GBIF_biv$latestAgeOrHighestStage, pattern = "[:punct:]"))]))
latePunct <- c(latePunct, GBIF_biv$latestAgeOrHighestStage[which(str_detect(GBIF_biv$latestAgeOrHighestStage, pattern = regex(" to ", ignore_case = T)))])
latePunct <- c(latePunct, GBIF_biv$latestAgeOrHighestStage[which(str_detect(GBIF_biv$latestAgeOrHighestStage, pattern = regex(" or ", ignore_case = T)))])
latePunct <- c(latePunct, GBIF_biv$latestAgeOrHighestStage[which(str_detect(GBIF_biv$latestAgeOrHighestStage, pattern = "[|]"))])
latePunct <- unique(latePunct)

earlyPunct <- c()
earlyPunct <- c(earlyPunct, unique(GBIF_biv$earliestAgeOrLowestStage[which(str_detect(GBIF_biv$earliestAgeOrLowestStage, pattern = "[:punct:]"))]))
earlyPunct <- c(earlyPunct, GBIF_biv$earliestAgeOrLowestStage[which(str_detect(GBIF_biv$earliestAgeOrLowestStage, pattern = regex(" to ", ignore_case = T)))])
earlyPunct <- c(earlyPunct, GBIF_biv$earliestAgeOrLowestStage[which(str_detect(GBIF_biv$earliestAgeOrLowestStage, pattern = regex(" or ", ignore_case = T)))])
earlyPunct <- c(earlyPunct, GBIF_biv$earliestAgeOrLowestStage[which(str_detect(GBIF_biv$earliestAgeOrLowestStage, pattern = "[|]"))])
earlyPunct <- unique(earlyPunct)

#View(data.frame(latePunct))
latePunct <- latePunct[-c(7, 8, 17, 19:21, 23:29, 35, 40:41, 47, 50, 53:54, 56, 59)]

#View(data.frame(earlyPunct))
earlyPunct <- earlyPunct[-c(20:21, 32:35, 40, 44:47, 49:50, 54, 57, 64, 75:76, 84, 88:100, 106, 109, 111)]

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

## Max 2 stages for all.
lateStage$latestAgeOrHighestStage2 <- ""
earlyStage$earliestAgeOrLowestStage2 <- ""

## Specify characters to split by
p <- c('-', ',' , '/', ' to ', ' or ', '|')

## Split early stages
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

## Split late stages
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

## Finally, pass over each entry, only retaining Macrostrat compatible strings
stages <- read.csv("data/metadata/macrostrat_raw.csv", row.names = 1, header = T)
stages <- stages[!duplicated(stages[,c("name", "t_age", "b_age")]),]
source("functions/stage.checker.R")

## Run stage checker
earlyStage$earliestAgeOrLowestStage1 <- stage.checker(earlyStage$earliestAgeOrLowestStage1, stages)
earlyStage$earliestAgeOrLowestStage2 <- stage.checker(earlyStage$earliestAgeOrLowestStage2, stages)
lateStage$latestAgeOrHighestStage1 <- stage.checker(lateStage$latestAgeOrHighestStage1, stages)
lateStage$latestAgeOrHighestStage2 <- stage.checker(lateStage$latestAgeOrHighestStage2, stages)

## Combine new stage objects and update names
new_biv_stages <- cbind(earlyStage, lateStage)
colnames(new_biv_stages) <- paste0("Macrostrat_unit", 1:4)

## delete old stages
GBIF_biv$earliestAgeOrLowestStage <- NULL
GBIF_biv$latestAgeOrHighestStage <- NULL

## Attach to dataset
GBIF_biv <- cbind(GBIF_biv, new_biv_stages, orig_biv_stages)

##### Splitting formations #####
## Now to do the same for formations
#View(data.frame(table(GBIF_biv$formation)))
forms <- unique(GBIF_biv$formation[which(str_detect(GBIF_biv$formation, pattern = "[:punct:]"))])
forms <- c(forms, GBIF_biv$formation[which(str_detect(GBIF_biv$formation, pattern = " and "))])
forms <- c(forms, GBIF_biv$formation[which(str_detect(GBIF_biv$formation, pattern = " & "))])
forms <- c(forms, GBIF_biv$formation[which(str_detect(GBIF_biv$formation, pattern = " or "))])
forms <- unique(forms)

## Wittle down to formations to be split
#View(data.frame(forms))
forms <- forms[c(18, 19, 69, 111, 138, 139, 153, 159, 180, 183, 187, 188, 206, 209, 235, 240, 264, 265, 269,
                 275, 276, 281, 286, 287, 288, 289, 294, 299, 310, 321, 327, 329, 334, 335, 336, 338, 342, 345, 349, 350,
                 361, 366, 368, 370, 372, 373, 386, 387, 389, 390, 391, 392, 394, 397, 398, 399, 401, 402, 404, 406, 413,
                 414, 418, 428, 434:457)]

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

## Tidy formations, stages, and genera
## Use misspell to tidy up dipthongs and alternative spellings
source("functions/misspell.R")
GBIF_biv$genus <- misspell(GBIF_biv$genus)

## Drop punctuation from formations and genus names
GBIF_biv$genus <- str_replace_all(GBIF_biv$genus, pattern = "[:punct:]", replacement = "")
GBIF_biv$formation1 <- str_replace_all(GBIF_biv$formation1, pattern = "[:punct:]", replacement = "")
GBIF_biv$formation2 <- str_replace_all(GBIF_biv$formation2, pattern = "[:punct:]", replacement = "")
GBIF_biv$formation3 <- str_replace_all(GBIF_biv$formation3, pattern = "[:punct:]", replacement = "")
GBIF_biv$formation4 <- str_replace_all(GBIF_biv$formation4, pattern = "[:punct:]", replacement = "")

## Correct capitalization for genera and formations
GBIF_biv$genus <- str_to_title(GBIF_biv$genus)
GBIF_biv$formation1 <- str_to_title(GBIF_biv$formation1)
GBIF_biv$formation2 <- str_to_title(GBIF_biv$formation2)
GBIF_biv$formation3 <- str_to_title(GBIF_biv$formation3)
GBIF_biv$formation4 <- str_to_title(GBIF_biv$formation4)

## Tidy up species
#View(data.frame(table(GBIF_biv$species)))
## separate out species name
splitSpecies <- str_split(GBIF_biv$species, pattern = fixed(" "))
splitSpecies <- lapply(1:length(splitSpecies), function(x){
  if(length(splitSpecies[[x]])>1){
    return(splitSpecies[[x]][2])
  } else {
    return(splitSpecies[[x]])
  }
})
GBIF_biv$species <- unlist(splitSpecies)

## Now clean species
#View(data.frame(table(GBIF_biv$species)))
gen.level <- c()
gen.level <- c(gen.level,which(GBIF_biv$species == ""))
GBIF_biv[gen.level, "species"] <- "sp."

## Finally, drop entries that could conceivably fall within Meghalayan
recent <- c()
recent <- c(recent, which(GBIF_biv$Macrostrat_unit1 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit1 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit1 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit1 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit1 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit1 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit1 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit1 == regex("C1", ignore_case = T)))

recent <- c(recent, which(GBIF_biv$Macrostrat_unit2 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit2 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit2 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit2 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit2 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit2 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit2 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit2 == regex("C1", ignore_case = T)))

recent <- c(recent, which(GBIF_biv$Macrostrat_unit3 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit3 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit3 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit3 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit3 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit3 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit3 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit3 == regex("C1", ignore_case = T)))

recent <- c(recent, which(GBIF_biv$Macrostrat_unit4 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit4 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit4 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit4 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit4 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit4 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit4 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_biv$Macrostrat_unit4 == regex("C1", ignore_case = T)))
recent <- unique(recent)
if(length(recent) > 0){
  GBIF_biv <- GBIF_biv[-recent,]
}

## Export
saveRDS(GBIF_biv, file = "data/GBIF/GBIF_biv_end_1_4.Rds")

#### Brachiopods ####
#View(data.frame(table(GBIF_brach$genus)))
## 6 gaps
#View(GBIF_brach[which(GBIF_brach$genus == ""),])
## Drop all. Miscategorised as being identified to species level.
GBIF_brach <- GBIF_brach[-which(GBIF_brach$genus == ""),]

## Check for miscategorised phyla
#View(data.frame(table(GBIF_brach$phylum)))
## All brachiopods! All good.

## Drop entries with no locality information and no coordinates
#View(data.frame(table(GBIF_biv$locality)))
noLoc <- c()
noLoc <- c(noLoc,which(GBIF_biv$locality == ""))
noLoc <- c(noLoc,which(GBIF_biv$locality == "?"))
## no uncertain or indeterminate entries
noLoc <- c(noLoc, which(str_detect(GBIF_brach$locality, pattern = "unknown")))
noLoc <- c(noLoc,which(GBIF_biv$locality == "[redacted]"))
## check locality hits
localityHits <- unique(GBIF_brach$locality[which(str_detect(GBIF_brach$locality, pattern = "locality"))])
#View(data.frame(localityHits))
localityHits <- localityHits[-c(2, 6, 8, 12, 13, 24, 25, 30, 32, 33, 34, 36, 38, 44, 45, 46, 51, 52, 55, 56, 58, 61, 63, 68, 70, 71, 72, 73, 78, 79, 81, 84, 87, 88, 91, 99,
                                276:281, 283, 284, 285, 286, 293, 294)]
#View(data.frame(localityHits))
noLoc <- c(noLoc, which(GBIF_brach$locality %in% localityHits))
noLoc <- unique(noLoc)

## Use coordinate cleaner to identify problematic coordinates
source("functions/cleanCoordinates.R")

## Read in ISOconversion (go from 2 digit codes to 3)
ISO.codes <- read.csv("data/metadata/ISO_conversion.csv", row.names = 1)
colnames(ISO.codes) <- c("lang.code", "ISO2", "ISO3")

## Run the function
GBIF_brach <- cleanCoordinates(GBIF_data = GBIF_brach, ISO.codes)

## Identify lat/longs with NAs
noLL <- which(is.na(GBIF_brach$decimalLatitude) | is.na(GBIF_brach$decimalLatitude))

## Drop intersection of noLL and noLoc
droppers <- c()
droppers <- c(droppers, intersect(noLL, noLoc))
GBIF_brach <- GBIF_brach[-droppers,]

## Record milestone - post coordinate cleaner
#saveRDS(GBIF_brach, file = "data/GBIF/GBIF_brach_milestone.Rds")
GBIF_brach <- readRDS("data/GBIF/GBIF_brach_milestone.Rds")

## First, check for incomplete age categories.
## First, early
#View(data.frame(table(GBIF_brach$earliestAgeOrLowestStage)))
noEarly <- c()
noEarly <-c(noEarly, which(GBIF_brach$earliestAgeOrLowestStage == ""))
noEarly <-c(noEarly, which(GBIF_brach$earliestAgeOrLowestStage == "Early"))
noEarly <-c(noEarly, which(GBIF_brach$earliestAgeOrLowestStage == "indet."))
noEarly <-c(noEarly, which(GBIF_brach$earliestAgeOrLowestStage == "Late"))
noEarly <-c(noEarly, which(GBIF_brach$earliestAgeOrLowestStage == "Middle"))
noEarly <-c(noEarly, which(GBIF_brach$earliestAgeOrLowestStage == "Unnamed"))
noEarly <- unique(noEarly)

#View(data.frame(table(GBIF_brach$latestAgeOrHighestStage)))
noLate <- c()
noLate <-c(noLate, which(GBIF_brach$latestAgeOrHighestStage == ""))
noLate <-c(noLate, which(GBIF_brach$latestAgeOrHighestStage == "early"))
noLate <-c(noLate, which(GBIF_brach$latestAgeOrHighestStage == "late"))
noLate <-c(noLate, which(GBIF_brach$latestAgeOrHighestStage == "middle"))
noLate <- unique(noLate)

## Get no stage
noStage <- intersect(noLate, noEarly)

## Check formations
#View(data.frame(table(GBIF_brach$formation)))
noForm <- c()
noForm <- c(noForm, which(GBIF_brach$formation == ""))
noForm <- c(noForm, which(GBIF_brach$formation == "?"))
noForm <- c(noForm, which(GBIF_brach$formation == "A2-3"))
noForm <- c(noForm, which(GBIF_brach$formation == "A3-4"))
noForm <- c(noForm, which(GBIF_brach$formation == "B2"))
noForm <- c(noForm, which(GBIF_brach$formation == "C1"))
noForm <- c(noForm, which(GBIF_brach$formation == "Chalk"))
noForm <- c(noForm, which(GBIF_brach$formation == "Chalk Formation"))
noForm <- c(noForm, which(GBIF_brach$formation == "Etage Dd1 alpha"))
noForm <- c(noForm, which(GBIF_brach$formation == "Etage Dd1 beta"))
noForm <- c(noForm, which(GBIF_brach$formation == "Etage Dd1 gamma"))
noForm <- c(noForm, which(GBIF_brach$formation == "Etage Dd2"))
noForm <- c(noForm, which(GBIF_brach$formation == "Etage Dd3"))
noForm <- c(noForm, which(GBIF_brach$formation == "Etage Ee1"))
noForm <- c(noForm, which(GBIF_brach$formation == "Etage Ee2"))
noForm <- c(noForm, which(GBIF_brach$formation == "Etage F"))
noForm <- c(noForm, which(GBIF_brach$formation == "Etage Ff1"))
noForm <- c(noForm, which(GBIF_brach$formation == "Etage Ff2"))
noForm <- c(noForm, which(GBIF_brach$formation == "Etage Gg1"))
noForm <- c(noForm, which(GBIF_brach$formation == "Etage Gg2"))
noForm <- c(noForm, which(GBIF_brach$formation == "Etage Hh1"))
noForm <- c(noForm, which(GBIF_brach$formation == "IV Bd"))
noForm <- c(noForm, which(GBIF_brach$formation == "LOWER"))
noForm <- c(noForm, which(GBIF_brach$formation == "Stage D"))
noForm <- c(noForm, which(GBIF_brach$formation == "unkn. Miss. Chester"))
noForm <- c(noForm, which(GBIF_brach$formation == "unknown"))
noForm <- c(noForm, which(GBIF_brach$formation == "Unknown"))
noForm <- c(noForm, which(GBIF_brach$formation == "unknown formation"))
noForm <- c(noForm, which(GBIF_brach$formation == "Unnamed"))
noForm <- c(noForm, which(GBIF_brach$formation == "unnamed Unit 3"))
noForm <- c(noForm, which(GBIF_brach$formation == "Unrecorded"))
noForm <- c(noForm, which(GBIF_brach$formation == "Ordovician undiff."))
noForm <- unique(noForm)

## get intersect
droppers <- intersect(noForm, noStage)
GBIF_brach <- GBIF_brach[-droppers, ]

## Parition out original stage data for record
orig_brach_stages <- GBIF_brach[,c(25,26)]
colnames(orig_brach_stages) <- paste0(colnames(orig_brach_stages), "_OLD")

#### Cleaning up stages #####
## Clean up stages
#View(data.frame(table(GBIF_brach$latestAgeOrHighestStage)))
noLate <- c()
noLate <-c(noLate, which(GBIF_brach$latestAgeOrHighestStage == "early"))
noLate <-c(noLate, which(GBIF_brach$latestAgeOrHighestStage == "late"))
noLate <-c(noLate, which(GBIF_brach$latestAgeOrHighestStage == "middle"))
noLate <- unique(noLate)
GBIF_brach$latestAgeOrHighestStage[noLate] <- ""

#View(data.frame(table(GBIF_brach$earliestAgeOrLowestStage)))
noEarly <-c()
noEarly <-c(noEarly, which(GBIF_brach$earliestAgeOrLowestStage == "Early"))
noEarly <-c(noEarly, which(GBIF_brach$earliestAgeOrLowestStage == "Middle"))
noEarly <-c(noEarly, which(GBIF_brach$earliestAgeOrLowestStage == "Late"))
noEarly <-c(noEarly, which(GBIF_brach$earliestAgeOrLowestStage == "Unnamed"))
noEarly <-c(noEarly, which(GBIF_brach$earliestAgeOrLowestStage == "indet."))
noEarly <- unique(noEarly)
GBIF_brach$earliestAgeOrLowestStage[noEarly] <- ""

## Clean stages - might move this to main cleaning scripts
## First, read in stages
stages <- read.csv("data/metadata/macrostrat_raw.csv", row.names = 1, header = T)
stages <- stages[!duplicated(stages[,c("name", "t_age", "b_age")]),]
stage_names <- stages$name

## Now clean
source("functions/clean.stage.names.R")
GBIF_brach <- clean.stage.names(data = GBIF_brach, columns = c("latestAgeOrHighestStage", "earliestAgeOrLowestStage"))

## Manual updates (spotted along the way)
GBIF_brach[which(GBIF_brach$earliestAgeOrLowestStage == "New Scotland Ls"),"formation"] <- "New Scotland Ls"
GBIF_brach[which(GBIF_brach$earliestAgeOrLowestStage == "Oolite"),"formation"] <- "Inferior Oolite"
GBIF_brach[which(GBIF_brach$earliestAgeOrLowestStage == "White Jura"),"formation"] <- "White Jura"

## Now check bivalves against Macrostrat names
## Run bulk.update.stages (aggregation of all specific name changes identified thus far), then check for other errors
source("functions/bulk.update.stages.R")
GBIF_brach$earliestAgeOrLowestStage <- bulk.update.stages(GBIF_brach$earliestAgeOrLowestStage)

## Inspect and update function
#View(data.frame(table(GBIF_brach$earliestAgeOrLowestStage)))
#View(data.frame(stage_names))

## Load function and run again
#source("functions/bulk.update.stages.R")
#GBIF_brach$earliestAgeOrLowestStage <- bulk.update.stages(GBIF_brach$earliestAgeOrLowestStage)

## Now run for late stages, then check
GBIF_brach$latestAgeOrHighestStage <- bulk.update.stages(GBIF_brach$latestAgeOrHighestStage)

## Then inspect and add to function
#View(data.frame(table(GBIF_brach$latestAgeOrHighestStage)))
#View(data.frame(stage_names))

## re-load function and run again
#source("functions/bulk.update.stages.R")
#GBIF_brach$latestAgeOrHighestStage <- bulk.update.stages(GBIF_brach$latestAgeOrHighestStage)

##### Splitting ages #####
#View(data.frame(table(GBIF_brach$latestAgeOrHighestStage)))
latePunct <- c()
latePunct <- c(latePunct, unique(GBIF_brach$latestAgeOrHighestStage[which(str_detect(GBIF_brach$latestAgeOrHighestStage, pattern = "[:punct:]"))]))
latePunct <- c(latePunct, GBIF_brach$latestAgeOrHighestStage[which(str_detect(GBIF_brach$latestAgeOrHighestStage, pattern = " to "))])
latePunct <- c(latePunct, GBIF_brach$latestAgeOrHighestStage[which(str_detect(GBIF_brach$latestAgeOrHighestStage, pattern = " or "))])
latePunct <- c(latePunct, GBIF_brach$latestAgeOrHighestStage[which(str_detect(GBIF_brach$latestAgeOrHighestStage, pattern = "[|]"))])
latePunct <- unique(latePunct)

#View(data.frame(table(GBIF_brach$earliestAgeOrLowestStage)))
earlyPunct <- c()
earlyPunct <- c(earlyPunct, unique(GBIF_brach$earliestAgeOrLowestStage[which(str_detect(GBIF_brach$earliestAgeOrLowestStage, pattern = "[:punct:]"))]))
earlyPunct <- c(earlyPunct, GBIF_brach$earliestAgeOrLowestStage[which(str_detect(GBIF_brach$earliestAgeOrLowestStage, pattern = " to "))])
earlyPunct <- c(earlyPunct, GBIF_brach$earliestAgeOrLowestStage[which(str_detect(GBIF_brach$earliestAgeOrLowestStage, pattern = " or "))])
earlyPunct <- c(earlyPunct, GBIF_brach$earliestAgeOrLowestStage[which(str_detect(GBIF_brach$earliestAgeOrLowestStage, pattern = "[|]"))])
earlyPunct <- unique(earlyPunct)

## Wittle down to those to be split
#View(data.frame(latePunct))
latePunct <- latePunct[c(1:2, 4, 6:9, 13, 16, 19:24, 26:32)]
#View(data.frame(latePunct))

#View(data.frame(earlyPunct))
earlyPunct <- earlyPunct[c(1:2, 4:16, 18:30, 32:35, 39, 43, 46:47, 49:54, 56:67, 69, 71:74, 77:80, 83:87)]
#View(data.frame(earlyPunct))

## get entries to be split
splitLate <- c()
for(i in 1:length(latePunct)){
  splitLate <- c(splitLate, which(GBIF_brach$latestAgeOrHighestStage == latePunct[i]))
}

splitEarly <- c()
for(i in 1:length(earlyPunct)){
  splitEarly <- c(splitEarly, which(GBIF_brach$earliestAgeOrLowestStage == earlyPunct[i]))
}

## Isolate stages from main data file
lateStage <- data.frame(GBIF_brach$latestAgeOrHighestStage)
colnames(lateStage) <- "latestAgeOrHighestStage1"

earlyStage <- data.frame(GBIF_brach$earliestAgeOrLowestStage)
colnames(earlyStage) <- "earliestAgeOrLowestStage1"

## Only 2 stages max for each
lateStage$latestAgeOrHighestStage2 <- ""
earlyStage$earliestAgeOrLowestStage2 <- ""

## Specify splitting strings/characters
p <- c('-', ',' , '/', ' to ', ' or ', '|')

## Split late stages
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

## Split late stages
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

## Finally, pass over each entry, only retaining Macrostrat compatible strings
stages <- read.csv("data/metadata/macrostrat_raw.csv", row.names = 1, header = T)
stages <- stages[!duplicated(stages[,c("name", "t_age", "b_age")]),]
source("functions/stage.checker.R")

## Run stage checker
earlyStage$earliestAgeOrLowestStage1 <- stage.checker(earlyStage$earliestAgeOrLowestStage1, stages)
earlyStage$earliestAgeOrLowestStage2 <- stage.checker(earlyStage$earliestAgeOrLowestStage2, stages)
lateStage$latestAgeOrHighestStage1 <- stage.checker(lateStage$latestAgeOrHighestStage1, stages)
lateStage$latestAgeOrHighestStage2 <- stage.checker(lateStage$latestAgeOrHighestStage2, stages)

## Combine new stage objects and update names
new_brach_stages <- cbind(earlyStage, lateStage)
colnames(new_brach_stages) <- paste0("Macrostrat_unit", 1:4)

## delete original stage data columns
GBIF_brach$earliestAgeOrLowestStage <- NULL
GBIF_brach$latestAgeOrHighestStage <- NULL

## Attach to dataset
GBIF_brach <- cbind(GBIF_brach, new_brach_stages, orig_brach_stages)

##### Splitting formations #####
## First, update two problem formations
GBIF_brach$formation[which(GBIF_brach$formation == "Upper and Lower Fezouata formations, undifferentiated")] <- "Upper Fezouata formation and Lower Fezouata formation"
GBIF_brach$formation[which(GBIF_brach$formation == "Bradforidien-Ferrugineus-Schichten")] <- "Bradforidian Ferrugineus Schichten"

## Now find formations to split
forms <- unique(GBIF_brach$formation[which(str_detect(GBIF_brach$formation, pattern = "[:punct:]"))])
forms <- c(forms, GBIF_brach$formation[which(str_detect(GBIF_brach$formation, pattern = " and "))])
forms <- c(forms, GBIF_brach$formation[which(str_detect(GBIF_brach$formation, pattern = " & "))])
forms <- c(forms, GBIF_brach$formation[which(str_detect(GBIF_brach$formation, pattern = " or "))])
forms <- unique(forms)

## Wittle down to those to split
#View(data.frame(forms))
forms <- forms[c(14, 16, 67, 122, 130, 153, 198, 205, 257, 259, 272, 299, 311, 318, 328, 348, 351, 361, 363, 369, 372:379)]
#View(data.frame(forms))

## get ages to be split
splitForms <- c()
for(i in 1:length(forms)){
  splitForms <- c(splitForms, which(GBIF_brach$formation == forms[i]))
}

## Isolate formation
formations <- data.frame(GBIF_brach$formation)
colnames(formations) <- "formation1"

## max 2 formations
formations$formation2 <- ""

## Specify string to split by
p <- c('/', '-', ' and ', ' & ', ' or ')

## Split formations
for(i in splitForms){
  for(m in p){
    if(str_detect(formations$formation1[i], pattern = fixed(m))){
      ## extract forms
      formVec <- unlist(str_split(formations$formation1[i], pattern = fixed(m)))
      ## assign to new columns
      for(f in 1:length(formVec)){
        formations[i,f] <- formVec[f]
      }
    }
  }
}

## delete original stage data columns
GBIF_brach$formation <- NULL

## Attach to dataset
GBIF_brach <- cbind(GBIF_brach, formations)

## Tidy formations, stages, and genera
## Use misspell to tidy up dipthongs and alternative spellings
source("functions/misspell.R")
GBIF_brach$genus <- misspell(GBIF_brach$genus)

## Drop punctuation from formations and stages (fossilbrush will clean taxa)
GBIF_brach$genus <- str_replace_all(GBIF_brach$genus, pattern = "[:punct:]", replacement = "")
GBIF_brach$formation1 <- str_replace_all(GBIF_brach$formation1, pattern = "[:punct:]", replacement = "")
GBIF_brach$formation2 <- str_replace_all(GBIF_brach$formation2, pattern = "[:punct:]", replacement = "")

## Correct capitalization
GBIF_brach$genus <- str_to_title(GBIF_brach$genus)
GBIF_brach$formation1 <- str_to_title(GBIF_brach$formation1)
GBIF_brach$formation2 <- str_to_title(GBIF_brach$formation2)

## Tidy up species - first, reduce to species name alone
splitSpecies <- str_split(GBIF_brach$species, pattern = fixed(" "))
splitSpecies <- lapply(1:length(splitSpecies), function(x){
  if(length(splitSpecies[[x]])>1){
    return(splitSpecies[[x]][2])
  } else {
    return(splitSpecies[[x]])
  }
})
GBIF_brach$species <- unlist(splitSpecies)

## Now to tidy up unknowns
#View(data.frame(table(GBIF_brach$species)))
gen.level <- c()
gen.level <- c(gen.level,which(GBIF_brach$species == ""))
GBIF_brach[gen.level, "species"] <- "sp."

## No punctuation or anything else

## Finally, drop entries that could conceivably fall within Meghalayan
recent <- c()
recent <- c(recent, which(GBIF_brach$Macrostrat_unit1 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit1 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit1 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit1 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit1 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit1 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit1 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit1 == regex("C1", ignore_case = T)))

recent <- c(recent, which(GBIF_brach$Macrostrat_unit2 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit2 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit2 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit2 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit2 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit2 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit2 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit2 == regex("C1", ignore_case = T)))

recent <- c(recent, which(GBIF_brach$Macrostrat_unit3 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit3 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit3 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit3 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit3 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit3 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit3 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit3 == regex("C1", ignore_case = T)))

recent <- c(recent, which(GBIF_brach$Macrostrat_unit4 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit4 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit4 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit4 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit4 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit4 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit4 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(GBIF_brach$Macrostrat_unit4 == regex("C1", ignore_case = T)))
recent <- unique(recent)
if(length(recent) > 0){
  GBIF_brach <- GBIF_brach[-recent,]
}

## Export final version of brachiopods
saveRDS(GBIF_brach, file = "data/GBIF/GBIF_brach_end_1_4.Rds")

#### Combining GBIF datasets ####
GBIF_brach <- readRDS("data/GBIF/GBIF_brach_end_1_4.Rds")
GBIF_biv <- readRDS("data/GBIF/GBIF_biv_end_1_4.Rds")

## Standardise columns, rearrange, combine
GBIF_brach$formation3 <- ""
GBIF_brach$formation4 <- ""

## Rearrange
## Rearrange columns
#View(data.frame(colnames(GBIF_biv)))
GBIF_biv <- GBIF_biv[,c(25, 2, 3, 4, 5, 6, 7, 8, 30, 31, 32, 33, 21, 36, 37, 38, 39, 22, 23, 10, 11, 12, 29, 15, 16, 17, 18, 19, 20, 34, 35, 26, 24, 27)]

#View(data.frame(colnames(GBIF_brach)))
GBIF_brach <- GBIF_brach[,c(25, 2, 3, 4, 5, 6, 7, 8, 30, 31, 32, 33, 21, 36, 37, 38, 39, 22, 23, 10, 11, 12, 29, 15, 16, 17, 18, 19, 20, 34, 35, 26, 24, 27)]

## Recombine and export
GBIF <- rbind(GBIF_biv, GBIF_brach)

## Get rid of all spaces outside of string
GBIF$formation1 <- str_trim(GBIF$formation1)
GBIF$formation2 <- str_trim(GBIF$formation2)
GBIF$formation3 <- str_trim(GBIF$formation3)
GBIF$formation4 <- str_trim(GBIF$formation4)

## Finally, combine or stages into a single string
units <- GBIF[,c(9:12)]
chronostratigraphy <- sapply(1:nrow(units), function(x){
  ## get unique units
  strat <- unique(unlist(units[x,]))
  ## remove gaps if present and concatenate with a comma
  if(all(strat == "")){
    out <- ""
  } else {
    ## Drop any gaps left. Identify then drop
    if(any(strat == "")){
      ## drop
      strat <- strat[-which(strat == "")]
      if(length(strat)>1){
        out <- str_flatten(strat, collapse = ",")
      } else {
        out <- strat
      }
    } else {
      if(length(strat)>1){
        out <- str_flatten(strat, collapse = ",")
      } else {
        out <- strat
      }
    }
  }
  return(out)
})

## delete old strings
GBIF <- GBIF[,-c(9:12)]
GBIF <- cbind(GBIF, chronostratigraphy)

## Re-order
#View(data.frame(colnames(GBIF)))
GBIF <- GBIF[,c(1:8, 31, 9:30)]
#View(data.frame(colnames(GBIF)))

## Whoops - forgot to check for NMS, AMNH, and Peabody occurrences. Final check and prune.
View(data.frame(table(GBIF$publisher)))
droppers <- which(GBIF$publisher == "American Museum of Natural History")
droppers <- c(droppers, which(GBIF$publisher == "Yale University Peabody Museum"))
droppers <- unique(droppers)

## Drop
GBIF <- GBIF[-droppers,]

## Export
saveRDS(GBIF, file = "data/GBIF/GBIF.Rds")

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

