## 1.5 Initial data processing - PDBD cleaning
## Started by TJS on 08/01/2024

## If packages aren't installed, install them, then load them
packages <- c("fossilbrush", "stringr", "divDyn")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(fossilbrush)
library(stringr)
library(divDyn)

## Clean directory
rm(list = ls())

#### Loading data ####
## Option A: November 2023 dataset
## Load raw PBDB data
#raw_PBDB <- readRDS("data/unclean_data/PBDB_Nov23.Rds")

## Isolate bivalve and brachiopod data
#PBDB <- raw_PBDB[c(which(raw_PBDB$phylum == "Brachiopoda"),which(raw_PBDB$class == "Bivalvia")),]
#rm(raw_PBDB)

## Download latest version
#library(paleobioDB)
#raw_PBDB_bivalves <- pbdb_occurrences(limit = "all", vocab = "pbdb", base_name = "Bivalvia",
#                                      show = c("coll", "class", "coords", "paleoloc", "strat", "stratext", "lith", "env"))

#raw_PBDB_brachiopods <- pbdb_occurrences(limit = "all", vocab = "pbdb", base_name = "Brachiopoda",
#                                      show = c("coll", "class", "coords", "paleoloc", "strat", "stratext", "lith", "env"))

## combine
#PBDB <- rbind(raw_PBDB_bivalves, raw_PBDB_brachiopods)

## trim down to necessary columns
#PBDB <- PBDB[,c(which(colnames(PBDB) %in% colnames(raw_PBDB)),37)]

## Export
#saveRDS(PBDB, "data/unclean_data/PBDB_biv_brach_Apr25.Rds")

## Or, load data that was used for these analyses
PBDB <- readRDS("data/unclean_data/PBDB_biv_brach_Apr25.Rds")

#### Taxonomic filtering and cleaning ####
## First, check accepted ranks
#View(data.frame(table(PBDB$accepted_rank)))

## Only retain those of genus, species, subgenus, and subspecies
PBDB <- PBDB[PBDB$accepted_rank %in% c("genus", "species", "subgenus", "subspecies"),]

## Replace PBDB default missing data entry with NA for other taxonomic levels
PBDB[grep("NO_", PBDB[,"class"]),"class"] <- ""
PBDB[grep("NO_", PBDB[,"order"]), "order"] <- ""
PBDB[grep("NO_", PBDB[,"family"]), "family"] <- ""
PBDB[grep("NO_", PBDB[,"genus"]), "genus"] <- ""

## Drop all genera with no information
PBDB <- PBDB[-which(PBDB$genus == ""),]

## Stip out subgenera in brackets
PBDB$genus <- str_split_i(PBDB$genus, pattern = ' ', i = 1)

## Standardise dipthongs for genera
source("functions/misspell.R")
PBDB$genus <- misspell(PBDB$genus)

#### Adding environmental covariates ####
## Define lithology, bathymetric, and reefal categories
data(keys)
PBDB$lithCat <- categorize(PBDB$lithology1, keys$lith)
PBDB$bathCat <- categorize(PBDB$environment, keys$bath)
PBDB$reefCat <- categorize(PBDB$environment, keys$reefs)
PBDB$reefCat[PBDB$lithCat == "siliciclastic" & PBDB$environment == "marine indet."] <- "non-reef"

## Drop unlithified sediments - effort to reduce sampling bias
PBDB <- PBDB[-which(PBDB$lithification1=="unlithified"),]

#### Updating chronostratigraphy and temporal filtering ####
## Adding stages of Kocsis et al. (2019)
dat <- PBDB
data(stages)

## Set column to use as label to 'name'
colnames(stages)[colnames(stages)=="stage"] <- "name"

## Update names of stages to match PBDB
stages[8, "name"] <- "Wuliuan"
stages[13, "name"] <- "Stage 10"

## Categorise occurrences using stage intervals, both early and late. Gives stage ID numbers according to Kocsis et al.
stgMin <- categorize(dat[ ,"early_interval"], keys$stgInt)
stgMax <- categorize(dat[ ,"late_interval"], keys$stgInt)

## convert to numeric
stgMin <- as.numeric(stgMin)
stgMax <- as.numeric(stgMax)

## initialise empty container
dat$stg <- rep(NA, nrow(dat))

## select entries, where
stgCondition <- c(
  # the early and late interval fields indicate the same stage
  which(stgMax==stgMin),
  # or the late_interval field is empty
  which(stgMax==-1))

## in these entries, use the stg indicated by the early_interval
dat$stg[stgCondition] <- stgMin[stgCondition]

## Correcting Cambrian and Ordovician collections
## Load updated Cambrian collection labels. Collection numbers are names, stage numbers are values.
load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/cambStrat.RData"))

## Get collection numbers
colls <- as.character(dat$collection_no)

## Which are Cambrian?
bool <- colls%in%names(camb)

## Isolate the Cambrian collections
subColls <- colls[bool]

## Use collection number names to get stage ID numbers
subStg <- camb[subColls]

## Create a copy of original object
newStg <- dat$stg

## replace the missing entries and reattach
newStg[bool]  <- subStg
dat$stg <- newStg

## Now to update intervals - first, delete old entries
dat[which(bool),"early_interval"] <- ""
dat[which(bool),"late_interval"] <- ""

## Then add updated intervals to early interval
dat[which(bool),"early_interval"] <- stages[subStg,"name"]

## Now to do same with Ordovician collections
load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/ordStrat.RData"))

## Create new data frame to record new stage data - only get those missing stages
new <- unique(dat[is.na(dat$stg), c("collection_no", "early_interval", "late_interval", "zone", "formation", "max_ma", "min_ma", "stg")])
## Total 72066 have not been assigned stages yet

## For each row in format(ions), check for matches between new formations and row formation. Add stg if match
for (i in 1:nrow(format)){
  ix <- which((as.character(new$formation) == as.character(format$formation[i])))
  new$stg[ix] <- format$stg[i]
}

## For each row in max.int(ervals), check for match with early_interval of data. Record ID if so.
for (i in 1:nrow(max.int)){
  ix <- which(as.character(new$early_interval) == as.character(max.int$Max.int[i]))
  new$stg[ix] <- max.int$stg.1[i]
}

## Now to check do the same with late intervals, and check for differences
## Create blank case
stg2 <- rep(NA, nrow(new))
## Now check late intervals
for (i in 1:nrow(max.int)){
  ix <- which(as.character(new$late_interval) == as.character(max.int$Max.int[i]))
  stg2[ix] <- max.int$stg.1[i]
}

## Bigger the number, the younger the interval
## Late interval can only be bigger
## So if bigger, straddles multiple intervals. Should ignore.
ix <- which(new$stg<stg2)
new$stg[ix] <- NA

## Now matching to zones
for (i in 1:nrow(zones)){
  ix <- which(as.character(new$zone) == as.character(zones$zone[i]))
  new$stg[ix] <- zones$stg[i]
}

# distill to collections with stage data
new2 <- new[!is.na(new$stg),]

## Get named vector of stg numbers, names = collection numbers
ord <- new2$stg
names(ord) <- new2$collection_no

## Get the collection identifiers of occurrences in the total dataset
colls <- as.character(dat$collection_no)

## Get which are present in the newly gathered data?
bool <- colls%in%names(ord)

## collection identifiers of the occurrences of only these collections
subColls <- colls[bool]

## order/assign the stg accordingly
subStg <- ord[subColls]

## copy original
newStg <- dat$stg

## replace the missing entries
newStg[bool] <- subStg

## make sure things are OK
origTab <- table(dat$stg)
newTab <- table(newStg)
sum(newTab)-sum(origTab)
sum(newTab)

## add to the full table
dat$stg <- newStg

## Update intervals again - first, remove
dat[which(bool),"early_interval"] <- ""
dat[which(bool),"late_interval"] <- ""

## Then add updated intervals to early interval
dat[which(bool),"early_interval"] <- stages[subStg,"name"]

## Once finished, update PBDB object
PBDB <- dat

## Remove Stg column - will time bin separately as one.
PBDB$stg <- NULL

### Cleaning up rest of Chronostratigraphy
## use fossilbrush to update Chronostratigraphy
PBDB <- chrono_scale(PBDB,  tscale = "GTS2020", srt = "early_interval", end = "late_interval",
                     max_ma = "max_ma", min_ma = "min_ma", verbose = FALSE)

## set new chronostratigraphy as "max_ma" and "min_ma", then remove added columns newFAD and newLAD
PBDB$max_ma <- PBDB$newFAD
PBDB$min_ma <- PBDB$newLAD
PBDB <- PBDB[,c(1:28)]

## check for and remove any entries with nonsensical entries (LAD older than FAD)
if(any(PBDB$max_ma < PBDB$min_ma)){
  PBDB <- PBDB[-which(PBDB$max_ma < PBDB$min_ma),]
}

## Remove entries with intervals that terminate at present
PBDB <- PBDB[-which(PBDB$min_ma == 0),]

#### Add ecological data based on Guo et al. (2023) ####
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
brachiopod.ecology <- read.csv("data/metadata/Guo2023_brachiopod_ecology.csv", row.names = 1)
bivalve.ecology <- read.csv("data/metadata/Guo2023_bivalve_ecology.csv", row.names = 1)
key <- rbind(brachiopod.ecology, bivalve.ecology)

## Read in function
source("functions/add.ecology.IDs.R")
PBDB <- add.ecology.IDs(data = PBDB, key)

#### Drop terrestrial records (if any) ####
omitEnv <- c(
  "\"floodplain\"", "alluvial fan", "cave", "\"channel\"", "channel lag" ,
  "coarse channel fill", "crater lake", "crevasse splay", "dry floodplain",
  "delta plain", "dune", "eolian indet.", "fine channel fill", "fissure fill",
  "fluvial indet.", "fluvial-lacustrine indet.", "fluvial-deltaic indet.",
  "glacial", "interdune", "karst indet.", "lacustrine - large",
  "lacustrine - small", "lacustrine delta front", "lacustrine delta plain",
  "lacustrine deltaic indet.", "lacustrine indet.",
  "lacustrine interdistributary bay", "lacustrine prodelta", "levee", "loess",
  "mire/swamp", "pond", "sinkhole", "spring", "tar", "terrestrial indet.",
  "wet floodplain")
PBDB <- PBDB[!PBDB$environment%in%omitEnv,]

#### Rearrange and export ####
## Rearrange
PBDB <- PBDB[,c(1, 10, 4, 5, 2, 3, 6, 23, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 25, 21, 24, 22, 26:30)]

## Export data
saveRDS(PBDB, "data/PBDB/PBDB.Rds")

