## 1.6 Initial data processing - PDBD cleaning
## Started by TJS on 08/01/2024

## If packages aren't installed, install them, then load them
packages <- c("fossilbrush", "rnaturalearth", "rnaturalearthdata", "divvy", "stringr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(fossilbrush)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(divvy)

## install divvyCompanion from github and load
#library(remotes)
#install_github("PalaeoTom/divvyCompanion")
library(divvyCompanion)

## Clean directory
rm(list = ls())

#### Cleaning PBDB ####
## Load raw PBDB data
raw_PBDB <- readRDS("data/unclean_data/PBDB_Nov23.Rds")

## Clean time data
## Isolate bivalve and brachiopod data
raw_PBDB <- raw_PBDB[c(which(raw_PBDB$phylum == "Brachiopoda"),which(raw_PBDB$class == "Bivalvia")),]

## Update formations using Ali Cribbs's key
formations <- read.csv("data/metadata/AC_cleaned_PBDB_formations.csv", row.names = 1)[,c(2,4)]
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

#### Checking ages and coordinates ####
#View(data.frame(table(PBDB$paleolat)))
#View(data.frame(table(PBDB$paleolng)))
#View(data.frame(table(PBDB$max_ma)))
#View(data.frame(table(PBDB$min_ma)))
## All good!

#### Cleaning taxonomy ####
## Replace PBDB default missing data entry with NA for other taxonomic levels
PBDB[grep("NO_", PBDB[,"class"]),"class"] <- NA
PBDB[grep("NO_", PBDB[,"order"]), "order"] <- NA
PBDB[grep("NO_", PBDB[,"family"]), "family"] <- NA
PBDB[grep("NO_", PBDB[,"genus"]), "genus"] <- NA

## Check all classes with NA are Brachiopoda
all(PBDB[which(is.na(PBDB$class)),"phylum"] == "Brachiopoda")
## All good!

## Drop all genera with NA
PBDB <- PBDB[-which(is.na(PBDB$genus)), ]

## Manually inspect genera
#View(data.frame(table(PBDB$genus)))
## Only issue - subgenera in brackets. Should be stripped out. Use Regex search to reduce to first string.
PBDB$genus <- str_split_i(PBDB$genus, pattern = ' ', i = 1)

## Use misspell function to update genus, family, and order designations
source("functions/misspell.R")
PBDB$genus <- misspell(PBDB$genus)

## Check for punctuation and capitalisation
PBDB$genus <- str_replace_all(PBDB$genus, pattern = "[:punct:]", replacement = "")
PBDB$genus <- str_to_title(PBDB$genus)

#### Adding environmental covariates ####
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
PBDB <- add.occ.covariate(PBDB, name = "occLith", ref = "lithology1", varsLabs = c("carb", "sili"), var1 = carb, var2 = clast)
PBDB <- add.occ.covariate(PBDB, name = "occEnv", ref = "environment", varsLabs = c("shal", "deep"), var1 = shallow, var2 = deep)
PBDB <- add.occ.covariate(PBDB, name = "occReef", ref = "environment", varsLabs = c("reef", "noRf"), var1 = reefal, var2 = nonreefal)

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
brachiopod.ecology <- read.csv("data/metadata/Guo2023_brachiopod_ecology.csv", row.names = 1)
bivalve.ecology <- read.csv("data/metadata/Guo2023_bivalve_ecology.csv", row.names = 1)
key <- rbind(brachiopod.ecology, bivalve.ecology)

## Read in function
source("functions/add.ecology.IDs.R")
PBDB <- add.ecology.IDs(data = PBDB, key)

## Export data
saveRDS(PBDB, "data/PBDB/PBDB.Rds")
