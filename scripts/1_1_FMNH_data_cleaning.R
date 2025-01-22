## 1. Initial data processing - FMNH data cleaning
## TJS, 17/01/2025

## If packages aren't installed, install them, then load them
packages <- c("stringr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(stringr)

#### FMNH data ####
rm(list = ls())

## Set working directory
setwd("~/R_packages/bivbrach")
home <- getwd()

## Load data
setwd("~/Library/CloudStorage/OneDrive-Nexus365/Bivalve_brachiopod/data")
FMNH_biv <- read.csv("FMNH_Bivalvia_June2024.csv")
FMNH_brach <- read.csv("FMNH_Brachiopoda_June2024.csv")
FMNH_biv_E1 <- read.csv("FMNH_AL004_BIVALVIA.csv")
FMNH_biv_E2 <- read.csv("FMNH_LI005_BIVALVIA.csv")
FMNH_biv_E3 <- read.csv("FMNH_SO047_BIVALVIA.csv")
FMNH_brach_E1 <- read.csv("FMNH_SO047_BRACHIOPODA.csv")[1,]
setwd(home)

## Start with FMNH. Classify as open and embargoed, then combine. Isolate formations first.
FMNH_biv$status <- "open"
FMNH_brach$status <- "open"
FMNH_biv_E1$status <- "embargoed"
FMNH_biv_E2$status <- "embargoed"
FMNH_biv_E3$status <- "embargoed"
FMNH_brach_E1$status <- "embargoed"

## Combine into single embargoed dataset for bivalves
## Create frame
FMNH_biv_E <- rbind(FMNH_biv_E1, FMNH_biv_E2, FMNH_biv_E3)
biv_E_frame <- as.data.frame(matrix("", ncol = ncol(FMNH_biv), nrow = nrow(FMNH_biv_E)))
colnames(biv_E_frame) <- colnames(FMNH_biv)

## Drop columns not included in main dataset
FMNH_biv_E <- FMNH_biv_E[,colnames(FMNH_biv_E) %in% colnames(FMNH_biv)]

## Populate frame
biv_E_frame[,match(colnames(FMNH_biv_E),colnames(FMNH_biv))] <- FMNH_biv_E

## Combine with other bivalves
FMNH_biv <- rbind(FMNH_biv, biv_E_frame)

## Same for brachiopods
brach_E_frame <- as.data.frame(matrix("", ncol = ncol(FMNH_brach), nrow = nrow(FMNH_brach_E1)))
colnames(brach_E_frame) <- colnames(FMNH_brach)

## Drop columns not included in main dataset
FMNH_brach_E1 <- FMNH_brach_E1[,colnames(FMNH_brach_E1) %in% colnames(FMNH_brach)]

## Populate frame
brach_E_frame[,match(colnames(FMNH_brach_E1),colnames(FMNH_brach))] <- FMNH_brach_E1

## Combine with other brachs
FMNH_brach <- rbind(FMNH_brach, brach_E_frame)

## Prune out deficient entries missing genus
## Bivalves
#View(data.frame(table(FMNH_biv$Genus)))
droppers <- c()
droppers <- c(droppers, which(FMNH_biv$Genus == ""))
droppers <- c(droppers, which(FMNH_biv$Genus == "HERE"))
## Check for "aff"
if(any(str_detect(FMNH_biv[,"Genus"], pattern = regex("aff", ignore_case = T)))){
  print("'aff's detected")
  ## None
}
## Check for "cf"
if(any(str_detect(FMNH_biv[,"Genus"], pattern = regex("cf", ignore_case = T)))){
  print("'cf's detected")
  ## None
}
## Check for dots
if(any(str_detect(FMNH_biv[,"Genus"], pattern = "\\."))){
  print("periods detected")
  ## None
}
## Check for "?"
if(any(str_detect(FMNH_biv[,"Genus"], pattern = "\\?"))){
  print("'?'s detected")
  ## None
}

## Check higher taxonomy
#View(data.frame(table(FMNH_biv$Phylum)))
# phyla fine
#View(data.frame(table(FMNH_biv$Class)))
# class fine

## Missing specimens
#View(data.frame(table(FMNH_biv$SpecimenStatus)))
droppers <- c(droppers, which(FMNH_biv$SpecimenStatus == "MISSING"))

## Missing or uncertain formations - needed for time
#View(data.frame(table(FMNH_biv$Formation)))
noForm <- which(FMNH_biv$Formation == "")
noLL <- which(is.na(FMNH_biv$Latitude1) | is.na(FMNH_biv$Longitude1))
droppers <- c(droppers, intersect(noForm, noLL))

## Drop droppers
droppers <- unique(droppers)
FMNH_biv <- FMNH_biv[-droppers,]

## Isolate formation data and split into 3
formations <- data.frame(FMNH_biv$Formation)
FMNH_biv$Formation <- NULL
colnames(formations) <- "formation1"

## Get maximum number of formations within a single record
## Find all punctuation
p <- unique(unlist(str_extract_all(formations$formation1, pattern = "[[:punct:]]")))
p

## 9 total
## Check entries associated with each
#View(data.frame(table(formations$formation1[which(str_detect(formations$formation1, pattern = "\\/"))])))
## Only punctuation that needs splitting by is /. The rest can be dropped and replace with spaces..
p <- paste0('\\', p[-2])
for(c in p){
  formations$formation1 <- str_replace_all(formations$formation1, pattern = c, replacement = " ")
}
## get split formations
split.forms <- str_split(formations$formation1, pattern = "\\/")
max.forms <- max(sapply(1:length(split.forms), function(x) length(split.forms[[x]])))
## maximum 3 formations
formations$formation2 <- ""
formations$formation3 <- ""

## Now to pass over each formation
for(i in 1:nrow(formations)){
  if(str_detect(formations$formation1[i], pattern = "\\/")){
    ## extract formations
    formVec <- unlist(str_split(formations$formation1[i], pattern = "\\/"))
    ## assign to new columns
    for(f in 1:length(formVec)){
      formations[i,f] <- formVec[f]
    }
  }
}

## Re-attach to dataset
FMNH_biv <- cbind(FMNH_biv, formations)

## Update all unknown species to sp.
#View(data.frame(table(FMNH_biv$Species)))
gen.level <- c()
gen.level <- c(gen.level, which(FMNH_biv$Species == ""))
## Check for "aff"
if(any(str_detect(FMNH_biv[,"Species"], pattern = regex("aff", ignore_case = T)))){
  print("'aff's detected")
  affs <- FMNH_biv[,"Species"][which(str_detect(FMNH_biv[,"Species"], pattern = regex("aff", ignore_case = T)))]
  affs
  ## No need to drop
}
## Check for "cf"
if(any(str_detect(FMNH_biv[,"Species"], pattern = regex("cf", ignore_case = T)))){
  print("'cf's detected")
  ## None
}
## Check for dots
if(any(str_detect(FMNH_biv[,"Species"], pattern = "\\."))){
  print("periods detected")
  ## None
}
## Check for "?"
if(any(str_detect(FMNH_biv[,"Species"], pattern = "\\?"))){
  print("'?'s detected")
  ## None
}
gen.level <- c(gen.level, which(FMNH_biv$Species == "incertae"))
gen.level <- unique(gen.level)

## Clean punctuation before converting all indeterminates to sp.
if(any(which(str_detect(FMNH_biv$Species, pattern = "[[:punct:]]")))){
  FMNH_biv$Species <- str_replace_all(FMNH_biv$Species, pattern = "[[:punct:]]", replacement = "")
}

FMNH_biv[gen.level,"Species"] <- "sp."

## Brachiopods
#View(data.frame(table(FMNH_brach$Genus)))
droppers <- c()
droppers <- c(droppers, which(FMNH_brach$Genus == ""))
## Check for "aff"
if(any(str_detect(FMNH_brach[,"Genus"], pattern = regex("aff", ignore_case = T)))){
  print("'aff's detected")
}
## Check for "cf"
if(any(str_detect(FMNH_brach[,"Genus"], pattern = regex("cf", ignore_case = T)))){
  print("'cf's detected")
}
## Check for dots
if(any(str_detect(FMNH_brach[,"Genus"], pattern = "\\."))){
  print("periods detected")
}
## Check for "?"
if(any(str_detect(FMNH_brach[,"Genus"], pattern = "\\?"))){
  print("'?'s detected")
}
## Check higher taxa
#View(data.frame(table(FMNH_brach$Phylum)))
## fine!
#View(data.frame(table(FMNH_brach$Class)))
## fine!

## Drop specimen status = missing entries
droppers <- c(droppers, which(FMNH_brach$SpecimenStatus == "MISSING"))
## Prune out entries missing lat or long entries
## Right now, not possible to geocode FMNH data as no locality strings. As such, need to stick to museum lat/long data.
## Drop entries missing lat/long AND formation data
#View(data.frame(table(FMNH_brach$Formation)))
noForm <- which(FMNH_brach$Formation == "")
noLL <- which(is.na(FMNH_brach$Latitude1) | is.na(FMNH_brach$Longitude1))
droppers <- c(droppers, intersect(noForm, noLL))
droppers <- unique(droppers)
FMNH_brach <- FMNH_brach[-droppers,]

## Isolate formation data and split into 3
formations <- data.frame(FMNH_brach$Formation)
FMNH_brach$Formation <- NULL
colnames(formations) <- "formation1"

## Get maximum number of formations within a single record
## Find all punctuation
p <- unique(unlist(str_extract_all(formations$formation1, pattern = "[[:punct:]]")))
p

## 9 total
## Check entries associated with each
#View(data.frame(table(formations$formation1[which(str_detect(formations$formation1, pattern = "\\/"))])))
## Only punctuation that needs splitting by is /. The rest can be dropped and replace with spaces..
p <- paste0('\\', p[-6])
for(c in p){
  formations$formation1 <- str_replace_all(formations$formation1, pattern = c, replacement = " ")
}
## get split formations
split.forms <- str_split(formations$formation1, pattern = "\\/")
max.forms <- max(sapply(1:length(split.forms), function(x) length(split.forms[[x]])))
## maximum 3 formations
formations$formation2 <- ""

## Now to pass over each formation
for(i in 1:nrow(formations)){
  if(str_detect(formations$formation1[i], pattern = "\\/")){
    ## extract formations
    formVec <- unlist(str_split(formations$formation1[i], pattern = "\\/"))
    ## assign to new columns
    for(f in 1:length(formVec)){
      formations[i,f] <- formVec[f]
    }
  }
}

## Re-attach to dataset
FMNH_brach <- cbind(FMNH_brach, formations)

## Update all unknown species
#View(data.frame(table(FMNH_brach$Species)))
gen.level <- c()
gen.level <- c(gen.level, which(FMNH_brach$Species == ""))
## Check for "aff"
if(any(str_detect(FMNH_brach[,"Species"], pattern = regex("aff", ignore_case = T)))){
  print("'aff's detected")
}
## Check for "cf"
if(any(str_detect(FMNH_brach[,"Species"], pattern = regex("cf", ignore_case = T)))){
  print("'cf's detected")
}
## Check for dots
if(any(str_detect(FMNH_brach[,"Species"], pattern = "\\."))){
  print("periods detected")
}
## Check for "?"
if(any(str_detect(FMNH_brach[,"Species"], pattern = "\\?"))){
  print("'?'s detected")
}
gen.level <- unique(gen.level)
## Clean punctuation before converting all indeterminates to sp.
if(any(which(str_detect(FMNH_brach$Species, pattern = "[[:punct:]]")))){
  FMNH_brach$Species <- str_replace_all(FMNH_brach$Species, pattern = "[[:punct:]]", replacement = "")
}

FMNH_brach[gen.level,"Species"] <- "sp."

## Clean bivalve data
FMNH_biv$Phylum <- "Mollusca"
FMNH_biv$Class <- "Bivalvia"
FMNH_biv$Genus <- str_to_title(FMNH_biv$Genus)
FMNH_biv$Species <- tolower(FMNH_biv$Species)
FMNH_biv$formation1 <- str_to_title(FMNH_biv$formation1)
FMNH_biv$formation2 <- str_to_title(FMNH_biv$formation2)
FMNH_biv$formation3 <- str_to_title(FMNH_biv$formation3)

## Clean brachiopod data
FMNH_brach$Phylum <- "Brachiopoda"
FMNH_brach$Genus <- str_to_title(FMNH_brach$Genus)
FMNH_brach$Species <- tolower(FMNH_brach$Species)
FMNH_brach$formation1 <- str_to_title(FMNH_brach$formation1)
FMNH_brach$formation2 <- str_to_title(FMNH_brach$formation2)

## Export data
setwd(home)
saveRDS(FMNH_biv, file = "data/museum/FMNH_biv.Rds")
saveRDS(FMNH_brach, file = "data/museum/FMNH_brach.Rds")



