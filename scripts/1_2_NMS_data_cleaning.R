## 1. Initial data processing - FMNH data cleaning
## TJS, 17/01/2025

## If packages aren't installed, install them, then load them
packages <- c("stringr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(stringr)

#### NMS data ####
rm(list = ls())

## Set working directory
setwd("~/R_packages/bivbrach")
home <- getwd()

setwd("~/Library/CloudStorage/OneDrive-Nexus365/Bivalve_brachiopod/data")
NMS_biv <- read.csv("NMS_bivalves.csv")
NMS_brach <- read.csv("NMS_brachiopoda.csv")
setwd(home)

## Start with bivalves - change gaps to 1
NMS_biv[which(NMS_biv[,"Number.of.specimens"] == ""),"Number.of.specimens"] <- 1

## Separate out duplicates
NMS_biv_dups <- NMS_biv[which(!NMS_biv[,"Number.of.specimens"] == 1),]
NMS_biv <- NMS_biv[which(NMS_biv[,"Number.of.specimens"] == 1),]

## Check non-singles
table(NMS_biv_dups[,"Number.of.specimens"])

## Tidy up
NMS_biv_dups <- NMS_biv_dups[which(!NMS_biv_dups$Number.of.specimens == ">10"),]
NMS_biv_dups[which(NMS_biv_dups$Number.of.specimens == "14 slides"),"Number.of.specimens"] <- 14
table(NMS_biv_dups[,"Number.of.specimens"])

## Duplicate
duped <- lapply(1:nrow(NMS_biv_dups), function(x){
  out <- NMS_biv_dups[rep(x,as.integer(NMS_biv_dups[x,"Number.of.specimens"])),]
})
duped <- do.call(rbind, duped)
duped$Number.of.specimens <- 1

## Recombine
NMS_biv <- rbind(NMS_biv, duped)

## Update column names
colnames(NMS_biv) <- c("accessionNumber", "n", "genus", "species", "locality", "period", "formation")
colnames(NMS_brach) <- c("x", "accessionNumber", "fullName", "locality", "formation", "stage", "period", "notes")

## Drop useless columns
NMS_biv <- NMS_biv[,c(1,3,4,5,6,7)]
NMS_brach <- NMS_brach[,c(2,3,4,5,6,7)]

## check for gaps and NAs, and genera for unusable names
#View(data.frame(table(NMS_biv$genus)))
droppers <- c()
droppers <- c(droppers, which(is.na(NMS_biv[,"genus"])))
droppers <- c(droppers, which(is.na(NMS_biv[,"locality"])))
droppers <- c(droppers, which(NMS_biv[,"genus"] == ""))
droppers <- c(droppers, which(NMS_biv[,"locality"] == ""))
droppers <- c(droppers, which(NMS_biv[,"genus"] == "undetermined"))
droppers <- c(droppers, which(NMS_biv[,"genus"] == "indeterminate"))
droppers <- c(droppers, which(NMS_biv[,"genus"] == "indetermiante"))
droppers <- c(droppers, which(NMS_biv[,"genus"] == "Indeterminate"))
droppers <- c(droppers, which(NMS_biv[,"genus"] == "bivave"))
droppers <- c(droppers, which(NMS_biv[,"genus"] == "bivaves"))
droppers <- c(droppers, which(NMS_biv[,"genus"] == "Myacites or Modiola"))
droppers <- c(droppers, which(NMS_biv[,"genus"] == "Myalinia & Myacites"))
droppers <- c(droppers, which(NMS_biv[,"genus"] == "Pholadomya  & Lima"))
droppers <- c(droppers, which(NMS_biv[,"genus"] == "various"))
## Check for "aff"
if(any(str_detect(NMS_biv[,"genus"], pattern = regex("aff", ignore_case = T)))){
  print("'aff's detected")
}
## Check for "cf"
if(any(str_detect(NMS_biv[,"genus"], pattern = regex("cf", ignore_case = T)))){
  print("'cf's detected")
  cfs <- unique(NMS_biv$genus[which(str_detect(NMS_biv$genus, pattern = regex("cf", ignore_case = T)))])
  cfs
  ## drop all
  droppers <- c(droppers, which(NMS_biv[,"genus"] %in% cfs))
}
## Check for dots
if(any(str_detect(NMS_biv[,"genus"], pattern = "\\."))){
  print("periods detected")
  dots <- unique(NMS_biv$genus[which(str_detect(NMS_biv$genus, pattern = "\\."))])
  dots
  ## prune out dots to retain
  dots <- dots[-c(4, 5, 6, 7)]
  dots
  droppers <- c(droppers,which(NMS_biv[,"genus"] %in% dots))
}
## Check for "?"
if(any(str_detect(NMS_biv[,"genus"], pattern = "\\?"))){
  print("'?'s detected")
  questions <- unique(NMS_biv$genus[which(str_detect(NMS_biv$genus, pattern = "\\?"))])
  questions
  droppers <- c(droppers,which(NMS_biv[,"genus"] %in% questions))
}
## Check formations for unusable entries
#View(data.frame(table(NMS_biv$formation)))
droppers <- c(droppers, which(is.na(NMS_biv[,"formation"])))
droppers <- c(droppers, which(NMS_biv[,"formation"] == ""))
droppers <- c(droppers, which(NMS_biv[,"formation"] == "basal similis-pulchra zone, from 30 ft above top furnace mine"))
droppers <- c(droppers, which(NMS_biv[,"formation"] == "Ga"))
droppers <- unique(droppers)
NMS_biv <- NMS_biv[-droppers,]

## Tidy up formations
formations <- data.frame(NMS_biv$formation)
NMS_biv$formation <- NULL
colnames(formations) <- "formation1"

## Get maximum number of formations within a single record
## Find all punctuation
p <- unique(unlist(str_extract_all(formations$formation1, pattern = "[[:punct:]]")))
p

## Check entries associated with each punctuation
#View(data.frame(table(formations$formation1[which(str_detect(formations$formation1, pattern = "\\."))])))
## No splitting required, can clean up all.
p <- paste0('\\', p)
for(c in p){
  formations$formation1 <- str_replace_all(formations$formation1, pattern = c, replacement = " ")
}

## Re-attach to dataset
NMS_biv <- cbind(NMS_biv, formations)

## Tidy up undetermined/indeterminate
#View(data.frame(table(NMS_biv$species)))
gen.level <- c()
gen.level <- c(gen.level,which(NMS_biv[,"species"] == ""))
gen.level <- c(gen.level,which(is.na(NMS_biv[,"species"])))
gen.level <- c(gen.level,which(NMS_biv[,"species"] == "undetermined"))
gen.level <- c(gen.level,which(NMS_biv[,"species"] == "indeterminate"))
gen.level <- c(gen.level,which(NMS_biv[,"species"] == "from"))
gen.level <- c(gen.level,which(NMS_biv[,"species"] == "iron"))
gen.level <- c(gen.level,which(NMS_biv[,"species"] == "sp"))
gen.level <- c(gen.level,which(NMS_biv[,"species"] == "sp "))
gen.level <- c(gen.level,which(NMS_biv[,"species"] == "sp,"))
gen.level <- c(gen.level,which(NMS_biv[,"species"] == "sp?"))
gen.level <- c(gen.level,which(NMS_biv[,"species"] == "gloucestershire"))
## Check for "aff"
if(any(str_detect(NMS_biv[,"species"], pattern = regex("aff", ignore_case = T)))){
  print("'aff's detected")
  affs <- unique(NMS_biv[,"species"][which(str_detect(NMS_biv[,"species"], pattern = regex("aff", ignore_case = T)))])
  affs
  ##
  affs <- affs[-c(1, 3, 18)]
  affs
  gen.level <- c(gen.level, which(NMS_biv[,"species"] %in% affs))
}
## Check for "cf"
if(any(str_detect(NMS_biv[,"species"], pattern = regex("cf", ignore_case = T)))){
  print("'cf's detected")
  cfs <- unique(NMS_biv$species[which(str_detect(NMS_biv$species, pattern = regex("cf", ignore_case = T)))])
  cfs
  ## change all
  gen.level <- c(gen.level, which(NMS_biv[,"species"] %in% cfs))
}
## Check for dots
if(any(str_detect(NMS_biv[,"species"], pattern = "\\."))){
  print("periods detected")
  dots <- unique(NMS_biv$species[which(str_detect(NMS_biv$species, pattern = "\\."))])
  dots
  ## prune out dots to retain
  dots <- dots[-c(2, 4, 8, 9, 38, 67, 68)]
  dots
  gen.level <- c(gen.level,which(NMS_biv[,"species"] %in% dots))
}
## Check for "?"
if(any(str_detect(NMS_biv[,"species"], pattern = "\\?"))){
  print("'?'s detected")
  questions <- unique(NMS_biv$species[which(str_detect(NMS_biv$species, pattern = "\\?"))])
  questions
  ## drop total unknowns, clean up others
  questions <- questions[c(6, 8, 12)]
  questions
  gen.level <- c(gen.level,which(NMS_biv[,"species"] %in% questions))
}
gen.level <- unique(gen.level)
## Clean punctuation before converting all indeterminates to sp.
if(any(which(str_detect(NMS_biv$species, pattern = "[[:punct:]]")))){
  NMS_biv$species <- str_replace_all(NMS_biv$species, pattern = "[[:punct:]]", replacement = "")
}
NMS_biv[gen.level,"species"] <- "sp."
NMS_biv$phylum <- "Mollusca"

## Finally, tidy up main columns
NMS_biv$genus <- str_to_title(NMS_biv$genus)
NMS_biv$species <- tolower(NMS_biv$species)
NMS_biv$formation1 <- str_to_title(NMS_biv$formation1)

## Now to do the same for brachiopods. First, need to split scientific name
names <- str_split_fixed(NMS_brach[,"fullName"], pattern = " ", n = 2)
colnames(names) <- c("genus", "species")
NMS_brach <- cbind(names, NMS_brach)
NMS_brach$fullName <- NULL

## dropping unusable records
#View(data.frame(table(NMS_brach$genus)))
droppers <- c()
droppers <- c(droppers, which(is.na(NMS_brach[,"genus"])))
droppers <- c(droppers, which(is.na(NMS_brach[,"locality"])))
droppers <- c(droppers, which(NMS_brach[,"genus"] == ""))
droppers <- c(droppers, which(NMS_brach[,"locality"] == ""))
droppers <- c(droppers, which(NMS_brach[,"genus"] == "FOR"))
droppers <- c(droppers, which(NMS_brach[,"genus"] == "brachiopod"))
droppers <- c(droppers, which(NMS_brach[,"genus"] == "Brachiopoda"))
droppers <- c(droppers, which(NMS_brach[,"genus"] == "brachiopoda"))
droppers <- c(droppers, which(NMS_brach[,"genus"] == "maybe"))
droppers <- c(droppers, which(NMS_brach[,"genus"] == "need"))
droppers <- c(droppers, which(NMS_brach[,"genus"] == "no"))
droppers <- c(droppers, which(NMS_brach[,"genus"] == "spines"))
droppers <- c(droppers, which(NMS_brach[,"genus"] == "fossil-bearing"))
droppers <- c(droppers, which(NMS_brach[,"genus"] == "mixed"))
droppers <- c(droppers, which(NMS_brach[,"genus"] == "Indeterminate,"))
droppers <- c(droppers, which(NMS_brach[,"genus"] == "unidentified"))
droppers <- c(droppers, which(NMS_brach[,"genus"] == "productellidae"))
droppers <- c(droppers, which(NMS_brach[,"genus"] == "Rhynchonelloidea"))
## Check for "aff"
if(any(str_detect(NMS_brach[,"genus"], pattern = regex("aff", ignore_case = T)))){
  print("'aff's detected")
}
## Check for "cf"
if(any(str_detect(NMS_brach[,"genus"], pattern = regex("cf", ignore_case = T)))){
  print("'cf's detected")
  cfs <- unique(NMS_brach$genus[which(str_detect(NMS_brach$genus, pattern = regex("cf", ignore_case = T)))])
  cfs
  ## drop all
  droppers <- c(droppers, which(NMS_brach[,"genus"] %in% cfs))
}
## Check for dots
if(any(str_detect(NMS_brach[,"genus"], pattern = "\\."))){
  print("periods detected")
  dots <- unique(NMS_brach$genus[which(str_detect(NMS_brach$genus, pattern = "\\."))])
  dots
  ## prune out all dots
  droppers <- c(droppers,which(NMS_brach[,"genus"] %in% dots))
}
## Check for "?"
if(any(str_detect(NMS_brach[,"genus"], pattern = "\\?"))){
  print("'?'s detected")
  questions <- unique(NMS_brach$genus[which(str_detect(NMS_brach$genus, pattern = "\\?"))])
  questions
  droppers <- c(droppers,which(NMS_brach[,"genus"] %in% questions))
}
## Drop entries that don't have stage data and don't have usable formations (only brachiopod has stage data)
## first, get no stage
#View(data.frame(table(NMS_brach$stage)))
noStage <- c(which(NMS_brach[,"stage"] == ""), which(NMS_brach[,"stage"] == "?"))
#View(data.frame(table(NMS_brach$formation)))
noForm <- c()
noForm <- c(noForm, which(NMS_brach[,"formation"] == ""))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "?"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "4 ft above Charlestown Main Limestone"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "4th Limestone above ?___"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "base of Micraster coranguinum Zone"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "black shale bed interstratified with volcanic blue and buff ash"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "Chalk"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "Limestone band"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "MGS"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "need to see the register"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "No 19 Zone"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "No. 1 Bed"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "No. 1 Bed, Invertiel"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "No. 3 Bed"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "No.1 Bed"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "No.1 Limestone"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "P. zone"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "shale above Highfield Limestone"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "shale above tuff with Bykneuk Limestone"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "shale above Upper Longcraig Limestone"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "shale band above 4 foot coal Ironstone Measures"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == 'shale in connection with "Blue" coal Seam'))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "shale under Top Hosie Limestone"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == 'shale underlying the "Blue" coal seam'))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "shales above bone bed"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "shales above Charlestown Main Limestone"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "shales above Lower Sandstone"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "Shales below 2nd Abden Limestone"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "Stage D"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "Stage D1y"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "Stage D1Y"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "Stage E"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "Stage E2"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "Stage F"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "Stage F Barrande"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "Stage F2"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "T. inconstans beds"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "top cor-test. Zone"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "top Micraster cor. Test. Zone"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "top of Merameca Group"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "top of Terebratulina gracilis Zone"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "unknown"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "zone 19"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "Zone S. Vaughan"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "Zone Terebratulina gracilis"))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "Zone with Holaster planus"))
droppers <- c(droppers, intersect(noStage, noForm))
droppers <- unique(droppers)
NMS_brach <- NMS_brach[-droppers,]

## Isolate formation data and split into 3
formations <- data.frame(NMS_brach$formation)
NMS_brach$formation <- NULL
colnames(formations) <- "formation1"

## Get maximum number of formations within a single record
## Find all punctuation
p <- unique(unlist(str_extract_all(formations$formation1, pattern = "[[:punct:]]")))
p

## 9 total
## Check entries associated with each
#View(data.frame(table(formations$formation1[which(str_detect(formations$formation1, pattern = '\"'))])))
## Split by hyphen and semi-colon
p <- paste0('\\', p[-c(3, 10)])
p
for(c in p){
  formations$formation1 <- str_replace_all(formations$formation1, pattern = c, replacement = " ")
}
## get split formations - maximum of 2.
split.forms <- c(str_split(formations$formation1, pattern = "\\-"), str_split(formations$formation1, pattern = "\\;"))
max.forms <- max(sapply(1:length(split.forms), function(x) length(split.forms[[x]])))
## maximum 3 formations
formations$formation2 <- ""

## Now to find offending rows
splitters <- c()
splitters <- c(splitters, which(str_detect(formations$formation1, pattern = '\\;')))
splitters <- c(splitters, which(str_detect(formations$formation1, pattern = '\\-')))
splitters <- unique(splitters)

## Remove keepers and check again
formations$formation1[splitters]
splitters <- splitters[-c(16:33)]
formations$formation1[splitters]

for(i in splitters){
  if(str_detect(formations$formation1[i], pattern = "\\;")){
    ## extract formations
    formVec <- unlist(str_split(formations$formation1[i], pattern = "\\;"))
    ## assign to new columns
    for(f in 1:length(formVec)){
      formations[i,f] <- formVec[f]
    }
  }
  if(str_detect(formations$formation1[i], pattern = "\\-")){
    ## extract formations
    formVec <- unlist(str_split(formations$formation1[i], pattern = "\\-"))
    ## assign to new columns
    for(f in 1:length(formVec)){
      formations[i,f] <- formVec[f]
    }
  }
}

## Re-attach to dataset
NMS_brach <- cbind(NMS_brach, formations)

## Clean stages
qmarks <- unique(NMS_brach$stage[which(str_detect(NMS_brach$stage, pattern = "\\?"))])
qmarks
NMS_brach[which(NMS_brach[,"stage"] %in% qmarks),"stage"] <- str_replace_all(NMS_brach[which(NMS_brach[,"stage"] %in% qmarks),"stage"], pattern = "\\?", replacement = "")

## Tidy up undetermined/indeterminate species
#View(data.frame(table(NMS_brach$species)))
gen.level <- c()
gen.level <- c(gen.level,which(NMS_brach[,"species"] == ""))
gen.level <- c(gen.level,which(is.na(NMS_brach[,"species"])))
gen.level <- c(gen.level,which(NMS_brach[,"species"] == "Group"))
gen.level <- c(gen.level,which(NMS_brach[,"species"] == "indet"))
gen.level <- c(gen.level,which(NMS_brach[,"species"] == "indetermined"))
gen.level <- c(gen.level,which(NMS_brach[,"species"] == "invertebrates"))
gen.level <- c(gen.level,which(NMS_brach[,"species"] == "brachiopod"))
gen.level <- c(gen.level,which(NMS_brach[,"species"] == "brachiopods"))
## Check for "aff"
if(any(str_detect(NMS_brach[,"species"], pattern = regex("aff", ignore_case = T)))){
  print("'aff's detected")
  affs <- unique(NMS_brach[,"species"][which(str_detect(NMS_brach[,"species"], pattern = regex("aff", ignore_case = T)))])
  affs
  ##
  affs <- affs[-c(9, 10)]
  affs
  gen.level <- c(gen.level, which(NMS_brach[,"species"] %in% affs))
}
## Check for "cf"
if(any(str_detect(NMS_brach[,"species"], pattern = regex("cf", ignore_case = T)))){
  print("'cf's detected")
  cfs <- unique(NMS_brach$species[which(str_detect(NMS_brach$species, pattern = regex("cf", ignore_case = T)))])
  cfs
  ## change all
  gen.level <- c(gen.level, which(NMS_brach[,"species"] %in% cfs))
}
## Check for dots
if(any(str_detect(NMS_brach[,"species"], pattern = "\\."))){
  print("periods detected")
  dots <- unique(NMS_brach$species[which(str_detect(NMS_brach$species, pattern = "\\."))])
  dots
  ## prune out dots to retain
  dots <- dots[-c(42, 44)]
  dots
  gen.level <- c(gen.level,which(NMS_brach[,"species"] %in% dots))
}
## Check for "?"
if(any(str_detect(NMS_brach[,"species"], pattern = "\\?"))){
  print("'?'s detected")
  questions <- unique(NMS_brach$species[which(str_detect(NMS_brach$species, pattern = "\\?"))])
  questions
  ## drop total unknowns, clean up others
  questions <- questions[c(2, 6)]
  questions
  gen.level <- c(gen.level,which(NMS_brach[,"species"] %in% questions))
}
gen.level <- unique(gen.level)
## Clean punctuation before converting all indeterminates to sp.
if(any(which(str_detect(NMS_brach$species, pattern = "[[:punct:]]")))){
  NMS_brach$species <- str_replace_all(NMS_brach$species, pattern = "[[:punct:]]", replacement = "")
}
NMS_brach[gen.level,"species"] <- "sp."
NMS_brach$phylum <- "Brachiopoda"

## Finally, tidy up main columns
NMS_brach$genus <- str_to_title(NMS_brach$genus)
NMS_brach$species <- tolower(NMS_brach$species)
NMS_brach$formation1 <- str_to_title(NMS_brach$formation1)
NMS_brach$formation2 <- str_to_title(NMS_brach$formation2)

## Export
saveRDS(NMS_biv, file = "data/museum/NMS_biv.Rds")
saveRDS(NMS_brach, file = "data/museum/NMS_brach.Rds")
View(data.frame(table(NMS_brach$species)))



