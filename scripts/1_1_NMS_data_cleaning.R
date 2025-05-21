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

## Check for holocene entries and drop
#View(data.frame(table(NMS_biv$period)))
## No Bivalves
#View(data.frame(table(NMS_brach$period)))
## No holocene brachiopods

#### Bivalves ####
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
## No higher taxa to check.

## Check formations for unusable entries - need formations as no stage data
#View(data.frame(table(NMS_biv$formation)))
droppers <- c(droppers, which(NMS_biv[,"formation"] == ""))
droppers <- c(droppers, which(NMS_biv[,"formation"] == "etage G"))
droppers <- c(droppers, which(NMS_biv[,"formation"] == "basal similis-pulchra zone, from 30 ft above top furnace mine"))
droppers <- c(droppers, which(NMS_biv[,"formation"] == "Ga"))
droppers <- unique(droppers)
NMS_biv <- NMS_biv[-droppers,]

#### Splitting formations #####
## Manually inspect
#View(data.frame(table(NMS_biv$formation)))

## Find all punctuation
p <- unique(unlist(str_extract_all(NMS_biv$formation, pattern = "[[:punct:]]")))

## Check to see if any spliting required.
#View(data.frame(table(NMS_biv$formation[which(str_detect(NMS_biv$formation, pattern = fixed(p[5])))])))

## No splitting required. Can clean up all
for(c in p){
  NMS_biv$formation <- str_replace_all(NMS_biv$formation, pattern = fixed(c), replacement = " ")
}

#### Tidying up locality info ####
## No lat/long - need locality data
#View(data.frame(table(NMS_biv$locality)))
noLoc <- c()
noLoc <- c(noLoc, which(NMS_biv$locality == "unknown locality"))
NMS_biv <- NMS_biv[-noLoc, ]

#### Tidying up taxonomy ####
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
  affs <- affs[-c(1, 4)]
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
  dots <- dots[-c(4, 5, 12)]
  dots
  gen.level <- c(gen.level,which(NMS_biv[,"species"] %in% dots))
}
## Check for "?"
if(any(str_detect(NMS_biv[,"species"], pattern = "\\?"))){
  print("'?'s detected")
  questions <- unique(NMS_biv$species[which(str_detect(NMS_biv$species, pattern = "\\?"))])
  questions
  ## drop all
  gen.level <- c(gen.level,which(NMS_biv[,"species"] %in% questions))
}
gen.level <- unique(gen.level)
## Clean punctuation before converting all indeterminates to sp.
if(any(which(str_detect(NMS_biv$species, pattern = "[[:punct:]]")))){
  NMS_biv$species <- str_replace_all(NMS_biv$species, pattern = "[[:punct:]]", replacement = "")
}
NMS_biv[gen.level,"species"] <- "sp."
NMS_biv$phylum <- "Mollusca"

## Clean up punctuation
NMS_biv$formation <- str_replace_all(NMS_biv$formation, pattern = "[:punct:]", replacement = "")

## Format capitalization
NMS_biv$formation <- str_to_title(NMS_biv$formation)
NMS_biv$species <- tolower(NMS_biv$species)

#### Brachiopods ####
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
## No higher taxa.

## Drop entries that don't have stage data and don't have usable formations (only brachiopod has stage data)
## first, get no stage
#View(data.frame(table(NMS_brach$stage)))
noStage <- c()
noStage <- c(noStage, which(NMS_brach[,"stage"] == ""))
noStage <- c(noStage, which(NMS_brach[,"stage"] == "unknown"))
noStage <- unique(noStage)
#View(data.frame(table(NMS_brach$formation)))
noForm <- c()
noForm <- c(noForm, which(NMS_brach[,"formation"] == ""))
noForm <- c(noForm, which(NMS_brach[,"formation"] == "Etage E"))
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

#### Cleaning up stages #####
## Isolate original
orig_brach_stages <- NMS_brach[,"stage"]

#View(data.frame(table(NMS_brach$stage)))
noStage <- c()
noStage <- c(noStage, which(NMS_brach[,"stage"] == "unknown"))
noStage <- unique(noStage)
NMS_brach[noStage,"stage"] <- ""

## Clean stages
## First, read in stages
stages <- read.csv("data/metadata/macrostrat_raw.csv", row.names = 1, header = T)
stages <- stages[!duplicated(stages[,c("name", "t_age", "b_age")]),]
stage_names <- stages$name

## Now clean
source("functions/clean.stage.names.R")
NMS_brach <- clean.stage.names(data = NMS_brach, columns = "stage")

## Now check against Macrostrat names
## Run bulk.update.stages (aggregation of all specific name changes identified thus far), then check for other errors
source("functions/bulk.update.stages.R")
NMS_brach$stage <- bulk.update.stages(NMS_brach$stage)

## Inspect and update function
#View(data.frame(table(NMS_brach$stage)))
#View(data.frame(stage_names))

## Re-load and run function again after updates
#source("functions/bulk.update.stages.R")
#NMS_brach$stage <- bulk.update.stages(NMS_brach$stage)

## Split stages
#View(data.frame(table(NMS_brach$stage)))
splitStages <- c()
splitStages <- c(splitStages, unique(NMS_brach$stage[which(str_detect(NMS_brach$stage, pattern = "[:punct:]"))]))
splitStages <- unique(splitStages)

## Weed out those that don't need splitting
#View(data.frame(splitStages))
splitStages <- splitStages[c(1:2, 4:11, 14:29, 31:32)]
#View(data.frame(splitStages))

## get intervals to be split
splitters <- c()
for(i in 1:length(splitStages)){
  splitters <- c(splitters, which(NMS_brach$stage == splitStages[i]))
}

## Isolate stages
newStage <- data.frame(NMS_brach$stage)
colnames(newStage) <- "Macrostrat_unit1"
newStage$Macrostrat_unit2 <- ""

## Specify characters to split by
p <- c('-', ',')

## Split early stages
for(i in splitters){
  for(m in p){
    if(str_detect(NMS_brach$stage[i], pattern = fixed(m))){
      ## extract stages
      ageVec <- unlist(str_split(NMS_brach$stage[i], pattern = fixed(m)))
      ## assign to new columns
      for(f in 1:length(ageVec)){
        newStage[i,f] <- ageVec[f]
      }
    }
  }
}

## Finally, pass over each entry, only retaining Macrostrat compatible strings
stages <- read.csv("data/metadata/macrostrat_raw.csv", row.names = 1, header = T)
stages <- stages[!duplicated(stages[,c("name", "t_age", "b_age")]),]
source("functions/stage.checker.R")

## Run stage checker
newStage$Macrostrat_unit1 <- stage.checker(newStage$Macrostrat_unit1, stages)
newStage$Macrostrat_unit2 <- stage.checker(newStage$Macrostrat_unit2, stages)

## delete old stages
NMS_brach$stage <- NULL

## Attach to dataset
NMS_brach <- cbind(NMS_brach, newStage, "stage_OLD" = orig_brach_stages)

#### Splitting formations ####
## Manually inspect
#View(data.frame(table(NMS_brach$formation)))

## Manual correction of two formations split by 'or'. Leave one with hyphen None split by ' and ' or '|'
NMS_brach[which(NMS_brach$formation == "3rd rudist Zone of d'Orbigny, Chloritische Kreide (?=Lower Chalk; or ?Upper Greensand)"),"formation"] <- "Lower Chalk-Upper Greensand"
NMS_brach[which(NMS_brach$formation == "shales associated with Corbie Craig or North Coal, Lower Limestone Group"),"formation"] <- "Lower Limestone"

## Find all punctuation
p <- unique(unlist(str_extract_all(NMS_brach$formation, pattern = "[[:punct:]]")))

## Check entries associated with each
#View(data.frame(table(NMS_brach$formation[which(str_detect(NMS_brach$formation, pattern = fixed(p[1])))])))

## Split by hyphen and
p <-p[-c(3)]

## Remove punctuation without hyphen or semi colon
for(c in p){
  NMS_brach$formation <- str_replace_all(NMS_brach$formation, pattern = fixed(c), replacement = " ")
}

## Find formations to be split
## Now find formations to be split
forms <- unique(NMS_brach$formation[which(str_detect(NMS_brach$formation, pattern = "[:punct:]"))])
forms <- c(forms, NMS_brach$formation[which(str_detect(NMS_brach$formation, pattern = regex(" or ", ignore_case = T)))])
forms <- unique(forms)

## Wittle down to those to split
#View(data.frame(forms))
forms <- forms[c(1, 2, 3, 4, 13)]
#View(data.frame(forms))

## get ages to be split
splitForms <- c()
for(i in 1:length(forms)){
  splitForms <- c(splitForms, which(NMS_brach$formation == forms[i]))
}

## Isolate formation
formations <- data.frame(NMS_brach$formation)
colnames(formations) <- "formation1"

## max 2 formations
formations$formation2 <- ""

## Specify string to split by
p <- c('-')

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
NMS_brach$formation <- NULL

## Re-attach to dataset
NMS_brach <- cbind(NMS_brach, formations)

#View(data.frame(table(NMS_brach$locality)))
noLoc <- c()
noLoc <- c(noLoc, which(NMS_brach$locality == "no locality"))
noLoc <- c(noLoc, which(NMS_brach$locality == "not precise"))
noLoc <- c(noLoc, which(NMS_brach$locality == "unknown"))
noLoc <- unique(noLoc)
NMS_brach <- NMS_brach[-noLoc,]

## Address punctuation and capitalization for formations
NMS_brach$formation1 <- str_replace_all(NMS_brach$formation1, pattern = "[:punct:]", replacement = "")
NMS_brach$formation2 <- str_replace_all(NMS_brach$formation2, pattern = "[:punct:]", replacement = "")

NMS_brach$formation1 <- str_to_title(NMS_brach$formation1)
NMS_brach$formation2 <- str_to_title(NMS_brach$formation2)

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
  affs <- affs[-c(8, 9)]
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
  dots <- dots[-c(41, 42)]
  dots
  gen.level <- c(gen.level,which(NMS_brach[,"species"] %in% dots))
}
## Check for "?"
if(any(str_detect(NMS_brach[,"species"], pattern = "\\?"))){
  print("'?'s detected")
  questions <- unique(NMS_brach$species[which(str_detect(NMS_brach$species, pattern = "\\?"))])
  questions
  ## drop total unknowns, clean up others
  questions <- questions[-c(3, 5)]
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
NMS_brach$species <- tolower(NMS_brach$species)

## Finally, check and drop holocene/recent entries
## Brachiopod first
recent <- c()
recent <- c(recent, which(NMS_brach$Macrostrat_unit1 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(NMS_brach$Macrostrat_unit1 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(NMS_brach$Macrostrat_unit1 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(NMS_brach$Macrostrat_unit1 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(NMS_brach$Macrostrat_unit1 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(NMS_brach$Macrostrat_unit1 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(NMS_brach$Macrostrat_unit1 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(NMS_brach$Macrostrat_unit1 == regex("C1", ignore_case = T)))

recent <- c(recent, which(NMS_brach$Macrostrat_unit2 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(NMS_brach$Macrostrat_unit2 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(NMS_brach$Macrostrat_unit2 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(NMS_brach$Macrostrat_unit2 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(NMS_brach$Macrostrat_unit2 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(NMS_brach$Macrostrat_unit2 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(NMS_brach$Macrostrat_unit2 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(NMS_brach$Macrostrat_unit2 == regex("C1", ignore_case = T)))
recent <- unique(recent)
if(length(recent) > 0){
  NMS_brach <- NMS_brach[-recent,]
}

#### Combining datasets ####
## Change NMS_biv formation column name to formation1
colnames(NMS_biv) <- c("accessionNumber", "genus", "species", "locality", "period", "formation1", "phylum")

## Standardise columns
NMS_biv$formation2 <- ""
NMS_biv$Macrostrat_unit1 <- ""
NMS_biv$Macrostrat_unit2 <- ""
NMS_biv$stage_OLD <- ""

## Rearrange
colnames(NMS_biv)
NMS_biv <- NMS_biv[,c(1,7, 2, 3, 5, 9, 10, 11, 6, 8, 4)]

colnames(NMS_brach)
NMS_brach <- NMS_brach[,c(3, 11, 1, 2, 5, 6, 7, 8, 9, 10, 4)]

## Combine
NMS <- rbind(NMS_biv, NMS_brach)

## Get rid of all spaces outside of string
NMS$formation1 <- str_trim(NMS$formation1)
NMS$formation2 <- str_trim(NMS$formation2)

## Finally, combine or stages into a single string
units <- NMS[,c(6:7)]
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
NMS <- NMS[,-c(6:7)]
NMS <- cbind(NMS, chronostratigraphy)

## Re-order
#View(data.frame(colnames(NMS)))
NMS <- NMS[,c(1:5, 10, 6:9)]
#View(data.frame(colnames(NMS)))

## Export
saveRDS(NMS, file = "data/museum/NMS.Rds")
