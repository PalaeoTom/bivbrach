## 1. Initial data processing - FMNH data cleaning
## TJS, 17/01/2025

## If packages aren't installed, install them, then load them
packages <- c("stringr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(stringr)

#### Peabody data ####
rm(list = ls())

## Set working directory
setwd("~/R_packages/bivbrach")
home <- getwd()

## Load data
setwd("~/Library/CloudStorage/OneDrive-Nexus365/Bivalve_brachiopod/data")
Peabody_biv <- read.csv("Peabody_Bivalves_May2024.csv")
Peabody_brach <- read.csv("Peabody_Brachiopods_May2024.csv")
setwd(home)

## Inspect data
#View(Peabody_biv)

## Prune to relevant columns
Peabody_biv <- Peabody_biv[,-c(1,2,4,5,7,8,13,17,19,20,21,22,25,33,34,35,36,38,39)]
Peabody_brach <- Peabody_brach[,-c(1,2,4,5,7,8,13,17,19,20,21,22,25,33,34,35,36,38,39)]

## Update column names
colnames(Peabody_biv) <- colnames(Peabody_brach) <- c("catFullNumber", "fullName", "locality", "epoch", "formation", "member", "period", "districtCountyShire", "age", "facies",
                                                      "latitude", "longitude", "phylum", "subPhylum", "class", "order", "family", "genus", "reference", "collectorSurname")

## Peabody-specific tasks
## 1. Split fullName into genera and species
species <- str_split(string = Peabody_biv[,"fullName"], n = 2, pattern = " ")
species <- data.frame(sapply(1:length(species), function(x) species[[x]][2]))
colnames(species) <- "species"
species$species[which(is.na(species))] <- ""
Peabody_biv <- cbind(Peabody_biv, species)

species <- str_split(string = Peabody_brach[,"fullName"], n = 2, pattern = " ")
species <- data.frame(sapply(1:length(species), function(x) species[[x]][2]))
colnames(species) <- "species"
species$species[which(is.na(species))] <- ""
Peabody_brach <- cbind(Peabody_brach, species)

## 2. Isolate formations in locality
## Start with bivalves
fms <- str_extract(Peabody_biv$locality, regex("(\\w+ fm)", ignore_case = T))
formations <- str_extract(Peabody_biv$locality, regex("(\\w+ formation)", ignore_case = T))
fms[is.na(fms)] <- ""
formations[is.na(formations)] <- ""

## If gap in formation, add
forms <- paste(fms, formations, sep = " ")
forms[which(forms == " ")] <- ""
tbAdded <- intersect(which(Peabody_biv$formation == ""), which(!forms == ""))
Peabody_biv$formation[tbAdded] <- forms[tbAdded]

## Now to do for brachiopods
fms <- str_extract(Peabody_brach$locality, regex("(\\w+ fm)", ignore_case = T))
formations <- str_extract(Peabody_brach$locality, regex("(\\w+ formation)", ignore_case = T))
fms[is.na(fms)] <- ""
formations[is.na(formations)] <- ""

## If gap in formation, add
forms <- paste(fms, formations, sep = " ")
forms[which(forms == " ")] <- ""
tbAdded <- intersect(which(Peabody_brach$formation == ""), which(!forms == ""))
Peabody_brach$formation[tbAdded] <- forms[tbAdded]

## Now to clean. Start with bivalves
## Drop rows with no genera, phylum/class, formation/age, or latitude+longtitude/locality
#View(data.frame(table(Peabody_biv$genus)))
droppers <- c()
droppers <- c(droppers, which(Peabody_biv$genus == ""))
## Check for "aff"
if(any(str_detect(Peabody_biv[,"genus"], pattern = regex("aff", ignore_case = T)))){
  print("'aff's detected")
  affs <- unique(Peabody_biv$genus[which(str_detect(Peabody_biv$genus, pattern = regex("aff", ignore_case = T)))])
  affs
  ## no affs
}
## Check for "cf"
if(any(str_detect(Peabody_biv[,"genus"], pattern = regex("cf", ignore_case = T)))){
  print("'cf's detected")
  cfs <- unique(Peabody_biv$genus[which(str_detect(Peabody_biv$genus, pattern = regex("cf", ignore_case = T)))])
  cfs
  ## no cfs
}
## Check for dots
if(any(str_detect(Peabody_biv[,"genus"], pattern = "\\."))){
  print("periods detected")
  dots <- unique(Peabody_biv$genus[which(str_detect(Peabody_biv$genus, pattern = "\\."))])
  dots
  ## no dots
}
## Check for "?"
if(any(str_detect(Peabody_biv[,"genus"], pattern = "\\?"))){
  print("'?'s detected")
  questions <- unique(Peabody_biv$genus[which(str_detect(Peabody_biv$genus, pattern = "\\?"))])
  questions
  # no questions
}
## Drop non-molluscs
droppers <- c(droppers, which(!Peabody_biv$phylum == "Mollusca"))
## Get unique rows and drop
droppers <- unique(droppers)
Peabody_biv <- Peabody_biv[-droppers,]
## Now to check stages and formations - drop if neither
droppers <- c()
#View(data.frame(table(Peabody_biv$age)))
## No punctuation!
noAge <- c()
noAge <- c(noAge, which(Peabody_biv$age == ""))
#View(data.frame(table(Peabody_biv$formation)))
## Some punctuation
noForm <- c()
noForm <- c(noForm, which(Peabody_biv$formation == ""))
noForm <- c(noForm, which(Peabody_biv$formation == " recognized formation"))
droppers <- c(droppers, intersect(noForm, noAge))
## neither both lat+long nor locality
## define noLat/long and noLoc, then get intersection and drop
noLL <- union(which(Peabody_biv$latitude == ""),which(Peabody_biv$longitude == ""))
#View(data.frame(table(Peabody_biv$locality)))
noLoc <- which(Peabody_biv$locality == "")
droppers <- c(droppers, intersect(noLL, noLoc))
## After this, get unique droppers and drop
droppers <- unique(droppers)
Peabody_biv <- Peabody_biv[-droppers,]

## Split and clean stages
## Ages to be split
splitAge <- c()
splitAge <- c(splitAge, which(Peabody_biv$age == "Cenomanian-Turonian"))

## Isolate stages
stages <- data.frame(Peabody_biv$age)
Peabody_biv$age <- NULL
colnames(stages) <- "stage1"

## Only 2 stages max
stages$stage2 <- ""

## Specify splitting punctuation
p <- c("-")

## Split stages
for(i in splitAge){
  for(m in p){
    if(str_detect(stages$stage1[i], pattern = fixed(m))){
      ## extract stages
      formVec <- unlist(str_split(stages$stage1[i], pattern = fixed(m)))
      ## assign to new columns
      for(f in 1:length(formVec)){
        stages[i,f] <- formVec[f]
      }
    }
  }
}

## Re-attach to dataset
Peabody_biv <- cbind(Peabody_biv, stages)

## Now do the same for formations
formations <- data.frame(Peabody_biv$formation)
Peabody_biv$formation <- NULL
colnames(formations) <- "formation1"

## Get maximum number of formations within a single record
## Find all punctuation
p <- unique(unlist(str_extract_all(formations$formation1, pattern = "[[:punct:]]")))
p

## 9 total
## Check entries associated with each
#View(data.frame(table(formations$formation1[which(str_detect(formations$formation1, pattern = fixed('-')))])))

## Manually correct one formation
formations$formation1[which(formations$formation1 == "Kahleberg-Sandstein")] <- "Kahleberg"
#View(data.frame(table(formations$formation1[which(str_detect(formations$formation1, pattern = fixed('-')))])))

## Split by hyphen alone
p <- paste0('\\', p[-c(3)])
p
for(c in p){
  formations$formation1 <- str_replace_all(formations$formation1, pattern = c, replacement = " ")
}
## get split formations - maximum of 2.
split.forms <- c(str_split(formations$formation1, pattern = fixed("-")))
max.forms <- max(sapply(1:length(split.forms), function(x) length(split.forms[[x]])))

## maximum 2 formations
formations$formation2 <- ""

## Now to find offending rows
splitters <- c()
splitters <- c(splitters, which(str_detect(formations$formation1, pattern = fixed('-'))))

## Specify splitting punctuation
p <- c("-")

## Split formations
for(i in splitters){
  for(m in p){
    if(str_detect(formations$formation1[i], pattern = fixed(m))){
      ## extract formation
      formVec <- unlist(str_split(formations$formation1[i], pattern = fixed(m)))
      ## assign to new columns
      for(f in 1:length(formVec)){
        formations[i,f] <- formVec[f]
      }
    }
  }
}

## Re-attach to dataset
Peabody_biv <- cbind(Peabody_biv, formations)

## Tidy up classes
#View(data.frame(table(Peabody_biv$class)))
#View(Peabody_biv[which(!Peabody_biv$class == "Bivalvia"),])
Peabody_biv$class[which(Peabody_biv$class == "")] <- "Bivalvia"
Peabody_biv <- Peabody_biv[-which(!Peabody_biv$class == "Bivalvia"),]

## Tidy up genus, stage, and formation capitalization
Peabody_biv$genus <- str_to_title(Peabody_biv$genus)
Peabody_biv$formation1 <- str_to_title(Peabody_biv$formation1)
Peabody_biv$formation2 <- str_to_title(Peabody_biv$formation2)
Peabody_biv$stage1 <- str_to_title(Peabody_biv$stage1)
Peabody_biv$stage2 <- str_to_title(Peabody_biv$stage2)

## Then convert all uncertain species to "sp."
## Tidy up undetermined/indeterminate species
#View(data.frame(table(Peabody_biv$species)))
gen.level <- c()
gen.level <- c(gen.level,which(Peabody_biv[,"species"] == ""))
## Check for "aff"
if(any(str_detect(Peabody_biv[,"species"], pattern = regex("aff", ignore_case = T)))){
  print("'aff's detected")
  affs <- unique(Peabody_biv[,"species"][which(str_detect(Peabody_biv[,"species"], pattern = regex("aff", ignore_case = T)))])
  affs
  ##
  affs <- affs[-c(1, 2)]
  affs
  gen.level <- c(gen.level, which(Peabody_biv[,"species"] %in% affs))
}
## Check for "cf"
if(any(str_detect(Peabody_biv[,"species"], pattern = regex("cf", ignore_case = T)))){
  print("'cf's detected")
  cfs <- unique(Peabody_biv$species[which(str_detect(Peabody_biv$species, pattern = regex("cf", ignore_case = T)))])
  cfs
  gen.level <- c(gen.level, which(Peabody_biv[,"species"] %in% cfs))
}
## Check for dots
if(any(str_detect(Peabody_biv[,"species"], pattern = "\\."))){
  print("periods detected")
  dots <- unique(Peabody_biv$species[which(str_detect(Peabody_biv$species, pattern = "\\."))])
  #View(data.frame(dots))
  ## prune out dots to retain
  dots <- dots[-c(8, 19)]
  #View(data.frame(dots))
  gen.level <- c(gen.level,which(Peabody_biv[,"species"] %in% dots))
}
## Check for "?"
if(any(str_detect(Peabody_biv[,"species"], pattern = "\\?"))){
  print("'?'s detected")
  questions <- unique(Peabody_biv$species[which(str_detect(Peabody_biv$species, pattern = "\\?"))])
  #View(table(questions))
  ## drop total unknowns, clean up others
  gen.level <- c(gen.level,which(Peabody_biv[,"species"] %in% questions))
}
gen.level <- c(gen.level,which(Peabody_biv[,"species"] == "(Arcopagia)"))
gen.level <- c(gen.level,which(Peabody_biv[,"species"] == "(Caritodens)"))
gen.level <- c(gen.level,which(Peabody_biv[,"species"] == "(Cosmetodon)"))
gen.level <- c(gen.level,which(Peabody_biv[,"species"] == "(Goniophora)"))
gen.level <- c(gen.level,which(Peabody_biv[,"species"] == "(Hypoxytoma)"))
gen.level <- c(gen.level,which(Peabody_biv[,"species"] == "(Pseudoptera)"))
gen.level <- c(gen.level,which(Peabody_biv[,"species"] == "(Scapharca)"))
gen.level <- c(gen.level,which(Peabody_biv[,"species"] == "(Tellinides)"))
gen.level <- unique(gen.level)
if(any(which(str_detect(Peabody_biv$species, pattern = "[[:punct:]]")))){
  Peabody_biv$species <- str_replace_all(Peabody_biv$species, pattern = "[[:punct:]]", replacement = "")
}
Peabody_biv[gen.level,"species"] <- "sp."

## Then fix capitalization for species
Peabody_biv$species <- tolower(Peabody_biv$species)

## Now to clean up brachiopods
## Drop rows with no genera, phylum/class, formation/age, or latitude+longtitude/locality
#View(data.frame(table(Peabody_brach$genus)))
droppers <- c()
droppers <- c(droppers, which(Peabody_brach$genus == ""))
## Check for "aff"
if(any(str_detect(Peabody_brach[,"genus"], pattern = regex("aff", ignore_case = T)))){
  print("'aff's detected")
  affs <- unique(Peabody_brach$genus[which(str_detect(Peabody_brach$genus, pattern = regex("aff", ignore_case = T)))])
  affs
  ## no affs
}
## Check for "cf"
if(any(str_detect(Peabody_brach[,"genus"], pattern = regex("cf", ignore_case = T)))){
  print("'cf's detected")
  cfs <- unique(Peabody_brach$genus[which(str_detect(Peabody_brach$genus, pattern = regex("cf", ignore_case = T)))])
  cfs
  ## no cfs
}
## Check for dots
if(any(str_detect(Peabody_brach[,"genus"], pattern = "\\."))){
  print("periods detected")
  dots <- unique(Peabody_brach$genus[which(str_detect(Peabody_brach$genus, pattern = "\\."))])
  dots
  ## no dots
}
## Check for "?"
if(any(str_detect(Peabody_brach[,"genus"], pattern = "\\?"))){
  print("'?'s detected")
  questions <- unique(Peabody_brach$genus[which(str_detect(Peabody_brach$genus, pattern = "\\?"))])
  questions
  # no questions
}
## Tidy up phyla and classes
#View(data.frame(table(Peabody_brach$phylum)))
#View(Peabody_brach[which(!Peabody_brach$phylum == "Brachiopoda"),])
## Drop all entries without Brachiopoda as phyla
droppers <- c(droppers, which(!Peabody_brach$phylum == "Brachiopoda"))
#View(data.frame(table(Peabody_brach$class)))
droppers <- c(droppers, which(Peabody_brach$class == "Anthozoa"))
droppers <- c(droppers, which(Peabody_brach$class == "Bivalvia"))
droppers <- c(droppers, which(Peabody_brach$class == "Echinoidea"))
droppers <- c(droppers, which(Peabody_brach$class == "Gastropoda"))
droppers <- c(droppers, which(Peabody_brach$class == "Gymnolaemata"))
## Get unique rows and drop
droppers <- unique(droppers)
Peabody_brach <- Peabody_brach[-droppers,]
## Now to check stages and formations - drop if neither
droppers <- c()
#View(data.frame(table(Peabody_brach$age)))
## No punctuation!
noAge <- c()
noAge <- c(noAge, which(Peabody_brach$age == ""))
Peabody_brach$age[which(Peabody_brach$age == "Recent")] <- "Holocene"

#View(data.frame(table(Peabody_brach$formation)))
## Some punctuation
noForm <- c()
noForm <- c(noForm, which(Peabody_brach$formation == ""))
noForm <- c(noForm, which(Peabody_brach$formation == "?Echinosphaerites Ls (c)"))
noForm <- c(noForm, which(Peabody_brach$formation == "238 fm "))
noForm <- c(noForm, which(Peabody_brach$formation == "Chalk"))
droppers <- c(droppers, intersect(noForm, noAge))

## neither both lat+long nor locality
## define noLat/long and noLoc, then get intersection and drop
noLL <- union(which(Peabody_brach$latitude == ""),which(Peabody_brach$longitude == ""))
#View(data.frame(table(Peabody_brach$locality)))
noLoc <- which(Peabody_brach$locality == "")
droppers <- c(droppers, intersect(noLL, noLoc))
## After this, get unique droppers and drop
droppers <- unique(droppers)
Peabody_brach <- Peabody_brach[-droppers,]

## Split and clean stages
## Ages to be split
splitAge <- c()
splitAge <- c(splitAge, which(Peabody_brach$age == "Missourian to Virgilian"))

## Isolate stages
stages <- data.frame(Peabody_brach$age)
Peabody_brach$age <- NULL
colnames(stages) <- "stage1"

## Only 2 stages max
stages$stage2 <- ""

## Specify string to split by
p <- c(" to ")

## Split stages
for(i in splitAge){
  for(m in p){
    if(str_detect(stages$stage1[i], pattern = fixed(m))){
      ## extract stages
      formVec <- unlist(str_split(stages$stage1[i], pattern = fixed(m)))
      ## assign to new columns
      for(f in 1:length(formVec)){
        stages[i,f] <- formVec[f]
      }
    }
  }
}

## Re-attach to dataset
Peabody_brach <- cbind(Peabody_brach, stages)

## Formations to be split
splitForm <- c()
splitForm <- c(splitForm, which(Peabody_brach$formation == "Upper and Lower Fezouata formations, undifferentiated"))

## Now do the same for formations
formations <- data.frame(Peabody_brach$formation)
Peabody_brach$formation <- NULL
colnames(formations) <- "formation1"

## Get maximum number of formations within a single record
## Find all punctuation
p <- unique(unlist(str_extract_all(formations$formation1, pattern = "[[:punct:]]")))
p

## Split by " and " alone
p <- paste0('\\', p)
p
for(c in p){
  formations$formation1 <- str_replace_all(formations$formation1, pattern = c, replacement = " ")
}
## maximum 2 formations
formations$formation2 <- ""

## Specify splitting string
p <- c(" and ")

## Split formations
for(i in splitForm){
  for(m in p){
    if(str_detect(formations$formation1[i], pattern = fixed(m))){
      ## extract formation
      formVec <- unlist(str_split(formations$formation1[i], pattern = fixed(m)))
      ## assign to new columns
      for(f in 1:length(formVec)){
        formations[i,f] <- formVec[f]
      }
    }
  }
}

## Re-attach to dataset
Peabody_brach <- cbind(Peabody_brach, formations)

## Tidy up genus, stage, and formation capitalization
Peabody_brach$genus <- str_to_title(Peabody_brach$genus)
Peabody_brach$formation1 <- str_to_title(Peabody_brach$formation1)
Peabody_brach$formation2 <- str_to_title(Peabody_brach$formation2)
Peabody_brach$stage1 <- str_to_title(Peabody_brach$stage1)
Peabody_brach$stage2 <- str_to_title(Peabody_brach$stage2)

## Then convert all uncertain species to "sp."
## Tidy up undetermined/indeterminate species
#View(data.frame(table(Peabody_brach$species)))
gen.level <- c()
gen.level <- c(gen.level,which(Peabody_brach[,"species"] == ""))
## Check for "aff"
if(any(str_detect(Peabody_brach[,"species"], pattern = regex("aff", ignore_case = T)))){
  print("'aff's detected")
  affs <- unique(Peabody_brach[,"species"][which(str_detect(Peabody_brach[,"species"], pattern = regex("aff", ignore_case = T)))])
  affs
  ##
  affs <- affs[-c(1, 3, 4, 5, 6, 12)]
  affs
  gen.level <- c(gen.level, which(Peabody_brach[,"species"] %in% affs))
}
## Check for "cf"
if(any(str_detect(Peabody_brach[,"species"], pattern = regex("cf", ignore_case = T)))){
  print("'cf's detected")
  cfs <- unique(Peabody_brach$species[which(str_detect(Peabody_brach$species, pattern = regex("cf", ignore_case = T)))])
  cfs
  gen.level <- c(gen.level, which(Peabody_brach[,"species"] %in% cfs))
}
## Check for dots
if(any(str_detect(Peabody_brach[,"species"], pattern = "\\."))){
  print("periods detected")
  dots <- unique(Peabody_brach$species[which(str_detect(Peabody_brach$species, pattern = "\\."))])
  #View(data.frame(dots))
  ## prune out dots to retain
  dots <- dots[-c(17, 18, 24, 50, 53, 60, 61, 77, 78, 79, 80, 87, 88, 89)]
  #View(data.frame(dots))
  gen.level <- c(gen.level,which(Peabody_brach[,"species"] %in% dots))
}
## Check for "?"
if(any(str_detect(Peabody_brach[,"species"], pattern = "\\?"))){
  print("'?'s detected")
  questions <- unique(Peabody_brach$species[which(str_detect(Peabody_brach$species, pattern = "\\?"))])
  #View(table(questions))
  ## drop total unknowns, clean up others
  gen.level <- c(gen.level,which(Peabody_brach[,"species"] %in% questions))
}
gen.level <- c(gen.level,which(Peabody_brach[,"species"] == "(Desquamatia)"))
gen.level <- c(gen.level,which(Peabody_brach[,"species"] == "(Independatrypa)"))
gen.level <- c(gen.level,which(Peabody_brach[,"species"] == "(Planatrypa)"))
gen.level <- c(gen.level,which(Peabody_brach[,"species"] == "(Seratrypa)"))
gen.level <- c(gen.level,which(Peabody_brach[,"species"] == "cor"))
gen.level <- unique(gen.level)
#### Resume from here - start with species names beginning with m
if(any(which(str_detect(Peabody_brach$species, pattern = "[[:punct:]]")))){
  Peabody_brach$species <- str_replace_all(Peabody_brach$species, pattern = "[[:punct:]]", replacement = "")
}
Peabody_brach[gen.level,"species"] <- "sp."

## Then fix capitalization for species
Peabody_brach$species <- tolower(Peabody_brach$species)

## Export
saveRDS(Peabody_biv, file = "data/museum/Peabody_biv.Rds")
saveRDS(Peabody_brach, file = "data/museum/Peabody_brach.Rds")



