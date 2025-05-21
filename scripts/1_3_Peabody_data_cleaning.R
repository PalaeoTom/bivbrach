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

#### Bivalves ####
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
noForm <- c(noForm, which(Peabody_biv$formation == "Etage Dd1delta"))
noForm <- c(noForm, which(Peabody_biv$formation == "Etage Dd5"))
noForm <- c(noForm, which(Peabody_biv$formation == "Etage Ee2"))
noForm <- c(noForm, which(Peabody_biv$formation == "Etage F"))
noForm <- c(noForm, which(Peabody_biv$formation == "Etage Ff1"))
noForm <- c(noForm, which(Peabody_biv$formation == "Etage Ff2"))
noForm <- c(noForm, which(Peabody_biv$formation == "Etage Gg1"))
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

#### Splitting stages ####
## Isolate original
orig_biv_stages <- Peabody_biv$age

## Convert unknowns to blanks
## All unknowns already blank!

## Read in stages
stages <- read.csv("data/metadata/macrostrat_raw.csv", row.names = 1, header = T)
stages <- stages[!duplicated(stages[,c("name", "t_age", "b_age")]),]
stage_names <- stages$name

## Now clean - remove accents and swap iens for ians
source("functions/clean.stage.names.R")
Peabody_biv <- clean.stage.names(data = Peabody_biv, columns = "age")

## Now check against Macrostrat names
## Run bulk.update.stages (aggregation of all specific name changes identified thus far), then check for other errors
source("functions/bulk.update.stages.R")
Peabody_biv$age <- bulk.update.stages(Peabody_biv$age)

## Inspect and update function
#View(data.frame(table(Peabody_biv$age)))
#View(data.frame(stage_names))

## Once checked, re-load and re-run
#source("functions/bulk.update.stages.R")
#Peabody_biv$age <- bulk.update.stages(Peabody_biv$age)

## Check how double stages are noted
#View(data.frame(table(Peabody_biv$age)))

## Only dashes used
splitStages <- c(Peabody_biv$age[which(str_detect(Peabody_biv$age, pattern = "-"))])

#View(data.frame(splitStages))
## No need to wittle these down, all need splitting

## get intervals to be split
splitters <- c()
for(i in 1:length(splitStages)){
  splitters <- c(splitters, which(Peabody_biv$age == splitStages[i]))
}

## Isolate stages
newStage <- data.frame(Peabody_biv$age)
colnames(newStage) <- "Macrostrat_unit1"
newStage$Macrostrat_unit2 <- ""

## Specify characters to split by
p <- c('-')

## Split early stages
for(i in splitters){
  for(m in p){
    if(str_detect(Peabody_biv$age[i], pattern = fixed(m))){
      ## extract stages
      ageVec <- unlist(str_split(Peabody_biv$age[i], pattern = fixed(m)))
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

## Delete old stages
Peabody_biv$age <- NULL

## Re-attach to dataset
Peabody_biv <- cbind(Peabody_biv, newStage, "age_OLD" = orig_biv_stages)

#### Splitting formations ####
## Manually inspect formations
#View(data.frame(table(Peabody_biv$formation)))
## No ambersand, _and_, _or_, or | separating formations

## Manually correct problem entries
Peabody_biv[which(Peabody_biv$formation == "Kahleberg-Sandstein"),"formation"] <- "Kahleberg"

## Let's check all punctuation
## Find all punctuation
p <- unique(unlist(str_extract_all(Peabody_biv$formation, pattern = "[[:punct:]]")))

## Check entries associated with each
#View(data.frame(table(Peabody_biv$formation[which(str_detect(Peabody_biv$formation, pattern = fixed(p[4])))])))

## Just split by hyphen. Clean up rest.
p <- p[-3]
for(c in p){
  Peabody_biv$formation <- str_replace_all(Peabody_biv$formation, pattern = fixed(c), replacement = " ")
}

## Find formations to be split
## Now find formations to be split
forms <- unique(Peabody_biv$formation[which(str_detect(Peabody_biv$formation, pattern = "[:punct:]"))])
forms <- c(forms, Peabody_biv$formation[which(str_detect(Peabody_biv$formation, pattern = regex(" and ", ignore_case = T)))])
forms <- c(forms, Peabody_biv$formation[which(str_detect(Peabody_biv$formation, pattern = regex(" or ", ignore_case = T)))])
forms <- c(forms, Peabody_biv$formation[which(str_detect(Peabody_biv$formation, pattern = "[|]"))])
forms <- unique(forms)

## No need to wittle down. Already ready for splitting.

## get ages to be split
splitForms <- c()
for(i in 1:length(forms)){
  splitForms <- c(splitForms, which(Peabody_biv$formation == forms[i]))
}

## Isolate formation
formations <- data.frame(Peabody_biv$formation)
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
Peabody_biv$formation <- NULL

## Re-attach to dataset
Peabody_biv <- cbind(Peabody_biv, formations)

#### Cleaning up taxonomy ####
## Tidy up classes
#View(data.frame(table(Peabody_biv$class)))
#View(Peabody_biv[which(!Peabody_biv$class == "Bivalvia"),])
Peabody_biv$class[which(Peabody_biv$class == "")] <- "Bivalvia"
Peabody_biv <- Peabody_biv[-which(!Peabody_biv$class == "Bivalvia"),]

## Drop unusable localities. First, refresh noLL
noLL <- union(which(Peabody_biv$latitude == ""),which(Peabody_biv$longitude == ""))

## Now find unusable localities. First, remove IDs
Peabody_biv$locality <- str_replace_all(Peabody_biv$locality, pattern = '^[A-Z]{3}\\.[0-9]{5}: ', replacement = "")
Peabody_biv$locality <- str_replace_all(Peabody_biv$locality, pattern = '^[A-Z]{3}\\.[0-9]{5}\\.[A-Z]{1}: ', replacement = "")

## Now extract collections
Peabody_biv$collection <- unlist(str_extract(Peabody_biv$locality, pattern = '(?=Coll).*$'))

## Convert NAs to blanks
Peabody_biv$collection[is.na(Peabody_biv$collection)] <- ""

## Tidy up colons
Peabody_biv$collection <- str_replace(Peabody_biv$collection, pattern = '^(Coll:) ', replacement = "")

## Now drop from locality strings
Peabody_biv$locality <- str_replace_all(Peabody_biv$locality, pattern = '(?=Coll).*$', replacement = "")

## Now to sort through localities
#View(data.frame(table(Peabody_biv$locality)))
noLoc <- c()
noLoc <- c(noLoc, which(Peabody_biv$locality == "Cretaceous. Albian."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "Europe. England or France. Late Cretaceous. Chalk."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "Europe. Jurassic. Lias alpha. lower lias."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "France. Chateau. Eocene."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "France. Paleogene. Eocene. Lutetian."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "Germany. Late Jurassic. Coral Rag."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "Germany. unknown locality. Late Permian. Zechstein Fm. Lower or Middle Zechstein."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "Japan. Quaternary. Recent."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "Mexico. Quaternary. Recent."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "France. "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Alabama or Mississippi. Cretaceous. Maastrichtian. Selma Grp. Prairie Bluff Chalk."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Alabama. "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. California. Quaternary. Recent."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Cretaceous. Colorado Grp. Niobrara Fm."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Devonian. Onondaga Ls."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Florida. "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Florida. 8. "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Cretaceous. Pierre Sh."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Florida. 9?. "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Florida. 9. "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Florida. general locality. Quaternary. Pleistocene. Caloosahatchee Fm."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Florida. Locality 2. "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Florida. Locality 4. "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Florida. Locality 7. "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Kansas or Missouri. Late Pennsylvanian. Missourian. Kansas City Grp."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Kansas. "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Kentucky. Ordovician. Trentonian."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Late Cretaceous. Cenomanian-Turonian. Colorado Grp. Greenhorn Ls."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Late Cretaceous. Niobrara Chalk."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Late Cretaceous. Pierre Sh."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Maryland. Neogene. Late Miocene. Chesapeake Grp. St Marys Fm."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Mississippi. "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. New York. "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. New York. Devonian. Hamilton Grp."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. North Carolina.  [BB-1]  "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. North or South Dakota; general loc. no. for accn. no. 7048. Late Cretaceous. Fox Hills Fm."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Ohio?, West Virginia?, western Pennsylvania?.  [C66]  Late Pennsylvanian. Conemaugh Fm. Brush Creek Sh Mbr."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Ohio. Late Ordovician. Maysvillian. Cincinnati Grp."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. South Dakota.  [100]  "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. South Dakota.  [23-60]  "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. South Dakota. Cretaceous. Fox Hills Fm."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. South Dakota. General locality. "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Southeastern to Gulf Coast. Paleogene. Early Oligocene. Rupelian. Vicksburg Grp."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Southeastern USA. Late Cretaceous. Ripley Fm."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. states along N shore of Gulf of Mexico. Late Cretaceous. Ripley Fm."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Texas. "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Texas.  [457, 15B3]  "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Texas. Cretaceous?. Laramie Fm?."))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Texas. western "))
noLoc <- c(noLoc, which(Peabody_biv$locality == "USA. Wyoming ?. Jurassic. Sundance Fm."))
noLoc <- unique(noLoc)
droppers <- intersect(noLL, noLoc)
Peabody_biv <- Peabody_biv[-droppers,]

## Clean out punctuation
Peabody_biv$formation1 <- str_replace_all(Peabody_biv$formation1, pattern = "[:punct:]", replacement = "")
Peabody_biv$formation2 <- str_replace_all(Peabody_biv$formation2, pattern = "[:punct:]", replacement = "")

## Tidy up genus, stage, and formation capitalization
Peabody_biv$formation1 <- str_to_title(Peabody_biv$formation1)
Peabody_biv$formation2 <- str_to_title(Peabody_biv$formation2)

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

## Finally, drop recent taxa
recent <- c()
recent <- c(recent, which(Peabody_biv$Macrostrat_unit1 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(Peabody_biv$Macrostrat_unit1 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(Peabody_biv$Macrostrat_unit1 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(Peabody_biv$Macrostrat_unit1 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(Peabody_biv$Macrostrat_unit1 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(Peabody_biv$Macrostrat_unit1 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(Peabody_biv$Macrostrat_unit1 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(Peabody_biv$Macrostrat_unit1 == regex("C1", ignore_case = T)))

recent <- c(recent, which(Peabody_biv$Macrostrat_unit2 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(Peabody_biv$Macrostrat_unit2 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(Peabody_biv$Macrostrat_unit2 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(Peabody_biv$Macrostrat_unit2 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(Peabody_biv$Macrostrat_unit2 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(Peabody_biv$Macrostrat_unit2 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(Peabody_biv$Macrostrat_unit2 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(Peabody_biv$Macrostrat_unit2 == regex("C1", ignore_case = T)))
recent <- unique(recent)
if(length(recent) > 0){
  Peabody_biv <- Peabody_biv[-recent,]
}

#### Brachiopods ####
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

#View(data.frame(table(Peabody_brach$formation)))
## Some punctuation
noForm <- c()
noForm <- c(noForm, which(Peabody_brach$formation == ""))
noForm <- c(noForm, which(Peabody_brach$formation == "Etage Dd3"))
noForm <- c(noForm, which(Peabody_brach$formation == "Etage Ee2"))
noForm <- c(noForm, which(Peabody_brach$formation == "Etage F"))
noForm <- c(noForm, which(Peabody_brach$formation == "Etage Ff1"))
noForm <- c(noForm, which(Peabody_brach$formation == "Etage Ff2"))
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

#### Splitting stages ####
## Isolate original
orig_brach_stages <- Peabody_brach$age

## Convert unknowns into blanks - all are already blank
#View(data.frame(table(Peabody_brach$age)))

## Read in stages
stages <- read.csv("data/metadata/macrostrat_raw.csv", row.names = 1, header = T)
stages <- stages[!duplicated(stages[,c("name", "t_age", "b_age")]),]
stage_names <- stages$name

## Now clean - remove accents and swap iens for ians
source("functions/clean.stage.names.R")
Peabody_brach <- clean.stage.names(data = Peabody_brach, columns = "age")

## Now check against Macrostrat names
## Run bulk.update.stages (aggregation of all specific name changes identified thus far), then check for other errors
source("functions/bulk.update.stages.R")
Peabody_brach$age <- bulk.update.stages(Peabody_brach$age)

## Inspect and update function
#View(data.frame(table(Peabody_brach$age)))
#View(data.frame(stage_names))

## Once function updates are complete, re-load and re-run
#source("functions/bulk.update.stages.R")
#Peabody_brach$age <- bulk.update.stages(Peabody_brach$age)

## Hyphens + one _to_
splitStages <- c()
splitStages <- c(splitStages, unique(Peabody_brach$age[which(str_detect(Peabody_brach$age, pattern = "[:punct:]"))]))
splitStages <- c(splitStages, Peabody_brach$age[which(str_detect(Peabody_brach$age, pattern = regex(" to ", ignore_case = T)))])
splitStages <- unique(splitStages)

## Wittle down to those that should be split
#View(data.frame(splitStages))
## Split all!

## get intervals to be split
splitters <- c()
for(i in 1:length(splitStages)){
  splitters <- c(splitters, which(Peabody_brach$age == splitStages[i]))
}

## Isolate stages
newStage <- data.frame(Peabody_brach$age)
colnames(newStage) <- "Macrostrat_unit1"
newStage$Macrostrat_unit2 <- ""

## Specify characters to split by
p <- c('-', ' to ')

## Split early stages
for(i in splitters){
  for(m in p){
    if(str_detect(Peabody_brach$age[i], pattern = fixed(m))){
      ## extract stages
      ageVec <- unlist(str_split(Peabody_brach$age[i], pattern = fixed(m)))
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

## Delete old stages
Peabody_brach$age <- NULL

## Re-attach to dataset
Peabody_brach <- cbind(Peabody_brach, newStage, "age_OLD" = orig_brach_stages)

#### Splitting formations ####
## Manually inspect formations
#View(data.frame(table(Peabody_brach$formation)))

## Correct one formation that needs splitting
Peabody_brach[which(Peabody_brach$formation == "Upper and Lower Fezouata formations, undifferentiated"),"formation"] <- "Upper Fezouata and Lower Fezouata"

## Initial tidy of punctuation
p <- unique(unlist(str_extract_all(Peabody_brach$formation, pattern = "[[:punct:]]")))

## Check each
#View(data.frame(table(Peabody_brach$formation[which(str_detect(Peabody_brach$formation, pattern = fixed(p[1])))])))

## No need to split by punctuation. Clean up.
for(c in p){
  Peabody_brach$formation <- str_replace_all(Peabody_brach$formation, pattern = fixed(c), replacement = " ")
}

## Now find formations to be split
forms <- unique(Peabody_brach$formation[which(str_detect(Peabody_brach$formation, pattern = "[:punct:]"))])
forms <- c(forms, Peabody_brach$formation[which(str_detect(Peabody_brach$formation, pattern = regex(" and ", ignore_case = T)))])
forms <- c(forms, Peabody_brach$formation[which(str_detect(Peabody_brach$formation, pattern = " & "))])
forms <- c(forms, Peabody_brach$formation[which(str_detect(Peabody_brach$formation, pattern = regex(" or ", ignore_case = T)))])
forms <- c(forms, Peabody_brach$formation[which(str_detect(Peabody_brach$formation, pattern = "[|]"))])
forms <- unique(forms)

## No need to wittle down - only one!

## get ages to be split
splitForms <- c()
for(i in 1:length(forms)){
  splitForms <- c(splitForms, which(Peabody_brach$formation == forms[i]))
}

## Isolate formation
formations <- data.frame(Peabody_brach$formation)
colnames(formations) <- "formation1"

## max 2 formations
formations$formation2 <- ""

## Specify string to split by
p <- c(' and ')

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
Peabody_brach$formation <- NULL

## Re-attach to dataset
Peabody_brach <- cbind(Peabody_brach, formations)

#### Cleaning localities and taxonomy ####
## Clean up localities
## First, refresh noLL
noLL <- union(which(Peabody_brach$latitude == ""),which(Peabody_brach$longitude == ""))

## Tidy up localities
## Now find unusable localities. First, remove IDs
Peabody_brach$locality <- str_replace_all(Peabody_brach$locality, pattern = '^[A-Z]{3}\\.[0-9]{5}: ', replacement = "")
Peabody_brach$locality <- str_replace_all(Peabody_brach$locality, pattern = '^[A-Z]{3}\\.[0-9]{5}\\.[A-Z]{1}: ', replacement = "")

## Now extract collections
Peabody_brach$collection <- unlist(str_extract(Peabody_brach$locality, pattern = '(?=Coll).*$'))

## Convert NAs to blanks
Peabody_brach$collection[is.na(Peabody_brach$collection)] <- ""

## Tidy up colons
Peabody_brach$collection <- str_replace(Peabody_brach$collection, pattern = '^(Coll:) ', replacement = "")

## Now drop from locality strings
Peabody_brach$locality <- str_replace_all(Peabody_brach$locality, pattern = '(?=Coll).*$', replacement = "")

## Now get no localities
noLoc <-c()
#View(data.frame(table(Peabody_brach$locality)))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Australia. Quaternary. Recent."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Canada. Manitoba. "))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Canada. Quebec. "))
noLoc <- c(noLoc, which(Peabody_brach$locality == 'Earth. "Mid Continental Area". Virgilian.'))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Earth. Late Ordovician. Maysvillian."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Earth. Mississippian. Chesterian."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Earth. Late Ordovician. Richmondian."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Europe + America. Ordovician. Richmondian."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Europe. "))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Italy. Quaternary. Recent."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Japan. Quaternary. Recent."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Middle Jurassic. Cornbrash."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "North America. "))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Norway. Quaternary. Recent."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Oceania. Australia and New Zealand. Quaternary. Recent."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Pacific Ocean. Korea. Pacific Ocean. north Pacific temperate region. Quaternary. Recent."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Pacific Ocean. Quaternary. Recent."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "Planet Earth. Quaternary. Recent."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "United Kingdom. England. "))
noLoc <- c(noLoc, which(Peabody_brach$locality == "United Kingdom. Scotland. Quaternary. Recent."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. "))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Devonian. Onondaga Ls."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Devonian. Oriskany Ss."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Early Permian. Council Grove Grp. Foraker Ls. Hughes Creek Sh Mbr."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Eastern USA. Ordovician. Trenton Ls."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Kentucky. Kentucky?. "))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Late Cretaceous. Pierre Sh."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Late Pennsylvanian. Missourian. Kansas City Grp. Iola Ls."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. locality unknown. "))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. locality unknown. Early Mississippian. Burlington Ls."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Maine. Quaternary. Recent."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Maryland. Devonian. Oriskany Fm."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Maryland. Western Maryland. "))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Missouri or Iowa. Early Mississippian. Kinderhookian."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Missouri.  [2]  "))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. New Jersey. "))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. New York. "))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Ordovician. Trenton Ls."))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Tennessee. "))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Texas. "))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Texas.  [3892/x]  "))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Texas.  [Sd.]  "))
noLoc <- c(noLoc, which(Peabody_brach$locality == "USA. Texas. Section 8, bed a''.  [37]  "))
noLoc <- unique(noLoc)
droppers <- intersect(noLL, noLoc)
Peabody_brach <- Peabody_brach[-droppers, ]

## Remove punctuation from  formations
Peabody_brach$formation1 <- str_replace_all(Peabody_brach$formation1, pattern = "[:punct:]", replacement = "")
Peabody_brach$formation2 <- str_replace_all(Peabody_brach$formation2, pattern = "[:punct:]", replacement = "")

## Tidy up formation capitalization
Peabody_brach$formation1 <- str_to_title(Peabody_brach$formation1)
Peabody_brach$formation2 <- str_to_title(Peabody_brach$formation2)

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

## Finally, drop recent entries
recent <- c()
recent <- c(recent, which(Peabody_brach$Macrostrat_unit1 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(Peabody_brach$Macrostrat_unit1 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(Peabody_brach$Macrostrat_unit1 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(Peabody_brach$Macrostrat_unit1 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(Peabody_brach$Macrostrat_unit1 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(Peabody_brach$Macrostrat_unit1 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(Peabody_brach$Macrostrat_unit1 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(Peabody_brach$Macrostrat_unit1 == regex("C1", ignore_case = T)))

recent <- c(recent, which(Peabody_brach$Macrostrat_unit2 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(Peabody_brach$Macrostrat_unit2 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(Peabody_brach$Macrostrat_unit2 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(Peabody_brach$Macrostrat_unit2 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(Peabody_brach$Macrostrat_unit2 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(Peabody_brach$Macrostrat_unit2 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(Peabody_brach$Macrostrat_unit2 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(Peabody_brach$Macrostrat_unit2 == regex("C1", ignore_case = T)))
recent <- unique(recent)
if(length(recent) > 0){
  Peabody_brach <- Peabody_brach[-recent,]
}

#### Preparing final version for export
## Combine
Peabody <- rbind(Peabody_biv, Peabody_brach)

## Rearrange columns
colnames(Peabody)
Peabody <- Peabody[,c(1, 11, 12, 13, 14, 15, 16, 19, 6, 4, 20, 21, 22, 23, 24, 5, 8, 9, 10, 7, 3, 17, 25, 18)]

## Get rid of all spaces outside of string
Peabody$formation1 <- str_trim(Peabody$formation1)
Peabody$formation2 <- str_trim(Peabody$formation2)

## Finally, combine or stages into a single string
units <- Peabody[,c(11:12)]
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
Peabody <- Peabody[,-c(11:12)]
Peabody <- cbind(Peabody, chronostratigraphy)

## Re-order
#View(data.frame(colnames(Peabody)))
Peabody <- Peabody[,c(1:10, 23, 11:22)]
#View(data.frame(colnames(Peabody)))

## Export
saveRDS(Peabody, file = "data/museum/Peabody.Rds")
