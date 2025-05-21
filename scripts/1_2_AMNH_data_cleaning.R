## 1. Initial data processing - FMNH data cleaning
## TJS, 17/01/2025

## If packages aren't installed, install them, then load them
packages <- c("stringr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(stringr)

#### AMNH data ####
rm(list = ls())

## Set working directory
setwd("~/R_packages/bivbrach")
home <- getwd()

## Load data
setwd("~/Library/CloudStorage/OneDrive-Nexus365/Bivalve_brachiopod/data")
AMNH <- read.csv("AMNH_May_2024.csv")
setwd(home)

## Update colnames
colnames(AMNH) <- c("IRN", "catN", "suffix", "phylum", "class", "order", "family", "genus", "subgenus", "species", "subspecies",
                    "recordType", "refPublicationDate", "refTitle", "bibFigurePagination", "bibPlatePagination", "system", "group",
                    "era", "eon", "siteNumber", "country", "state", "county", "township", "preciseLocation", "locality", "period",
                    "epoch", "age", "formation", "member", "zone", "bed", "collectorString", "collectorDate", "originalCount", "latitude",
                    "latitudeDecimal", "latitudeVerbatim", "longitude", "longitudeDecimal", "longitudeVerbatim")

## Prune to relevant columns
AMNH <- AMNH[,c(1:11, 14, 17:20, 22:32, 39, 42)]

## Drop rows with no genera, phylum/class, formation/age, or latitude+longtitude/locality
#View(data.frame(table(AMNH$genus)))
droppers <- c()
droppers <- c(droppers, which(AMNH$genus == ""))
## Check for "aff"
if(any(str_detect(AMNH[,"genus"], pattern = regex("aff", ignore_case = T)))){
  print("'aff's detected")
}
## Check for "cf"
if(any(str_detect(AMNH[,"genus"], pattern = regex("cf", ignore_case = T)))){
  print("'cf's detected")
  cfs <- unique(AMNH$genus[which(str_detect(AMNH$genus, pattern = regex("cf", ignore_case = T)))])
  cfs
  droppers <- c(droppers, which(AMNH[,"genus"] %in% cfs))
}
## Check for dots
if(any(str_detect(AMNH[,"genus"], pattern = "\\."))){
  print("periods detected")
  dots <- unique(AMNH$genus[which(str_detect(AMNH$genus, pattern = "\\."))])
  dots
  ## prune out most dots
  dots <- dots[-c(14)]
  dots
  droppers <- c(droppers,which(AMNH[,"genus"] %in% dots))
}
## Check for "?"
if(any(str_detect(AMNH[,"genus"], pattern = "\\?"))){
  print("'?'s detected")
  questions <- unique(AMNH$genus[which(str_detect(AMNH$genus, pattern = "\\?"))])
  #View(table(data.frame(questions)))
  droppers <- c(droppers,which(AMNH[,"genus"] %in% questions))
}
droppers <- c(droppers, which(AMNH$genus == "Bivalvia | cephalopoda"))
droppers <- c(droppers, which(AMNH$genus == "Centrinella-rhipidomella"))
droppers <- c(droppers, which(AMNH$genus == "Deltopecten + etheripectn"))
droppers <- c(droppers, which(AMNH$genus == "Fragum/trigonicardia"))
droppers <- c(droppers, which(AMNH$genus == "Genus"))
droppers <- c(droppers, which(AMNH$genus == "Indet"))
droppers <- c(droppers, which(AMNH$genus == "Indeterminate"))
droppers <- c(droppers, which(AMNH$genus == "Indetermined"))
droppers <- c(droppers, which(AMNH$genus == "Unidentified"))
droppers <- c(droppers, which(AMNH$genus == "Undetermined"))
droppers <- c(droppers, which(AMNH$genus == "L'azaria"))
droppers <- c(droppers, which(AMNH$genus == "Liebea + bakevellia"))
droppers <- c(droppers, which(AMNH$genus == "Mya  or panopea"))
droppers <- c(droppers, which(AMNH$genus == "New genus"))
droppers <- c(droppers, which(AMNH$genus == "Pseudomonotis + libea"))
## neither phylum nor class
droppers <- c(droppers, intersect(which(AMNH$phylum == ""), which(AMNH$class == "")))
## neither formation nor age
## define noStage and noForm, then get intersection
## first, manually move formations to formation column
AMNH$formation[which(AMNH$age == "(Duplin)")] <- "Upper Duplin"
AMNH$age[which(AMNH$age == "(Duplin)")] <- ""
#View(data.frame(table(AMNH$age)))
noStage <- c()
noStage <- c(noStage, which(AMNH$age == ""))
noStage <- c(noStage, which(AMNH$age == "(E)"))
noStage <- c(noStage, which(AMNH$age == "A"))
noStage <- c(noStage, which(AMNH$age == "Late, Or Permian"))
noStage <- c(noStage, which(AMNH$age == "Lower-Middle"))
noStage <- c(noStage, which(AMNH$age == "Lower/Middle"))
noStage <- c(noStage, which(AMNH$age == "P."))
noStage <- c(noStage, which(AMNH$age == "P.P.?"))
noStage <- c(noStage, which(AMNH$age == "Sub"))
noStage <- c(noStage, which(AMNH$age == "Recent"))
noStage <- c(noStage, which(AMNH$age == "Top"))
noStage <- c(noStage, which(AMNH$age == "Upper"))
noStage <- c(noStage, which(AMNH$age == "Upper ?"))
noStage <- c(noStage, which(AMNH$age == "W. Facies"))
noStage <- c(noStage, which(AMNH$age == "West Facies"))
noStage <- c(noStage, which(AMNH$age == "Conglomerate"))
noStage <- c(noStage, which(AMNH$age == "Florida"))
noStage <- c(noStage, which(AMNH$age == "Lower"))
noStage <- c(noStage, which(AMNH$age == "Middle"))
noStage <- c(noStage, which(AMNH$age == "Post"))
noStage <- c(noStage, which(AMNH$age == "San Pablo"))
noStage <- c(noStage, which(AMNH$age == "ARISAIG"))

## Now for noForm
#View(data.frame(table(AMNH$formation)))
noForm <- c()
noForm <- c(noForm, which(AMNH$formation == ""))
noForm <- c(noForm, which(AMNH$formation == "(Overlying Lignite Beds0"))
noForm <- c(noForm, which(AMNH$formation == "50' Above Phosphoria"))
noForm <- c(noForm, which(AMNH$formation == "Above Abeih Beds"))
noForm <- c(noForm, which(AMNH$formation == "Above Abeith Beds"))
noForm <- c(noForm, which(AMNH$formation == "Above Bewerty Beds"))
noForm <- c(noForm, which(AMNH$formation == "Above Bone Spring Ls."))
noForm <- c(noForm, which(AMNH$formation == "Above Middle Of Placid Shale"))
noForm <- c(noForm, which(AMNH$formation == "Albian"))
noForm <- c(noForm, which(AMNH$formation == "Bluff 3"))
noForm <- c(noForm, which(AMNH$formation == "Clay"))
noForm <- c(noForm, which(AMNH$formation == "Jurassic Beds"))
noForm <- c(noForm, which(AMNH$formation == "Jurassic Limestone"))
noForm <- c(noForm, which(AMNH$formation == "Just Below Tully Limestone"))
noForm <- c(noForm, which(AMNH$formation == "Shale Above Hurlet Limestone"))
noForm <- c(noForm, which(AMNH$formation == "Shale Below Lecompton Ls."))
noForm <- c(noForm, which(AMNH$formation == "Shale Below Topeka Limestone"))
noForm <- c(noForm, which(AMNH$formation == "Shale Below Topeka Ls."))
noForm <- c(noForm, which(AMNH$formation == "Shale Just Under Lecompton Ls."))
noForm <- c(noForm, which(AMNH$formation == "Sub."))
noForm <- c(noForm, which(AMNH$formation == "Sub. Moy."))
noForm <- c(noForm, which(AMNH$formation == "Upper"))
noForm <- c(noForm, which(AMNH$formation == "Upper Bed"))
noForm <- c(noForm, which(AMNH$formation == "Upper Beds"))
noForm <- c(noForm, which(AMNH$formation == "Upper Chalk"))
noForm <- c(noForm, which(AMNH$formation == "Word 1 ?, Kaibab"))
noForm <- c(noForm, which(AMNH$formation == "X Shale"))
droppers <- c(droppers, intersect(noForm, noStage))
## neither both lat+long nor locality
## define noLat/long and noLoc, then get intersection
noLL <- union(which(is.na(AMNH$latitudeDecimal)),which(is.na(AMNH$longitudeDecimal)))
noLoc <- which(AMNH$locality == "")
droppers <- c(droppers, intersect(noLL, noLoc))
## After this, get unique droppers and drop
droppers <- unique(droppers)
AMNH <- AMNH[-droppers,]

#### Cleaning up stages ####
## Isolate original
orig_stages <- AMNH$age

## Convert unknowns to blanks
#View(data.frame(table(AMNH$age)))
noStage <- c()
noStage <- c(noStage, which(AMNH$age == ""))
noStage <- c(noStage, which(AMNH$age == "(E)"))
noStage <- c(noStage, which(AMNH$age == "A"))
noStage <- c(noStage, which(AMNH$age == "Late, Or Permian"))
noStage <- c(noStage, which(AMNH$age == "Lower-Middle"))
noStage <- c(noStage, which(AMNH$age == "Lower/Middle"))
noStage <- c(noStage, which(AMNH$age == "P."))
noStage <- c(noStage, which(AMNH$age == "P.P.?"))
noStage <- c(noStage, which(AMNH$age == "Sub"))
noStage <- c(noStage, which(AMNH$age == "Recent"))
noStage <- c(noStage, which(AMNH$age == "Top"))
noStage <- c(noStage, which(AMNH$age == "Upper"))
noStage <- c(noStage, which(AMNH$age == "Upper ?"))
noStage <- c(noStage, which(AMNH$age == "W. Facies"))
noStage <- c(noStage, which(AMNH$age == "West Facies"))
noStage <- c(noStage, which(AMNH$age == "Conglomerate"))
noStage <- c(noStage, which(AMNH$age == "Florida"))
noStage <- c(noStage, which(AMNH$age == "Lower"))
noStage <- c(noStage, which(AMNH$age == "Middle"))
noStage <- c(noStage, which(AMNH$age == "Post"))
noStage <- c(noStage, which(AMNH$age == "San Pablo"))
noStage <- c(noStage, which(AMNH$age == "ARISAIG"))
noStage <- unique(noStage)
AMNH[noStage, "age"] <- ""

## Manual updates
AMNH[which(AMNH$age == "CASSIAN"),"formation"] <- "Cassian"
AMNH[which(AMNH$age == "Domengine"),"formation"] <- "Domengine"
AMNH[which(AMNH$age == "Ii Mediterran Stufe"),"formation"] <- "Eggenburg group"
AMNH[which(AMNH$age == "Med. Stf."),"formation"] <- "Eggenburg group"
AMNH[which(AMNH$age == "Med. Stuf."),"formation"] <- "Eggenburg group"
AMNH[which(AMNH$age == "Mediterranstuffe Ii."),"formation"] <- "Eggenburg group"
AMNH[which(AMNH$age == "Ithaca"),"formation"] <- "Chemung"
AMNH[which(AMNH$age == "Lincoln"),"formation"] <- "Pittsburg Bluff"
AMNH[which(AMNH$age == "MUSCHELKALK"),"formation"] <- "Muschelkalk"
AMNH[which(AMNH$age == "Wilcox"),"formation"] <- "Tuscahoma"

## Read in stages
stages <- read.csv("data/metadata/macrostrat_raw.csv", row.names = 1, header = T)
stages <- stages[!duplicated(stages[,c("name", "t_age", "b_age")]),]
stage_names <- stages$name

## Now clean - remove accents and swap iens for ians
source("functions/clean.stage.names.R")
AMNH <- clean.stage.names(data = AMNH, columns = "age")

## Now check against Macrostrat names
## Run bulk.update.stages (aggregation of all specific name changes identified thus far), then check for other errors
source("functions/bulk.update.stages.R")
AMNH$age <- bulk.update.stages(AMNH$age)

## Inspect and update function
#View(data.frame(table(AMNH$age)))
#View(data.frame(stage_names))

## Once function updated, load again and re-run
#source("functions/bulk.update.stages.R")
#AMNH$age <- bulk.update.stages(AMNH$age)

## Re-load and run function again after updates
#source("functions/bulk.update.stages.R")
#AMNH$age <- bulk.update.stages(AMNH$age)

## Split stages
#View(data.frame(table(AMNH$age)))

## No &s, ands, tos, or ors. Just refine punctuation
splitStages <- c()
splitStages <- c(splitStages, unique(AMNH$age[which(str_detect(AMNH$age, pattern = "[:punct:]"))]))
splitStages <- c(splitStages, AMNH$age[which(str_detect(AMNH$age, pattern = "[|]"))])
splitStages <- unique(splitStages)

## Wittle down to those that should be split
#View(data.frame(splitStages))
splitStages <- splitStages[c(1:10, 13:15, 18:20, 24, 30, 37, 46:48)]

## get intervals to be split
splitters <- c()
for(i in 1:length(splitStages)){
  splitters <- c(splitters, which(AMNH$age == splitStages[i]))
}

## Isolate stages
newStage <- data.frame(AMNH$age)
colnames(newStage) <- "Macrostrat_unit1"
newStage$Macrostrat_unit2 <- ""

## Specify characters to split by
p <- c('-', '|')

## Split early stages
for(i in splitters){
  for(m in p){
    if(str_detect(AMNH$age[i], pattern = fixed(m))){
      ## extract stages
      ageVec <- unlist(str_split(AMNH$age[i], pattern = fixed(m)))
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
AMNH$age <- NULL

## Re-attach to dataset
AMNH <- cbind(AMNH, newStage, "age_OLD" = orig_stages)

#### Splitting formations ####
## Manually inspect formations
#View(data.frame(table(AMNH$formation)))

## Manually sort problem formations
AMNH[which(AMNH$formation == "Denison Or Ft. Worth Beds"),"formation"] <- "Denison formation or Fort Worth formation"
AMNH[which(AMNH$formation == "Liberty or Whitewater"),"formation"] <- "Liberty formation or Whitewater formation"
AMNH[which(AMNH$formation == "D-10-Tebo Shale,Erving Pit;D-11-Lexington Coal Shideler Pit;D-12-Lexigton Coal Lear Pit"),"formation"] <- "Tebo Shale-Lexington Coal"
AMNH[which(AMNH$formation == "Pierre  Sh  |  Fox Hills  Fm contact"),"formation"] <- "Pierre Shale | Fox Hills"
AMNH[which(AMNH$formation == "Pierre  Sh  |  Fox Hills  Fm TRANSITION"),"formation"] <- "Pierre Shale | Fox Hills"
AMNH[which(AMNH$formation == "ZOHAR | MATMOR FM"),"formation"] <- "Zohar | Matmor"
AMNH[which(AMNH$formation == "TOP OF TINTON FM AND BASE OF HORNERSTOWN FM"),"formation"] <- "Tinton | Hornerstown"
AMNH[which(AMNH$formation == "WARSAW LS OR SECOND ARCHIMEDES LS"),"formation"] <- "Warsaw Limestone"
AMNH[which(AMNH$formation == "Upper Or Pliocene"),"formation"] <- ""

## Initially, tidy up punctuation
p <- unique(unlist(str_extract_all(AMNH$formation, pattern = "[[:punct:]]")))

## Check to see whether type of punctuation indicates two or more formations
#View(data.frame(table(AMNH$formation[which(str_detect(AMNH$formation, pattern = fixed(p[1])))])))

## Replace all non-hyphens, ambersands, and backslashes with spaces
p <- p[-c(6, 7, 12)]

## Clean up punctuation not in use
for(c in p){
  AMNH$formation <- str_replace_all(AMNH$formation, pattern = fixed(c), replacement = " ")
}

## Find formations to be split
## Now find formations to be split
forms <- unique(AMNH$formation[which(str_detect(AMNH$formation, pattern = "[:punct:]"))])
forms <- c(forms, AMNH$formation[which(str_detect(AMNH$formation, pattern = regex(" and ", ignore_case = T)))])
forms <- c(forms, AMNH$formation[which(str_detect(AMNH$formation, pattern = " & "))])
forms <- c(forms, AMNH$formation[which(str_detect(AMNH$formation, pattern = regex(" or ", ignore_case = T)))])
forms <- c(forms, AMNH$formation[which(str_detect(AMNH$formation, pattern = "[|]"))])
forms <- unique(forms)

## Wittle down to those to split
#View(data.frame(forms))
forms <- forms[c(3:6, 12:22, 25:29, 31:39)]
#View(data.frame(forms))

## get ages to be split
splitForms <- c()
for(i in 1:length(forms)){
  splitForms <- c(splitForms, which(AMNH$formation == forms[i]))
}

## Isolate formation
formations <- data.frame(AMNH$formation)
colnames(formations) <- "formation1"

## max 2 formations
formations$formation2 <- ""

## Specify string to split by
p <- c('/', '-', '&', ' or ', '|')

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
AMNH$formation <- NULL

## Re-attach to dataset
AMNH <- cbind(AMNH, formations)

#### Tidy up taxonomy and localities ####
## Tidy up phylum and classifications
#View(data.frame(table(AMNH$phylum)))
AMNH$phylum[which(AMNH$phylum == "Brachiopoda-articulata")] <- "Brachiopoda"
AMNH$phylum[which(AMNH$phylum == "")] <- "Mollusca"
#View(data.frame(table(AMNH$phylum)))
#View(data.frame(table(AMNH$class)))
AMNH$class[which(AMNH$class == "Bivalvia (lost)")] <- "Bivalvia"
AMNH$class[which(AMNH$class == "Lingulida")] <- "Lingulata"
AMNH$class[which(AMNH$class == "Stophomenata")] <- "Strophomenata"
AMNH$phylum[which(AMNH$class == "Brachiopoda")] <- "Brachiopoda"
AMNH$class[which(AMNH$class == "Brachiopoda")] <- ""
AMNH <- AMNH[-which(AMNH$class == "Gastropoda"),]

## Refresh noLL
noLL <- union(which(is.na(AMNH$latitudeDecimal)),which(is.na(AMNH$longitudeDecimal)))

## Refresh noLoc
#View(data.frame(table(AMNH$locality)))
noLoc <- c()
noLoc <- c(noLoc, which(AMNH$locality == ""))
noLoc <- c(noLoc, which(AMNH$locality == "(?)"))
noLoc <- c(noLoc, which(AMNH$locality == "#1, Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "#11 Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "#11, Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "#13, Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "#340 Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "#7 Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "#7, Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "#8 Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "#8, Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "#9 Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "#9, Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "1 Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "1, Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "11 Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "11, Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "12 Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "120 FMS."))
noLoc <- c(noLoc, which(AMNH$locality == "13 Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "13 Texas 13"))
noLoc <- c(noLoc, which(AMNH$locality == "13, Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "14 Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "14, Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "15 Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "15 Texas 15"))
noLoc <- c(noLoc, which(AMNH$locality == "15, Shale"))
noLoc <- c(noLoc, which(AMNH$locality == "15, Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "16, Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "17, Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "18 Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "18, Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "2 Or 4 Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "2 Or 4, Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "282 Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "282, Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "284, Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "290 Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "292, Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "512"))
noLoc <- c(noLoc, which(AMNH$locality == "7 Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "703"))
noLoc <- c(noLoc, which(AMNH$locality == "Bulldozer cut in hillside. See results on page after Mapes. Age upper shale member of the Graford Formation."))
noLoc <- c(noLoc, which(AMNH$locality == "HHSU47"))
noLoc <- c(noLoc, which(AMNH$locality == "Int. 6"))
noLoc <- c(noLoc, which(AMNH$locality == "Interval 6"))
noLoc <- c(noLoc, which(AMNH$locality == "Loc. 13, Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "Loc. 15, Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "Loc. 17, Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "Loc. 20, Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "Loc. 282, Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "Loc. 7, Oklahoma"))
noLoc <- c(noLoc, which(AMNH$locality == "Loc.13, Texas"))
noLoc <- c(noLoc, which(AMNH$locality == "LOC.316,LOT B"))
noLoc <- c(noLoc, which(AMNH$locality == "LOC.317,LOT C"))
noLoc <- c(noLoc, which(AMNH$locality == "Locality ?"))
noLoc <- c(noLoc, which(AMNH$locality == "Locality Unknown"))
noLoc <- c(noLoc, which(AMNH$locality == "Lot E"))
noLoc <- c(noLoc, which(AMNH$locality == "No Locality Info"))
noLoc <- c(noLoc, which(AMNH$locality == "Oklahoma 7"))
noLoc <- c(noLoc, which(AMNH$locality == "Rd Cut 6"))
noLoc <- c(noLoc, which(AMNH$locality == "Rd. Cut"))
noLoc <- c(noLoc, which(AMNH$locality == "SUBUNIT43"))
noLoc <- c(noLoc, which(AMNH$locality == "UNIT 17"))
noLoc <- c(noLoc, which(AMNH$locality == "UNIT 18B"))
noLoc <- c(noLoc, which(AMNH$locality == "Unknown"))
noLoc <- c(noLoc, which(AMNH$locality == "UNKNOWN"))
noLoc <- c(noLoc, which(AMNH$locality == "UNKNOWN, IN SHALY LS"))
noLoc <- c(noLoc, which(AMNH$locality == "UNKNOWN, MIDDLE MARL BEDS, YELLOW SANDS"))
noLoc <- c(noLoc, which(AMNH$locality == "UNKNOWN, NORTHERN PART OF COUNTY"))
noLoc <- c(noLoc, which(AMNH$locality == "UNKNOWN, SOUTHWEST PART"))
noLoc <- c(noLoc, which(AMNH$locality == "UNKNOWN, WESTERN PART OF THE STATE"))
noLoc <- unique(noLoc)
droppers <- intersect(noLL, noLoc)
AMNH <- AMNH[-droppers, ]

## Clean up punctuation
AMNH$formation1 <- str_replace_all(AMNH$formation1, pattern = "[:punct:]", replacement = "")
AMNH$formation2 <- str_replace_all(AMNH$formation2, pattern = "[:punct:]", replacement = "")

## Tidy up formation capitalization
AMNH$formation1 <- str_to_title(AMNH$formation1)
AMNH$formation2 <- str_to_title(AMNH$formation2)

## Then convert all uncertain species to "sp."
## Tidy up undetermined/indeterminate species
#View(data.frame(table(AMNH$species)))
gen.level <- c()
gen.level <- c(gen.level,which(AMNH[,"species"] == ""))
gen.level <- c(gen.level,which(AMNH[,"species"] == "aesop.&pyranitatoides"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "ap. A"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "ap. B"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "cp. sp 1"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "eburnea&perlaevis"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "indet."))
gen.level <- c(gen.level,which(AMNH[,"species"] == "medium"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "n sp"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "n. sp"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "n. sp."))
gen.level <- c(gen.level,which(AMNH[,"species"] == "n. sp.?"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "n.sp"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "n.sp?"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "n.sp."))
gen.level <- c(gen.level,which(AMNH[,"species"] == "new sp."))
gen.level <- c(gen.level,which(AMNH[,"species"] == "nov. sp."))
gen.level <- c(gen.level,which(AMNH[,"species"] == "nov.sp."))
gen.level <- c(gen.level,which(AMNH[,"species"] == "orbiculata & lingula"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp ?"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp #1"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp aff p."))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp?"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp."))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. ?"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. 1"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. 2"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. a"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. A"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. b"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. B"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. indet."))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. new"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. nov."))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. undes (?)"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. undes."))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. undesc."))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. undescr."))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. undescribed"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. undet"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. undet."))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. undeter."))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp. undetermined"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp.?"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp.1"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "sp.2"))
gen.level <- c(gen.level,which(AMNH[,"species"] == "undescribed"))
## Check for "aff"
if(any(str_detect(AMNH[,"species"], pattern = regex("aff", ignore_case = T)))){
  print("'aff's detected")
  affs <- unique(AMNH[,"species"][which(str_detect(AMNH[,"species"], pattern = regex("aff", ignore_case = T)))])
  affs
  ##
  affs <- affs[-c(1, 2, 3, 5)]
  affs
  gen.level <- c(gen.level, which(AMNH[,"species"] %in% affs))
}
## Check for "cf"
if(any(str_detect(AMNH[,"species"], pattern = regex("cf", ignore_case = T)))){
  print("'cf's detected")
  cfs <- unique(AMNH$species[which(str_detect(AMNH$species, pattern = regex("cf", ignore_case = T)))])
  cfs
  ## change some
  cfs <- cfs[-c(46)]
  cfs
  gen.level <- c(gen.level, which(AMNH[,"species"] %in% cfs))
}
## Check for dots
if(any(str_detect(AMNH[,"species"], pattern = "\\."))){
  print("periods detected")
  dots <- unique(AMNH$species[which(str_detect(AMNH$species, pattern = "\\."))])
  #View(data.frame(dots))
  ## prune out dots to retain
  dots <- dots[-c(10, 14, 20, 73, 75, 77, 79, 87, 88, 89, 92, 93, 95:100, 102)]
  #View(data.frame(dots))
  gen.level <- c(gen.level,which(AMNH[,"species"] %in% dots))
}
## Check for "?"
if(any(str_detect(AMNH[,"species"], pattern = "\\?"))){
  print("'?'s detected")
  questions <- unique(AMNH$species[which(str_detect(AMNH$species, pattern = "\\?"))])
  questions
  ## drop all
  gen.level <- c(gen.level,which(AMNH[,"species"] %in% questions))
}
gen.level <- unique(gen.level)
if(any(which(str_detect(AMNH$species, pattern = "[[:punct:]]")))){
  AMNH$species <- str_replace_all(AMNH$species, pattern = "[[:punct:]]", replacement = "")
}
AMNH[gen.level,"species"] <- "sp."

## Then fix capitalization for species
AMNH$species <- tolower(AMNH$species)

## Finally, check and drop holocene/recent entries
recent <- c()
recent <- c(recent, which(AMNH$Macrostrat_unit1 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(AMNH$Macrostrat_unit1 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(AMNH$Macrostrat_unit1 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(AMNH$Macrostrat_unit1 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(AMNH$Macrostrat_unit1 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(AMNH$Macrostrat_unit1 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(AMNH$Macrostrat_unit1 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(AMNH$Macrostrat_unit1 == regex("C1", ignore_case = T)))

recent <- c(recent, which(AMNH$Macrostrat_unit2 == regex("Meghalayan", ignore_case = T)))
recent <- c(recent, which(AMNH$Macrostrat_unit2 == regex("Haweran", ignore_case = T)))
recent <- c(recent, which(AMNH$Macrostrat_unit2 == regex("Holocene", ignore_case = T)))
recent <- c(recent, which(AMNH$Macrostrat_unit2 == regex("Quaternary", ignore_case = T)))
recent <- c(recent, which(AMNH$Macrostrat_unit2 == regex("NN21", ignore_case = T)))
recent <- c(recent, which(AMNH$Macrostrat_unit2 == regex("Cenozoic", ignore_case = T)))
recent <- c(recent, which(AMNH$Macrostrat_unit2 == regex("Phanerozoic", ignore_case = T)))
recent <- c(recent, which(AMNH$Macrostrat_unit2 == regex("C1", ignore_case = T)))
recent <- unique(recent)
if(length(recent) > 0){
  AMNH <- AMNH[-recent,]
}

#### Preparing final version for export ####
## Rearrange and re-label
colnames(AMNH)
AMNH <- AMNH[,c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 16, 15, 23, 24, 28, 29, 30, 13, 14, 31, 32, 25, 26, 27, 17, 18, 19, 20, 21, 22, 12)]

## Get rid of all spaces outside of string for formations
AMNH$formation1 <- str_trim(AMNH$formation1)
AMNH$formation2 <- str_trim(AMNH$formation2)

## Finally, combine or stages into a single string
units <- AMNH[,c(15:16)]
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
AMNH <- AMNH[,-c(15:16)]
AMNH <- cbind(AMNH, chronostratigraphy)

## Re-order
#View(data.frame(colnames(AMNH)))
AMNH <- AMNH[,c(1:14, 30, 15:29)]
#View(data.frame(colnames(AMNH)))

## Export
saveRDS(AMNH, file = "data/museum/AMNH.Rds")


