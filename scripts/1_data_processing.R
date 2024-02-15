## Initial data processing
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
if(length(c("fossilbrush", "velociraptr")[!c("fossilbrush", "velociraptr") %in% installed.packages()[,"Package"]]) > 0){
  install.packages(c("fossilbrush", "velociraptr")[!c("fossilbrush", "velociraptr") %in% installed.packages()[,"Package"]])
}
library(fossilbrush)
library(velociraptr)

#### PBDB ####
## Load raw data
raw_PBDB <- readRDS("data/PBDB_Nov23.Rds")

## use fossilbrush to update Chronostratigraphy
PBDB <- chrono_scale(raw_PBDB,  tscale = "GTS2020", srt = "early_interval", end = "late_interval",
                           max_ma = "max_ma", min_ma = "min_ma", verbose = FALSE)

## set new chronostratigraphy as "max_ma" and "min_ma"
PBDB$max_ma <- PBDB$newFAD
PBDB$min_ma <- PBDB$newLAD
PBDB <- PBDB[,-c(29,30)]

## Add stages (to mirror Antell sampling approach)
# The ICS chronostratigraphic chart is available at www.stratigraphy.org
# The same dates can be downloaded through the Macrostrat API, directly or via {velociraptr}
stages <- downloadTime('international ages')
stages$name <- row.names(stages)
stages <- stages[order(stages$b_age, decreasing=TRUE), ]
stages$b_round <- stages$t_round <- 0

## Define stages to omit (following Antell)
stages2omit <- c('Stage 2','Stage 3','Stage 4','Wuliuan',
                 'Drumian','Guzhangian','Paibian','Jiangshanian',
                 'Stage 10',
                 'Floian','Darriwilian',
                 'Katian', # otherwise no seed cells for Sandbian
                 'Aeronian', # otherwise no Rhuddanian or Aeronian seed cells
                 'Homerian','Ludfordian','Pragian','Eifelian','Bashkirian','Kasimovian',
                 'Sakmarian','Kungurian', # no Artinskian species records
                 'Olenekian', # otherwise only 1 abundance datum for Olenekian
                 'Sinemurian', # Hettangian is too poorly sampled
                 'Bajocian', # otherwise no Aalenian seed cells
                 'Hauterivian','Barremian', # no seed cells for Haut., Barremian or Valanginian alone
                 'Santonian', # otherwise nothing survives from Coniacian
                 'Thanetian',
                 'Bartonian', # otherwise no environmental data for Bartonian
                 'Aquitanian', # otherwise no seeds here or in Chattian
                 'Serravallian', # otherwise no seed cells for Langhian
                 'Messinian', # otherwise no seed cells for Messinian
                 'Calabrian','Middle Pleistocene','Late Pleistocene', # otherwise weird extinction rates
                 'Northgrippian','Meghalayan') # lump all Holocene records so they're easy to remove later

## Create a new stage object with stages listed above omitted
stages_trunc <- stages[!(stages$name %in% stages2omit),]


## cleaning data
## first, set ranks and acceptable suffixes to be used in dataset
b_ranks <- c("phylum", "class", "order", "family", "genus")
b_suff = list(NULL, NULL, NULL, NULL, c("ina", "ella", "etta"))

## next, replace PBDB default missing data entry with NA
PBDB[grep("NO_", PBDB[,"phylum"]), "phylum"] <- NA
PBDB[grep("NO_", PBDB[,"class"]), "class"] <- NA
PBDB[grep("NO_", PBDB[,"order"]), "order"] <- NA
PBDB[grep("NO_", PBDB[,"family"]), "family"] <- NA
PBDB[grep("NO_", PBDB[,"genus"]), "genus"] <- NA

## now clean up taxonomy using fossil brush
PBDB <- check_taxonomy(PBDB, suff_set = b_suff, ranks = b_ranks, clean_name = TRUE, resolve_duplicates = TRUE, jump = 5)$data

## check for and remove any entries with nonsensical entries (LAD older than FAD)
any(PBDB$max_ma < PBDB$min_ma)

## none present!

## Isolate bivalve and brachiopod data
brach_PBDB <- PBDB[which(PBDB[,12] %in% "Brachiopoda"),]
biv_PBDB <- PBDB[which(PBDB[,13] %in% "Bivalvia"),]

## Final steps: 1. check synonyms (and harmonise if necessary) and 2. frequency density distributions (for unusual tails or multimodal distributions)
## Synonyms to be checked later (80ish) with Cooper.
## Frequency density distributions to be checked later.

#### Other data sources ####
## TBC

#### Final dataset preparation ####
## Combine with other data sources for final data files
brach <- brach_c
biv <- biv_c

## Export polished files
saveRDS(brach, file = "data/brach.Rds")
saveRDS(biv, file = "data/biv.Rds")
