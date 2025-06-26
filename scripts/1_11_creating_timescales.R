#### 1.11 - creating timescales
## Started 04/06/2025

## If packages aren't installed, install them, then load them
packages <- c("velociraptr", "divDyn", "readr", "palaeoverse", "stringr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(velociraptr)
library(divDyn)
library(readr)
library(palaeoverse)
library(stringr)

## Clean directory
rm(list = ls())

#### Create time binning scheme ####
## Use updated version of Ogg et al. 2016 stage time binning scheme
## get version originally published in divDyn
data(stages)
divDyn_stages <- stages
rm(stages)

## Export for manual comparison
write.csv(divDyn_stages, "data/metadata/divDyn_stages.csv")

## Derive stage time binning scheme
## Download raw stage data and create names column
stages <- downloadTime('international ages')
stages$name <- row.names(stages)

## Export for manual comparison
write.csv(divDyn_stages, file = "data/metadata/divDyn_stages.csv")

## Re-order stages by age, oldest first
stages <- stages[order(stages$b_age, decreasing=TRUE), ]

## Create columns for new rounded ages
stages$b_round <- stages$t_round <- 0

## Read in function for defining rounded stage ages
source("functions/round.age.R")

## Define groupings for rounding - taken from Antell et al. (2020)
groupings <- list(u10 <- which(stages$b_age < 10),
                  u150 <- which(stages$b_age < 150 & stages$b_age > 10),
                  old <- which(stages$b_age > 150))

## Round all to two digits
for (group in 1:length(groupings)){
  bins <- groupings[[group]]
  digits <- c(3, 3, 3)[group]

  # round all up (so no 0-0 bins, and no overlap)
  for (i in bins){
    b <- stages$b_age[i]
    t <- stages$b_age[i+1]
     stages$b_round[i] <- round.age(b, digits=digits, round_up=T)
     stages$t_round[i] <- round.age(t, digits=digits, round_up=T)
   }
 }

## Inspect time bins for overlap
## First bin is always fine
checker <- matrix(NA, nrow = length(2:nrow(stages)), ncol = 2)
checker[,1] <- stages[,"name"][-1]
for(i in 2:nrow(stages)){
  if(stages$t_round[i-1] == stages$b_round[i]){
    checker[i-1,2] <- "equal to previous"
  } else {
    checker[i-1,2] <- "unequal"
  }
}
View(checker)

## No overlap between bins. Export for manual refinement
write.csv(stages, file = "data/metadata/rounded_macrostrat_stages.csv")

## Condense manually using excel, then read in (quicker than writing code)
## Make sure manually condensed version is named final_stages.csv
stages <- read.csv("data/metadata/final_stages.csv", row.names = 1)

## Re-do rounding
stages$b_round <- stages$t_round <- 0

## Read in function for defining rounded stage ages
source("functions/round.age.R")

## Define groupings for rounding - taken from Antell et al. (2020)
groupings <- list(u10 <- which(stages$b_age < 10),
                  u150 <- which(stages$b_age < 150 & stages$b_age > 10),
                  old <- which(stages$b_age > 150))

## Round all to two digits
for (group in 1:length(groupings)){
  bins <- groupings[[group]]
  digits <- c(3, 3, 3)[group]

  # round all up (so no 0-0 bins, and no overlap)
  for (i in bins){
    b <- stages$b_age[i]
    t <- stages$b_age[i+1]
     stages$b_round[i] <- round.age(b, digits=digits, round_up=T)
     stages$t_round[i] <- round.age(t, digits=digits, round_up=T)
   }
}

# Holocene t_round needs changing to 0
 stages[92,"t_round"] <- 0

## Inspect time bins for overlap
## First bin is always fine
checker <- matrix(NA, nrow = length(2:nrow(stages)), ncol = 2)
checker[,1] <- stages[,"name"][-1]
for(i in 2:nrow(stages)){
  if(stages$t_round[i-1] == stages$b_round[i]){
    checker[i-1,2] <- "equal to previous"
  } else {
    checker[i-1,2] <- "unequal"
  }
}
View(checker)

## No overlap between bins. Update midpoints
source("functions/get.midpoints.R")
stages$Midpoint <- get.midpoints(stages[,c(7,8)])

## Refine to final columns
stages <- stages[,c(1,6,7,8,5)]
rownames(stages) <- NULL
colnames(stages) <- c("interval","midpoint","LAD","FAD","colour")

## Export
write.csv(stages, file = "data/metadata/binning_timescale.csv")

#### Creating timescale for chrono_scale ####
## Clean up directory
rm(list=ls())

## Load new stages, epochs, and periods
new_stages <- downloadTime('international ages')
new_epochs <- downloadTime('international epochs')
new_periods <- downloadTime('international periods')
new_stages$name <- row.names(new_stages)
new_epochs$name <- row.names(new_epochs)
new_periods$name <- row.names(new_periods)

## Re-order by age, oldest first
new_stages <- new_stages[order(new_stages$b_age, decreasing=TRUE), ]
new_epochs <- new_epochs[order(new_epochs$b_age, decreasing=TRUE), ]
new_periods <- new_periods[order(new_periods$b_age, decreasing=TRUE), ]

## Create columns for new rounded ages
new_stages$b_round <- new_stages$t_round <- 0
new_periods$b_round <- new_periods$t_round <- 0
new_epochs$b_round <- new_epochs$t_round <- 0

## Read in function for defining rounded stage ages
source("functions/round.age.R")
source("functions/round.timescale.R")

## Define groupings for rounding - taken from Antell et al. (2020)
stage.groupings <- list(u10 <- which(new_stages$b_age < 10),
                  u150 <- which(new_stages$b_age < 150 & new_stages$b_age > 10),
                  old <- which(new_stages$b_age > 150))

epoch.groupings <- list(u10 <- which(new_epochs$b_age < 10),
                        u150 <- which(new_epochs$b_age < 150 & new_epochs$b_age > 10),
                        old <- which(new_epochs$b_age > 150))

period.groupings <- list(u10 <- which(new_periods$b_age < 10),
                        u150 <- which(new_periods$b_age < 150 & new_periods$b_age > 10),
                        old <- which(new_periods$b_age > 150))

## Get rounded ages
new_stages <- round.timescale(new_stages, groupings = stage.groupings, check = T)
new_periods <- round.timescale(new_periods, groupings = period.groupings, check = T)
new_epochs <- round.timescale(new_epochs, groupings = epoch.groupings, check = T)

## Change NA values to 0
new_stages[102,"t_round"] <- 0
new_periods[22,"t_round"] <- 0
new_epochs[34,"t_round"] <- 0

## Prune down to columns I need
new_stages <- new_stages[,c(2,6,10,11)]
colnames(new_stages) <- c("Interval", "type", "LAD", "FAD")

new_epochs <- new_epochs[,c(2,6,10,11)]
colnames(new_epochs) <- c("Interval", "type", "LAD", "FAD")

new_periods <- new_periods[,c(2,6,10,11)]
colnames(new_periods) <- c("Interval", "type", "LAD", "FAD")

## Combine
new_intervals <- rbind(new_stages,new_epochs,new_periods)

## Remove rownames
rownames(new_intervals) <- NULL

## Drop duplicated Pridoli
new_intervals <- new_intervals[-which(new_intervals$Interval == "Pridoli")[2],]

## Update types to match GTS types
new_intervals$type <- str_replace(new_intervals$type, "age", "ICS Age")
new_intervals$type <- str_replace(new_intervals$type, "epoch", "ICS Epoch")
new_intervals$type <- str_replace(new_intervals$type, "period", "ICS Period")

## Re-order by age, oldest first
new_intervals <- new_intervals[order(new_intervals$FAD, decreasing=TRUE), ]

## Now to load time scale to update - this contain all ages in PBDB. We will add a few missing possibilities.
data("GTS_2020")

## Check which intervals need to have names updated
new_intervals$Interval[which(!new_intervals$Interval %in% GTS_2020$Interval)]

## Just Wuliuan - should add this to GTS_2020. Movement of matching boundaries will get other spelling.
GTS_2020 <- rbind(GTS_2020,GTS_2020[which(GTS_2020$Interval == "Wulian")[1],])
GTS_2020[which(GTS_2020$Interval == "Wulian")[3],"Interval"] <- "Wuliuan"

## Should also add duplicates of all Early and Late intervals
earlys <- GTS_2020[which(str_detect(GTS_2020$Interval,pattern = "Early ")),]
earlys$Interval <- str_replace(earlys$Interval, pattern = "Early ", replacement = "Lower ")
earlys$Interval <- str_replace(earlys$Interval, pattern = "early ", replacement = "lower ")
earlys$Interval <- str_replace(earlys$Interval, pattern = "late ", replacement = "upper ")

lates <- GTS_2020[which(str_detect(GTS_2020$Interval,pattern = "Late ")),]
lates$Interval <- str_replace(lates$Interval, pattern = "Late ", replacement = "Upper ")
lates$Interval <- str_replace(lates$Interval, pattern = "early ", replacement = "lower ")
lates$Interval <- str_replace(lates$Interval, pattern = "late ", replacement = "upper ")

## Recombine
GTS_2020 <- rbind(GTS_2020,earlys,lates)

## Find duplicated intervals
duplicated_ints <- names(which(table(GTS_2020$Interval)>1))

## Retain those with lower order values
for(i in duplicated_ints){
  ## Extract
  dups <- GTS_2020[which(GTS_2020$Interval==i),]
  GTS_2020 <- GTS_2020[-which(GTS_2020$Interval==i),]
  ## Filter
  dups <- dups[which(dups$Order == min(dups$Order)),]
  ## Recombine
  GTS_2020 <- rbind(GTS_2020,dups)
}

## Reorder by order
GTS_2025 <- GTS_2020[order(GTS_2020$Order, decreasing=F), ]

## Check for duplicates
names(which(table(GTS_2025$Interval)>1))
## None! Job done!

## Re-do order - need to ensure all intervals submitted to the functions below are this order
GTS_2025$Order <- seq(1,nrow(GTS_2025),1)

## Update column names
colnames(GTS_2025) <- c("Interval","type","range","FAD","LAD","FAD_basis","LAD_basis","notes","order")

## Final adjustment - combine type and range into single type
GTS_2025$type <- apply(GTS_2025, 1, function(x) str_flatten(c(x[2],x[3]),collapse = " "))
GTS_2025$range <- NULL

## Now bring into alignment early/lower FADs and late/upper LADs with parent interval boundaries
## find all intervals missing these strings
ear <- which(!str_detect(GTS_2025$Interval, pattern = regex("early ", ignore_case = T)))
low <- which(!str_detect(GTS_2025$Interval, pattern = regex("lower ", ignore_case = T)))
n1 <- intersect(ear,low)

up <- which(!str_detect(GTS_2025$Interval, pattern = regex("upper ", ignore_case = T)))
lat <- which(!str_detect(GTS_2025$Interval, pattern = regex("late ", ignore_case = T)))
n2 <- intersect(up,lat)
n3 <- intersect(n1,n2)

## Update FADs and LADs to match
for(i in n3){
  FAD_t <- c()
  ## lower Lower
  FAD_t <- c(FAD_t,which(GTS_2025$Interval==paste0("lower Lower ",GTS_2025$Interval[i])))
  ## early Early
  FAD_t <- c(FAD_t,which(GTS_2025$Interval==paste0("early Early ",GTS_2025$Interval[i])))
  ## Lower
  FAD_t <- c(FAD_t,which(GTS_2025$Interval==paste0("Lower ",GTS_2025$Interval[i])))
  ## Early
  FAD_t <- c(FAD_t,which(GTS_2025$Interval==paste0("Early ",GTS_2025$Interval[i])))
  ## Get unique
  FAD_t <- unique(FAD_t)
  ## Update the FADs of these indexed intervals to match that of parent interval
  if(length(FAD_t)>0){
    GTS_2025[FAD_t,"FAD"] <- GTS_2025[i,"FAD"]
  }
  ## Same for LAD
  LAD_t <- c()
  ## lower Lower
  LAD_t <- c(LAD_t,which(GTS_2025$Interval==paste0("upper Upper ",GTS_2025$Interval[i])))
  ## early Early
  LAD_t <- c(LAD_t,which(GTS_2025$Interval==paste0("late Late ",GTS_2025$Interval[i])))
  ## Lower
  LAD_t <- c(LAD_t,which(GTS_2025$Interval==paste0("Upper ",GTS_2025$Interval[i])))
  ## Early
  LAD_t <- c(LAD_t,which(GTS_2025$Interval==paste0("Late ",GTS_2025$Interval[i])))
  ## Get unique
  LAD_t <- unique(LAD_t)
  ## Update the LADs of these indexed intervals to match that of parent interval
  if(length(LAD_t)>0){
    GTS_2025[LAD_t,"LAD"] <- GTS_2025[i,"LAD"]
  }
}

## Read in function for updating intervals. This is also updates matching boundaries
source("functions/update.intervals.R")

## First up, bring Miocene and Late Miocene LADs into alignment
#GTS_2025[which(GTS_2025$Interval == "Late Miocene"),"LAD"] <- GTS_2025[which(GTS_2025$Interval == "Miocene"),"LAD"]
#GTS_2025[which(GTS_2025$Interval == "Upper Miocene"),"LAD"] <- GTS_2025[which(GTS_2025$Interval == "Miocene"),"LAD"]

## Run function
#intervals = GTS_2025
#update = new_intervals
#update_source = "ICS_via_Macrostrat_API_05_06_2025"
#name = "Interval"
#FAD = "FAD"
#LAD = "LAD"
#FAD_basis = "FAD_basis"
#LAD_basis = "LAD_basis"
#retain_boundary_alignment = T

GTS_final <- update.intervals(intervals = GTS_2025, update = new_intervals, update_source = "ICS_via_Macrostrat_API_05_06_2025")

## Reorder by order
GTS_final <- GTS_final[order(GTS_final$order, decreasing=F), ]

## Now to read in function for interpolating
source("functions/interpolate.intervals.R")

#intervals = GTS_final
#original = GTS_2025
#interpolate = new_stages
#interpolate_source = "ICS_via_Macrostrat_API_05_06_2025"
#ignore = "ICS_via_Macrostrat_API_05_06_2025"
#name = "Interval"
#FAD = "FAD"
#LAD = "LAD"
#FAD_basis = "FAD_basis"
#LAD_basis = "LAD_basis"
#scheme = "type"

## Now interpolate using stages
GTS_final_1 <- interpolate.intervals(intervals = GTS_final, original = GTS_2025, interpolate = new_stages, interpolate_source = "ICS_via_Macrostrat_API_05_06_2025", ignore = "ICS_via_Macrostrat_API_05_06_2025")

## Reorder by order
GTS_final_1 <- GTS_final_1[order(GTS_final_1$order, decreasing=F), ]

## Now interpolate using epochs
GTS_final_2 <- interpolate.intervals(intervals = GTS_final_1, original = GTS_2025, interpolate = new_epochs, interpolate_source = "ICS_via_Macrostrat_API_05_06_2025", ignore = "ICS_via_Macrostrat_API_05_06_2025")

## Reorder by order
GTS_final_2 <- GTS_final_2[order(GTS_final_2$order, decreasing=F), ]

## Now to do it with periods, ignoring those that have been done for stages and epochs
GTS_final_3 <- interpolate.intervals(intervals = GTS_final_2, original = GTS_2025, interpolate = new_periods, interpolate_source = "ICS_via_Macrostrat_API_05_06_2025", ignore = "ICS_via_Macrostrat_API_05_06_2025")

## All good! Reorder by order
GTS_final_3 <- GTS_final_3[order(GTS_final_3$order, decreasing=F), ]

## Check that all LADs are more than FADs
any(GTS_final_3$LAD > GTS_final_3$FAD)

## Export
write.csv(GTS_final_3, file = "data/metadata/cleaning_timescale.csv")


