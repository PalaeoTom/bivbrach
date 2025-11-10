## 4.1.2.1 GLMM - genera, original covariates, raw - type 1 and type 2 error testing
## Started by TJS on 27/06/2025

#### Set up ####
## Clean up
rm(list=ls())

## Libraries
library(glmmTMB)
library(lme4)
library(simr)
library(stringr)
library(parallel)
library(sjmisc)
library(sjPlot)
library(cowplot)
library(ggplot2)

## Load data
raw_data <- read.csv("data/analysis_data/genera_CO_raw.csv", header = T, row.names = 1)

#### Prepping data ####
## Drop columns not being used in model
raw_data <- raw_data[,c(-1, -3)]

## Check numeric entries are numeric
nums <- c(2:9, 11)
for(i in nums){
  if(!is.numeric(raw_data[,i])){
    raw_data[,i] <- as.numeric(raw_data[,i])
  }
}

## And factors are factors
factors <- c(1,10)
for(i in factors){
  if(!is.numeric(raw_data[,i])){
    raw_data[,i] <- as.factor(raw_data[,i])
  }
}

## Relevel PTME factor
raw_data[,"PTME"] <- relevel(raw_data[,"PTME"], ref = "PrePTME")

## Rename to simplify models
colnames(raw_data) <- c("stage", "bivalve", "brachiopod", "long", "lat", "lith", "bath", "reef", "AbsLat", "PTME", "n")

## Some counts are decimal. Round these to discrete values to work with negative binomial
raw_data$bivalve <- round(raw_data$bivalve, digits = 0)
raw_data$brachiopod <- round(raw_data$brachiopod, digits = 0)

## Standardise predictors
raw_data <- std(raw_data, raw_data[,c(2, 6, 7, 8, 9, 11)])

## Drop non-standardized predictors
raw_data <- raw_data[,c(-2, -6, -7, -8, -9, -11)]

## Re-do names
colnames(raw_data) <- c("stage", "brachiopod", "long", "lat", "PTME", "bivalve", "lith", "bath", "reef", "AbsLat", "n")

#### Define best models
bestModel_1 <- glmmTMB(brachiopod ~ PTME + bath + reef + lith + n + (bivalve|stage), data = raw_data, family = nbinom1(link = "log"))
bestModel_2 <- glmmTMB(brachiopod ~ PTME + bath + reef + lith + AbsLat + n + (bivalve|stage), data = raw_data, family = nbinom1(link = "log"))
bestModel_3 <- glmmTMB(brachiopod ~ bivalve + PTME + bath + reef + lith + n + (bivalve|stage), data = raw_data, family = nbinom1(link = "log"))

#### Testing for type 2 errors ####
## Create output for predictor terms
labels <- c("Bathymetry", "Generic\nbivalve\nrichness", "Lithology", "PTME", "Reefs", "Absolute\nlatitude", "n", "Generic\nbivalve\nrichness +\nPTME")
term <- c("bath", "bivalve", "lith", "PTMEPostPTME", "reef", "AbsLat", "n", "bivalve:PTMEPostPTME")
powerSimTerms <- c("bath", "bivalve", "lith", "PTME", "reef", "AbsLat", "n", "bivalve:PTME")
type2_out <- data.frame(cbind("term" = term, "powerSimTerms" = powerSimTerms,  "labels" = labels))

## Create model containers
type2_out$bestModel_1 <- NA
type2_out$bestModel_2 <- NA
type2_out$bestModel_3 <- NA

## list models
bestModels <- list(bestModel_1, bestModel_2, bestModel_3)

## Initialise list for results
type2_results <- list()

## get terms
for(m in 1:length(bestModels)){
  ## output
  out <- list()
  ## define column
  c = m+3
  ## Get terms in model
  terms <- get_model_data(bestModels[[m]], type = "est", transform = NULL)[,"term"]
  ## for each term
  m = 3

  for(t in 3:length(terms)){
    ## find row
    r <- which(type2_out[,"term"] %in% terms[t])
    ## Get power sim results
    res <- powerSim(bestModels[[m]], nsim = 100, test = fcompare(paste0("brachiopod~",type2_out[r,"powerSimTerms"])))
    ## Record power value
    type2_out[r,c] <- res$x
    ## Save output
    out <- c(out, list(res))
  }
  type2_results <- c(type2_results, list(out))
}

## Export results
saveRDS(type2_results, "data/sensitivity_testing/genera_CO_raw_power_analyses_raw.Rds")
write.csv(type2_out, "data/sensitivity_testing/genera_CO_raw_power_analyses_power.csv")

#### Shuffling occurrences and richness values for type 1 error testing ####
## Need to read in occurrence data
occs <- readRDS("data/final/final_100_genera_covsPruned.Rds")

## Total number of occurrences in each grid cell, structure as data frame
sc_abun <- table(occs$stage_cell)
sc <- names(sc_abun)
abun <- as.vector(sc_abun)
ref <- data.frame(cbind("stage_cell" = sc, "abundance" = abun))

## Shuffle occurrences function
source("functions/shuffle_occurrences.R")
source("functions/get.cell.covariate.R")
source("functions/extract_cell_metadata.R")
source("functions/CR_richness.R")
source("functions/raw_richness.R")

## 1. Shuffle occurrences across stages
shuffled_occs_between_stages <- shuffle_occurrences(data = occs, reps = 100, stage = "stage", cell = "stage_cell", cell_abun = ref,
                                                    occ_covariates = c("lith_category","bath_category","reef_category"),
                                                    cell_covariates = c("cellLith", "cellBath", "cellReef"),
                                                    covariate_values = c("carbonate", "deep", "reef"), CR = F,
                                                    fix_stages = F, n.cores = 8)


## Export
saveRDS(shuffled_occs_between_stages, file = "data/sensitivity_testing/genera_CO_raw_shuffled_occurrences_btwnStgs.Rds")

## 2. Shuffle occurrences within stages
shuffled_occs_within_stages <- shuffle_occurrences(occs, 100, "stage", "stage_cell", cell_abun = ref,
                                                   occ_covariates = c("lith_category","bath_category","reef_category"),
                                                   cell_covariates = c("cellLith", "cellBath", "cellReef"),
                                                   covariate_values = c("carbonate", "deep", "reef"), CR = F,
                                                   fix_stages = T, n.cores = 8)
saveRDS(shuffled_occs_within_stages, file = "data/sensitivity_testing/genera_CO_raw_shuffled_occurrences_wthnStgs.Rds")

## Now standardise each dataset using code above!
nums <- c(4:11)
factors <- c(1:3,12)
WithinStages <- mclapply(1:length(shuffled_occs_within_stages), mc.cores = 8, function(x){
  out <- shuffled_occs_within_stages[[x]]
  ## Check numeric are numeric
  for(i in nums){
    if(!is.numeric(out[,i])){
      out[,i] <- as.numeric(out[,i])
    }
  }
  ## Check factors are factors
  for(i in factors){
    if(!is.factor(out[,i])){
      out[,i] <- as.factor(out[,i])
    }
  }
  ## Re-level PTME
  out[,"PTME"] <- relevel(out[,"PTME"], ref = "PrePTME")
  ## Round counts
  out$bivalve <- round(out$bivalve, digits = 0)
  out$brachiopod <- round(out$brachiopod, digits = 0)
  ## Standardise
  out <- std(out,out[,c(4,8,9,10,11)])
  out <- out[,c(-4,-8,-9,-10,-11)]
  colnames(out) <- c("stage_cell", "stage", "cells", "brachiopod", "long", "lat", "PTME", "bivalve", "lith", "bath", "reef", "AbsLat")
  return(out)
})

betweenStages <- mclapply(1:length(shuffled_occs_between_stages), mc.cores = 8, function(x){
  out <- shuffled_occs_between_stages[[x]]
  ## Check numeric are numeric
  for(i in nums){
    if(!is.numeric(out[,i])){
      out[,i] <- as.numeric(out[,i])
    }
  }
  ## Check factors are factors
  for(i in factors){
    if(!is.factor(out[,i])){
      out[,i] <- as.factor(out[,i])
    }
  }
  ## Re-level PTME
  out[,"PTME"] <- relevel(out[,"PTME"], ref = "PrePTME")
  ## Round counts
  out$bivalve <- round(out$bivalve, digits = 0)
  out$brachiopod <- round(out$brachiopod, digits = 0)
  ## Standardise
  out <- std(out,out[,c(4,8,9,10,11)])
  out <- out[,c(-4,-8,-9,-10,-11)]
  colnames(out) <- c("stage_cell", "stage", "cells", "brachiopod", "long", "lat", "PTME", "bivalve", "lith", "bath", "reef", "AbsLat")
  return(out)
})

## Reload raw data with n column
#raw_data <- read.csv("data/analysis_data/genera_CO_raw.csv", header = T, row.names = 1)

## Re-add N from main dataset to both datasets
betweenStages <- lapply(1:length(betweenStages), function(x){
  cn <- colnames(betweenStages[[x]])
  n <- raw_data[match(as.character(betweenStages[[x]][,"stage_cell"]),as.character(raw_data[,"stage_cell"])),"n"]
  out <- data.frame(cbind(betweenStages[[x]],n))
  colnames(out) <- c(cn, "n")
  return(out)
})

withinStages <- lapply(1:length(withinStages), function(x){
  cn <- colnames(withinStages[[x]])
  n <- raw_data[match(as.character(withinStages[[x]][,"stage_cell"]),as.character(raw_data[,"stage_cell"])),"n"]
  out <- data.frame(cbind(withinStages[[x]],n))
  colnames(out) <- c(cn, "n")
  return(out)
})

## Re-export
saveRDS(betweenStages, file = "data/sensitivity_testing/genera_CO_raw_shuffled_occurrences_btwnStgs.Rds")
saveRDS(withinStages, file = "data/sensitivity_testing/genera_CO_raw_shuffled_occurrences_wthnStgs.Rds")

#### Type 1 error testing ####
## Read in standardised data
betweenStages <- readRDS("data/sensitivity_testing/genera_CO_raw_shuffled_occurrences_btwnStgs.Rds")
withinStages <- readRDS("data/sensitivity_testing/genera_CO_raw_shuffled_occurrences_wthnStgs.Rds")

## Run models
betweenStages_bestModel_1 <- mclapply(1:length(betweenStages), mc.cores = 8, function(y){
  model <- tryCatch(
    {glmmTMB(brachiopod ~ PTME + bath + reef + lith + n + (bivalve|stage),
           data = betweenStages[[y]], family = nbinom1(link = "log"))},
    error = function(msg){
      return(NA)
    })
  if(!all(is.na(model))){
    out <- get_model_data(model, type = "est", transform = NULL)
  } else {
    out <- NA
  }
  return(out)
})

withinStages_bestModel_1 <- mclapply(1:length(withinStages), mc.cores = 8, function(y){
  model <- tryCatch(
    {glmmTMB(brachiopod ~ PTME + bath + reef + lith + n + (bivalve|stage),
             data = withinStages[[y]], family = nbinom1(link = "log"))},
    error = function(msg){
      return(NA)
    })
  if(!all(is.na(model))){
    out <- get_model_data(model, type = "est", transform = NULL)
  } else {
    out <- NA
  }
  return(out)
})

betweenStages_bestModel_2 <- mclapply(1:length(betweenStages), mc.cores = 8, function(y){
  model <- tryCatch(
    {glmmTMB(brachiopod ~ PTME + bath + reef + lith + AbsLat + n + (bivalve|stage),
             data = betweenStages[[y]], family = nbinom1(link = "log"))},
    error = function(msg){
      return(NA)
    })
  if(!all(is.na(model))){
    out <- get_model_data(model, type = "est", transform = NULL)
  } else {
    out <- NA
  }
  return(out)
})

withinStages_bestModel_2 <- mclapply(1:length(withinStages), mc.cores = 8, function(y){
  model <- tryCatch(
    {glmmTMB(brachiopod ~ PTME + bath + reef + lith + AbsLat + n + (bivalve|stage),
             data = withinStages[[y]], family = nbinom1(link = "log"))},
    error = function(msg){
      return(NA)
    })
  if(!all(is.na(model))){
    out <- get_model_data(model, type = "est", transform = NULL)
  } else {
    out <- NA
  }
  return(out)
})

betweenStages_bestModel_3 <- mclapply(1:length(betweenStages), mc.cores = 8, function(y){
  model <- tryCatch(
    {glmmTMB(brachiopod ~ bivalve + PTME + bath + reef + lith + n + (bivalve|stage),
             data = betweenStages[[y]], family = nbinom1(link = "log"))},
    error = function(msg){
      return(NA)
    })
  if(!all(is.na(model))){
    out <- get_model_data(model, type = "est", transform = NULL)
  } else {
    out <- NA
  }
  return(out)
})

withinStages_bestModel_3 <- mclapply(1:length(withinStages), mc.cores = 8, function(y){
  model <- tryCatch(
    {glmmTMB(brachiopod ~ bivalve + PTME + bath + reef + lith + n + (bivalve|stage),
             data = withinStages[[y]], family = nbinom1(link = "log"))},
    error = function(msg){
      return(NA)
    })
  if(!all(is.na(model))){
    out <- get_model_data(model, type = "est", transform = NULL)
  } else {
    out <- NA
  }
  return(out)
})

## Read in functions
source("functions/isolate_coeffs.R")
source("functions/compare_coeffs.R")
source("functions/isolate_and_compare_coeffs.R")

## Define export directories
fig.export.dir <- "figures/final/supplemental"
data.export.dir <- "data/sensitivity_testing"

## Create visualsRef object
axis.labels <- c("Bathymetry", "Generic bivalve richness", "Lithology", "PTME", "Reefs", "Generic bivalve richness + PTME", "Absolute latitude", "Sample size")
colours <- c("lightblue", "darkgreen", "purple", "darkgrey", "pink", "orange", "cyan", "navy")
term <- c("bath", "bivalve", "lith", "PTMEPostPTME", "reef", "bivalve:PTMEPostPTME", "AbsLat", "n")
visualsRef <- data.frame(cbind("term" = term, "labels" = axis.labels, "colour" = colours))

## Define plotlimits
plot.limits <- list("bivalve" = c(-0.5,1),
                    "bivalve:PTMEPostPTME" = c(-1.5,0.5),
                    "bath" = c(-0.5,0.5),
                    "lith" = c(-0.5,0.5),
                    "reef" = c(-0.5,0.5),
                    "PTMEPostPTME" = c(-3,0.5),
                    "AbsLat" = c(-0.5,0.5),
                    "n" = c(-0.5,0.5))

#simModels = withinStages_bestModel_1
#mainModel = bestModel_1

## Run function for each combination
## Within stages, best model 1
title = "Comparing simulated and empirical coefficients from 100km grid cells\nRaw generic richness, original covariate data only\n Occurrences shuffled within stages, best model 1"
coeffs <- c("bath", "lith", "reef", "PTMEPostPTME", "n")
figure.name <- "genera_CO_raw_richness_bestModel1_withinStgs_coeffs"
data.name <- "genera_CO_raw_richness_bestModel1_withinStgs_coeffs"

simModels = withinStages_bestModel_1
mainModel = bestModel_1
coeffs = coeffs
fig.export.dir = fig.export.dir
data.export.dir = data.export.dir
figure.name = figure.name
data.name = data.name
plot.title = title
plot.limits = plot.limits
visualsRef = visualsRef

isolate_and_compare_coeffs(simModels = withinStages_bestModel_1, mainModel = bestModel_1, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = plot.limits, visualsRef = visualsRef)

## Between stages, best model 1
title = "Comparing simulated and empirical coefficients from 100km grid cells\nRaw generic richness, original covariate data only\n Occurrences shuffled between stages, best model 1"
coeffs <- c("bath", "lith", "reef", "PTMEPostPTME", "n")
figure.name <- "genera_CO_raw_richness_bestModel1_betweenStgs_coeffs"
data.name <- "genera_CO_raw_richness_bestModel1_betweenStgs_coeffs"

isolate_and_compare_coeffs(simModels = betweenStages_bestModel_1, mainModel = bestModel_1, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = plot.limits, visualsRef = visualsRef)

## Within stages, best model 2
title = "Comparing simulated and empirical coefficients from 100km grid cells\nRaw generic richness, original covariate data only\n Occurrences shuffled within stages, best model 2"
coeffs <- c("bath", "lith", "reef", "PTMEPostPTME", "n", "AbsLat")
figure.name <- "genera_CO_raw_richness_bestModel2_withinStgs_coeffs"
data.name <- "genera_CO_raw_richness_bestModel2_withinStgs_coeffs"

isolate_and_compare_coeffs(simModels = withinStages_bestModel_2, mainModel = bestModel_2, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = plot.limits, visualsRef = visualsRef)

## Between stages, best model 2
title = "Comparing simulated and empirical coefficients from 100km grid cells\nRaw generic richness, original covariate data only\n Occurrences shuffled between stages, best model 2"
coeffs <- c("bath", "lith", "reef", "PTMEPostPTME", "n", "AbsLat")
figure.name <- "genera_CO_raw_richness_bestModel2_betweenStgs_coeffs"
data.name <- "genera_CO_raw_richness_bestModel2_betweenStgs_coeffs"

isolate_and_compare_coeffs(simModels = betweenStages_bestModel_2, mainModel = bestModel_2, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = plot.limits, visualsRef = visualsRef)

## Within stages, best model 3
title = "Comparing simulated and empirical coefficients from 100km grid cells\nRaw generic richness, original covariate data only\n Occurrences shuffled within stages, best model 3"
coeffs <- c("bath", "lith", "reef", "PTMEPostPTME", "n", "bivalve")
figure.name <- "genera_CO_raw_richness_bestModel3_withinStgs_coeffs"
data.name <- "genera_CO_raw_richness_bestModel3_withinStgs_coeffs"

isolate_and_compare_coeffs(simModels = withinStages_bestModel_3, mainModel = bestModel_3, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = plot.limits, visualsRef = visualsRef)

## Between stages, best model 3
title = "Comparing simulated and empirical coefficients from 100km grid cells\nRaw generic richness, original covariate data only\n Occurrences shuffled between stages, best model 3"
coeffs <- c("bath", "lith", "reef", "PTMEPostPTME", "n", "bivalve")
figure.name <- "genera_CO_raw_richness_bestModel3_betweenStgs_coeffs"
data.name <- "genera_CO_raw_richness_bestModel3_betweenStgs_coeffs"

isolate_and_compare_coeffs(simModels = betweenStages_bestModel_3, mainModel = bestModel_3, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = plot.limits, visualsRef = visualsRef)

