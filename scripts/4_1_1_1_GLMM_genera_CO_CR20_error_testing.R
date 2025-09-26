## 4.1.4.1 GLMM - genera, original covariates, CR20 - type 1 and type 2 error testing
## Started by TJS on 27/06/2025

#### Set up ####
## Clean up
rm(list=ls())

## Libraries
library(lme4)
library(simr)
library(stringr)
library(parallel)
library(sjmisc)
library(sjPlot)

#### Load data and define model ####
## Read in data
raw_data <- read.csv("data/analysis_data/genera_CO_CR20.csv", header = T, row.names = 1)

## Drop columns not being used in model
raw_data <- raw_data[,c(-1, -3)]

## Check numeric entries are numeric
nums <- c(2:9)
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
colnames(raw_data) <- c("stage", "bivalve", "brachiopod", "long", "lat", "lith", "bath", "reef", "AbsLat", "PTME")

## Some counts are decimal. Round these to discrete values to work with negative binomial
raw_data$bivalve <- round(raw_data$bivalve, digits = 0)
raw_data$brachiopod <- round(raw_data$brachiopod, digits = 0)

## Standardise predictors
raw_data <- std(raw_data, raw_data[,c(2, 6, 7, 8, 9)])

## Drop non-standardized predictors
raw_data <- raw_data[,c(-2, -6, -7, -8, -9)]

## Re-do names
colnames(raw_data) <- c("stage", "brachiopod", "long", "lat", "PTME", "bivalve", "lith", "bath", "reef", "AbsLat")

## Drop columns not being used
modelData <- raw_data[,c(1,2,5,6,7,8,9,10)]

## Define best model
bestModel <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + (bivalve|stage),
                   data = modelData, family = poisson(link = "sqrt"),
                   control = glmerControl(optimizer="bobyqa"))

#### Testing for type 2 errors ####
## Use simr to test each predictor separately
sim_PTME <- powerSim(bestModel, nsim = 100, test = fcompare(brachiopod~PTME))
sim_bath <- powerSim(bestModel, nsim = 100, test = fcompare(brachiopod~bath))
sim_reef <- powerSim(bestModel, nsim = 100, test = fcompare(brachiopod~reef))
sim_lith <- powerSim(bestModel, nsim = 100, test = fcompare(brachiopod~lith))
sim_bivalve <- powerSim(bestModel, nsim = 100, test = fcompare(brachiopod~bivalve))
sim_PTME
sim_bath
sim_lith
sim_reef
sim_bivalve

## Combine results into single object and export
power_results <- list(sim_PTME, sim_bath, sim_reef, sim_lith, sim_bivalve)
saveRDS(power_results, "data/sensitivity_testing/genera_CO_CR20_power_analyses.Rds")

#### Testing for type 1 errors ####
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

## 1. Shuffle occurrences across stages
shuffled_occs_between_stages <- shuffle_occurrences(data = occs, reps = 100, stage = "stage", cell = "stage_cell", cell_abun = ref,
                                                    occ_covariates = c("lith_category","bath_category","reef_category"),
                                                    cell_covariates = c("cellLith", "cellBath", "cellReef"),
                                                    covariate_values = c("carbonate", "deep", "reef"), CR_nOccs = 20,
                                                    fix_stages = F, n.cores = 8)


## Export
saveRDS(shuffled_occs_between_stages, file = "data/sensitivity_testing/genera_CO_shuffled_occurrences_btwnStgs.Rds")

## 2. Shuffle occurrences within stages
shuffled_occs_within_stages <- shuffle_occurrences(occs, 100, "stage", "stage_cell", cell_abun = ref,
                                                    occ_covariates = c("lith_category","bath_category","reef_category"),
                                                    cell_covariates = c("cellLith", "cellBath", "cellReef"),
                                                    covariate_values = c("carbonate", "deep", "reef"), CR_nOccs = 20,
                                                   fix_stages = T, n.cores = 8)
saveRDS(shuffled_occs_within_stages, file = "data/sensitivity_testing/genera_CO_shuffled_occurrences_wthnStgs.Rds")

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

## Re-export
saveRDS(betweenStages, file = "data/sensitivity_testing/genera_CO_shuffled_occurrences_btwnStgs.Rds")
saveRDS(WithinStages, file = "data/sensitivity_testing/genera_CO_shuffled_occurrences_wthnStgs.Rds")

## Read in standardised data
betweenStages <- readRDS("data/sensitivity_testing/genera_CO_shuffled_occurrences_btwnStgs.Rds")
WithinStages <- readRDS("data/sensitivity_testing/genera_CO_shuffled_occurrences_wthnStgs.Rds")

## Rerun best model for both sets of shuffled data
betweenStages_models <- mclapply(1:length(betweenStages), mc.cores = 4, function(y){
  model <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + (bivalve|stage),
                 data = betweenStages[[y]], family = poisson(link = "sqrt"),
                 control = glmerControl(optimizer="bobyqa"))
  out <- get_model_data(model, type = "est", transform = NULL)
})

WithinStages_models <- mclapply(1:length(WithinStages), mc.cores = 4, function(y){
  model <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + (bivalve|stage),
                 data = WithinStages[[y]], family = poisson(link = "sqrt"),
                 control = glmerControl(optimizer="bobyqa"))
  out <- get_model_data(model, type = "est", transform = NULL)
})

## Isolate coefficients and p-values
## Load function
source("functions/isolate_coeffs.R")

## Run function
withinStages_values <- isolate.coeffs(WithinStages_models)
betweenStages_values <- isolate.coeffs(betweenStages_models)

## Get original model coefficients
emp_coeffs <- get_model_data(bestModel, type = "est", transform = NULL)

## Define new compare coeffs function
source("functions/compare_coeffs.R")

## Run functions - start by just checking signs and p-values
## Within stages
length(which(compare_coeffs(emp_coeffs, withinStages_values, coeff = "bivalve", p_value_and_coeff = F)))
length(which(compare_coeffs(emp_coeffs, withinStages_values, coeff = "bivalve", p_value_and_coeff = T)))

length(which(compare_coeffs(emp_coeffs, withinStages_values, coeff = "bath", p_value_and_coeff = F)))
length(which(compare_coeffs(emp_coeffs, withinStages_values, coeff = "bath", p_value_and_coeff = T)))

length(which(compare_coeffs(emp_coeffs, withinStages_values, coeff = "lith", p_value_and_coeff = F)))
length(which(compare_coeffs(emp_coeffs, withinStages_values, coeff = "lith", p_value_and_coeff = T)))

length(which(compare_coeffs(emp_coeffs, withinStages_values, coeff = "reef", p_value_and_coeff = F)))
length(which(compare_coeffs(emp_coeffs, withinStages_values, coeff = "reef", p_value_and_coeff = T)))

length(which(compare_coeffs(emp_coeffs, withinStages_values, coeff = "PTMEPostPTME", p_value_and_coeff = F)))
length(which(compare_coeffs(emp_coeffs, withinStages_values, coeff = "PTMEPostPTME", p_value_and_coeff = T)))

length(which(compare_coeffs(emp_coeffs, withinStages_values, coeff = "bivalve:PTMEPostPTME", p_value_and_coeff = F)))
length(which(compare_coeffs(emp_coeffs, withinStages_values, coeff = "bivalve:PTMEPostPTME", p_value_and_coeff = T)))

## Between stages
length(which(compare_coeffs(emp_coeffs, betweenStages_values, coeff = "bivalve", p_value_and_coeff = F)))
length(which(compare_coeffs(emp_coeffs, betweenStages_values, coeff = "bivalve", p_value_and_coeff = T)))

length(which(compare_coeffs(emp_coeffs, betweenStages_values, coeff = "bath", p_value_and_coeff = F)))
length(which(compare_coeffs(emp_coeffs, betweenStages_values, coeff = "bath", p_value_and_coeff = T)))

length(which(compare_coeffs(emp_coeffs, betweenStages_values, coeff = "lith", p_value_and_coeff = F)))
length(which(compare_coeffs(emp_coeffs, betweenStages_values, coeff = "lith", p_value_and_coeff = T)))

length(which(compare_coeffs(emp_coeffs, betweenStages_values, coeff = "reef", p_value_and_coeff = F)))
length(which(compare_coeffs(emp_coeffs, betweenStages_values, coeff = "reef", p_value_and_coeff = T)))

length(which(compare_coeffs(emp_coeffs, betweenStages_values, coeff = "PTMEPostPTME", p_value_and_coeff = F)))
length(which(compare_coeffs(emp_coeffs, betweenStages_values, coeff = "PTMEPostPTME", p_value_and_coeff = T)))

length(which(compare_coeffs(emp_coeffs, betweenStages_values, coeff = "bivalve:PTMEPostPTME", p_value_and_coeff = F)))
length(which(compare_coeffs(emp_coeffs, betweenStages_values, coeff = "bivalve:PTMEPostPTME", p_value_and_coeff = T)))

## Load function for plotting histogram
source("functions/generate_coeff_histogram.R")

## Plot histograms by isolating coefficients
## Between stages
pdf("figures/final/supplemental/genera_CO_CR20_richness_bivalve_coeff_null_vs_emp_btwnStgs.pdf")
generate_coeff_histogram(emp_coeffs, betweenStages_values, coeff = "bivalve", xlim = c(-1,0))
dev.off()
pdf("figures/final/supplemental/genera_CO_CR20_richness_PTME_bivalve_interact_coeff_null_vs_emp_btwnStgs.pdf")
generate_coeff_histogram(emp_coeffs, betweenStages_values, coeff = "bivalve:PTMEPostPTME", xlim = c(-0.5,0.5))
dev.off()
pdf("figures/final/supplemental/genera_CO_CR20_richness_bathymetry_coeff_null_vs_emp_btwnStgs.pdf")
generate_coeff_histogram(emp_coeffs, betweenStages_values, coeff = "bath", xlim = c(-0.5,0.5))
dev.off()
pdf("figures/final/supplemental/genera_CO_CR20_richness_lithology_coeff_null_vs_emp_btwnStgs.pdf")
generate_coeff_histogram(emp_coeffs, betweenStages_values, coeff = "lith", xlim = c(-0.5,0.5))
dev.off()
pdf("figures/final/supplemental/genera_CO_CR20_richness_reef_coeff_null_vs_emp_btwnStgs.pdf")
generate_coeff_histogram(emp_coeffs, betweenStages_values, coeff = "reef", xlim = c(-0.5,0.5))
dev.off()
pdf("figures/final/supplemental/genera_CO_CR20_richness_PTME_coeff_null_vs_emp_btwnStgs.pdf")
generate_coeff_histogram(emp_coeffs, betweenStages_values, coeff = "PTMEPostPTME", xlim = c(-1.5,1.5))
dev.off()

## Within stages
pdf("figures/final/supplemental/genera_CO_CR20_richness_bivalve_coeff_null_vs_emp_withinStgs.pdf")
generate_coeff_histogram(emp_coeffs, withinStages_values, coeff = "bivalve", xlim = c(-1,0))
dev.off()
pdf("figures/final/supplemental/genera_CO_CR20_richness_PTME_bivalve_interact_coeff_null_vs_emp_withinStgs.pdf")
generate_coeff_histogram(emp_coeffs, withinStages_values, coeff = "bivalve:PTMEPostPTME", xlim = c(-1,1))
dev.off()
pdf("figures/final/supplemental/genera_CO_CR20_richness_bathymetry_coeff_null_vs_emp_withinStgs.pdf")
generate_coeff_histogram(emp_coeffs, withinStages_values, coeff = "bath", xlim = c(-0.2,0.2))
dev.off()
pdf("figures/final/supplemental/genera_CO_CR20_richness_lithology_coeff_null_vs_emp_withinStgs.pdf")
generate_coeff_histogram(emp_coeffs, withinStages_values, coeff = "lith", xlim = c(-0.5,0.5))
dev.off()
pdf("figures/final/supplemental/genera_CO_CR20_richness_reef_coeff_null_vs_emp_withinStgs.pdf")
generate_coeff_histogram(emp_coeffs, withinStages_values, coeff = "reef", xlim = c(-0.2,0.2))
dev.off()
pdf("figures/final/supplemental/genera_CO_CR20_richness_PTME_coeff_null_vs_emp_withinStgs.pdf")
generate_coeff_histogram(emp_coeffs, withinStages_values, coeff = "PTMEPostPTME", xlim = c(-1.5,0))
dev.off()


