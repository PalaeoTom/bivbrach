## 4.1.1 GLMM - genera, no covariates
## Started by TJS on 27/06/2025

#### Startup ####
## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("sjmisc", "stringr", "DHARMa", "performance", "glmmTMB", "terra", "lme4")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(sjmisc)
library(stringr)
library(DHARMa)
library(performance)
library(glmmTMB)
library(terra)
library(lme4)

#### Read in and prepare data ####
## Load data
CR20 <- read.csv("data/analysis_data/genera_NC_CR20.csv", header = T, row.names = 1)

## Read in functions for testing assumptions
source("functions/test.model.assumptions.R")
source("functions/test.spatial.autocorrelation.R")

## Check numeric entries are numeric
## CR20
for(i in c(4:8)){
  if(!is.numeric(CR20[,i])){
    CR20[,i] <- as.numeric(CR20[,i])
  }
}

## And factors are factors
## CR20
for(i in c(2,9)){
  if(!is.factor(CR20[,i])){
    CR20[,i] <- as.factor(CR20[,i])
  }
}

## Relevel PTME factor
CR20[,"PTME"] <- relevel(CR20[,"PTME"], ref = "PrePTME")

## Rename to simplify next few steps
colnames(CR20) <- c("stage_cell", "stage", "cell", "bivalve", "brachiopod", "long", "lat", "AbsLat", "PTME")

## Some counts are decimal. Round these to discrete values to work with negative binomial models
# No need to do this for bivalve counts - will be standardised
CR20$brachiopod <- round(CR20$brachiopod, digits = 0)

## Standardise predictors
CR20 <- std(CR20, CR20[,c(4, 8)])

## Drop non-standardized predictors
CR20 <- CR20[,c(-4, -8)]

## Re-do names
colnames(CR20) <- c("stage_cell", "stage", "cell", "brachiopod", "long", "lat", "PTME", "bivalve", "AbsLat")

#### GLMMs ####
## CR20
## Poisson. Link = log
CR20m1 <- glmer(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = CR20, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"))
CR20m1_diag <- test.model.assumptions(CR20m1)

## Poisson. Link = sqrt
CR20m2 <- glmer(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = CR20, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))
CR20m2_diag <- test.model.assumptions(CR20m2)

## Poisson. Link = log, zero-inflation
CR20m1zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20, family = poisson(link = "log"))
CR20m1zi_diag <- test.model.assumptions(CR20m1zi)

## Poisson. Link = sqrt, zero-inflation
CR20m2zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20, family = poisson(link = "sqrt"))
CR20m2zi_diag <- test.model.assumptions(CR20m2zi)

## nbinom2. Convergence issues with link = sqrt
CR20m3 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = CR20, family = nbinom2(link = "log"))
CR20m3_diag <- test.model.assumptions(CR20m3)

## nbinom2. Zero-inflation. Link = sqrt does not converge. Gives best results of the lot.
CR20m3zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20, family = nbinom2(link = "log"))
CR20m3zi_diag <- test.model.assumptions(CR20m3zi)
write.csv(CR20m3zi_diag, file = "data/sensitivity_testing/classicalRarefaction_bestModel_diagnostics.csv")

## nbinom1. Link = sqrt worse
CR20m4 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = CR20, family = nbinom1(link = "log"))
CR20m4_diag <- test.model.assumptions(CR20m4)

## nbinom1. Zero inflation. Link = sqrt worse.
CR20m4zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20, family = nbinom1(link = "log"))
CR20m4zi_diag <- test.model.assumptions(CR20m4zi)

## nbinom12. Link = sqrt worse.
CR20m5 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = CR20, family = nbinom12(link = "log"))
CR20m5_diag <- test.model.assumptions(CR20m5)

## nbinom12. Zero inflation. Sqrt worse.
CR20m5zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20, family = nbinom12(link = "log"))
CR20m5zi_diag <- test.model.assumptions(CR20m5zi)

## Conway-Maxwell Poisson (link = log). Link = sqrt just runs forever
CR20m6 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = CR20, family = compois(link = "log"))
CR20m6_diag <- test.model.assumptions(CR20m6)

## Conway-Maxwell Poisson (link = log), zero inflation
CR20m6zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20, family = compois(link = "log"))
CR20m6zi_diag <- test.model.assumptions(CR20m6zi)

## Generalized Poisson model
CR20m7 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = CR20, family = genpois(link = "log"))
CR20m7_diag <- test.model.assumptions(CR20m7)

## Generalized Poisson model with zero inflation
CR20m7zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20, family = genpois(link = "log"))
CR20m7zi_diag <- test.model.assumptions(CR20m7zi)

### Best model for CR20 - nbinom2, link = log, zero inflation ###


