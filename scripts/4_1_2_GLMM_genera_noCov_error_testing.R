## 4.1.2 GLMM type 1 and type 2 error testing
## Started by TJS on 27/06/2025

#### Set up ####
{
  ## Clean up
  rm(list=ls())

  ## Libraries
  library(glmmTMB)
  library(simr)
  library(stringr)
  library(parallel)
  library(sjmisc)
  library(sjPlot)
  library(cowplot)
  library(ggplot2)
  library(beepr)
  library(DHARMa)
  library(performance)

#### Read in and prepare data ####
  SQS <- read.csv("data/analysis_data/genera_NC_SQS.csv", header = T, row.names = 1)
  raw <- read.csv("data/analysis_data/genera_NC_raw.csv", header = T, row.names = 1)[,c(1:9)]
  CR20 <- read.csv("data/analysis_data/genera_NC_CR20.csv", header = T, row.names = 1)

  ## Check numeric entries are numeric
  ## raw
  for(i in c(4:8)){
    if(!is.numeric(raw[,i])){
      raw[,i] <- as.numeric(raw[,i])
    }
  }

  for(i in c(2,9)){
    if(!is.factor(raw[,i])){
      raw[,i] <- as.factor(raw[,i])
    }
  }

  ## CR20
  for(i in c(4:8)){
    if(!is.numeric(CR20[,i])){
      CR20[,i] <- as.numeric(CR20[,i])
    }
  }

  for(i in c(2,9)){
    if(!is.factor(CR20[,i])){
      CR20[,i] <- as.factor(CR20[,i])
    }
  }

  ## SQS
  for(i in c(4:8)){
    if(!is.numeric(SQS[,i])){
      SQS[,i] <- as.numeric(SQS[,i])
    }
  }

  for(i in c(2,9)){
    if(!is.factor(SQS[,i])){
      SQS[,i] <- as.factor(SQS[,i])
    }
  }

  ## Relevel PTME factor
  SQS[,"PTME"] <- relevel(SQS[,"PTME"], ref = "PrePTME")
  raw[,"PTME"] <- relevel(raw[,"PTME"], ref = "PrePTME")
  CR20[,"PTME"] <- relevel(CR20[,"PTME"], ref = "PrePTME")

  ## Rename to simplify next few steps
  colnames(SQS) <- colnames(raw) <- colnames(CR20) <- c("stage_cell", "stage", "cell", "bivalve", "brachiopod", "long", "lat", "AbsLat", "PTME")

  ## Some counts are decimal. Round these to discrete values to work with negative binomial models
  # No need to do this for bivalve counts - will be standardised
  SQS$brachiopod <- round(SQS$brachiopod, digits = 0)
  raw$brachiopod <- round(raw$brachiopod, digits = 0)
  CR20$brachiopod <- round(CR20$brachiopod, digits = 0)

  ## Standardise predictors
  SQS <- std(SQS, SQS[,c(4, 8)])
  raw <- std(raw, raw[,c(4, 8)])
  CR20 <- std(CR20, CR20[,c(4, 8)])

  ## Drop non-standardized predictors
  SQS <- SQS[,c(-4, -8)]
  raw <- raw[,c(-4, -8)]
  CR20 <- CR20[,c(-4, -8)]

  ## Re-do names
  colnames(SQS) <- colnames(raw) <- colnames(CR20) <- c("stage_cell", "stage", "cell", "brachiopod", "long", "lat", "PTME", "bivalve", "AbsLat")

  ## Define best models
  SQSmod <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = SQS, family = nbinom12(link = "sqrt"))
  rawMod<- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw, family = nbinom12(link = "sqrt"))
  CR20mod <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20, family = nbinom2(link = "log"))
}

#### Shuffling richness values for type 1 error testing - setup ####
## Load data
{
SQS_t1 <- read.csv("data/analysis_data/genera_NC_SQS.csv", header = T, row.names = 1)
raw_t1 <- read.csv("data/analysis_data/genera_NC_raw.csv", header = T, row.names = 1)[,c(1:9)]
CR20_t1 <- read.csv("data/analysis_data/genera_NC_CR20.csv", header = T, row.names = 1)

## Check numeric entries are numeric
## raw_t1
for(i in c(4:8)){
  if(!is.numeric(raw_t1[,i])){
    raw_t1[,i] <- as.numeric(raw_t1[,i])
  }
}

for(i in c(2,9)){
  if(!is.factor(raw_t1[,i])){
    raw_t1[,i] <- as.factor(raw_t1[,i])
  }
}

## CR20_t1
for(i in c(4:8)){
  if(!is.numeric(CR20_t1[,i])){
    CR20_t1[,i] <- as.numeric(CR20_t1[,i])
  }
}

for(i in c(2,9)){
  if(!is.factor(CR20_t1[,i])){
    CR20_t1[,i] <- as.factor(CR20_t1[,i])
  }
}

## SQS_t1
for(i in c(4:8)){
  if(!is.numeric(SQS_t1[,i])){
    SQS_t1[,i] <- as.numeric(SQS_t1[,i])
  }
}

for(i in c(2,9)){
  if(!is.factor(SQS_t1[,i])){
    SQS_t1[,i] <- as.factor(SQS_t1[,i])
  }
}

## Relevel PTME factor
SQS_t1[,"PTME"] <- relevel(SQS_t1[,"PTME"], ref = "PrePTME")
raw_t1[,"PTME"] <- relevel(raw_t1[,"PTME"], ref = "PrePTME")
CR20_t1[,"PTME"] <- relevel(CR20_t1[,"PTME"], ref = "PrePTME")

## Rename to simplify next few steps
colnames(SQS_t1) <- colnames(raw_t1) <- colnames(CR20_t1) <- c("stage_cell", "stage", "cell", "bivalve", "brachiopod", "long", "lat", "AbsLat", "PTME")

## Read in functions
source("functions/isolate_coeffs.R")
source("functions/compare_coeffs.R")
source("functions/isolate_and_compare_coeffs.R")
source("functions/shuffle_responses.R")

## Define export directories
fig.export.dir <- "figures/final/supplemental"
data.export.dir <- "data/sensitivity_testing"

## Create visualsRef object
axis.labels <- c("Generic bivalve richness", "PTME", "Generic bivalve richness + PTME", "Absolute latitude", "Generic bivalve richness + absolute latitude")
colours <- c("lightblue", "darkgrey", "darkblue", "pink", "purple")
term <- c("bivalve", "PTMEPostPTME", "bivalve:PTMEPostPTME", "AbsLat", "bivalve:AbsLat")
visualsRef <- data.frame(cbind("term" = term, "labels" = axis.labels, "colour" = colours))

## Run function for each combination
coeffs <- c("bivalve", "PTMEPostPTME", "bivalve:PTMEPostPTME", "AbsLat", "bivalve:AbsLat")

## Define iterations
iter = 200
}

#### Shuffling richness values for type 1 error testing - CR20 ####
##### CR20 - fix stages - shuffle brachiopods #####
{
CR20_fixed_resp <- shuffle_responses(data = CR20_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8), shuffle_predictor = F, fix_stages = T, n_cores = 8)

## Run models and record warnings
CR20_fixed_resp_M <- list()
CR20_fixed_resp_W <- list()
for(i in 1:length(CR20_fixed_resp)){
  ## Run model
  warns <- list()
  withCallingHandlers(CR20_fixed_resp_M <- append(CR20_fixed_resp_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20_fixed_resp[[i]], family = nbinom2(link = "log")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    CR20_fixed_resp_W <- append(CR20_fixed_resp_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    CR20_fixed_resp_W <- append(CR20_fixed_resp_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(CR20_fixed_resp_W), function(w){
  if(all(!is.na(CR20_fixed_resp_W[[w]]))){
    if(any(str_detect(unlist(CR20_fixed_resp_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  CR20_fixed_resp_M <- CR20_fixed_resp_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(CR20_fixed_resp_M)>100){
  CR20_fixed_resp_M <- CR20_fixed_resp_M[sample(seq(1,length(CR20_fixed_resp_M),1), 100, replace = F)]
}

## Get coefficients
CR20_fixed_resp_C <- mclapply(1:length(CR20_fixed_resp_M), mc.cores = 8, function(x){
  out <- get_model_data(CR20_fixed_resp_M[[x]], type = "est", transform = NULL)
})

## Define plot limits
CR20.plot.limits <- list("bivalve" = c(-2,0.5),
                         "PTMEPostPTME" = c(-2,0.5),
                         "bivalve:PTMEPostPTME" = c(-2,0.5),
                         "AbsLat" = c(-0.5,0.5),
                         "bivalve:AbsLat" = c(-0.5,0.5))

## CR20, within stages
title = "Comparing simulated and empirical coefficients of best model\nClassical rarefaction (sample size = 20)\n Brachiopod richness shuffled within stages"
figure.name <- "genera_noCov_CR20_brach_fixedStages_coeffs"
data.name <- "genera_noCov_CR20_brach_fixedStages_coeffs"

isolate_and_compare_coeffs(simModels = CR20_fixed_resp_C, mainModel = CR20mod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = CR20.plot.limits, visualsRef = visualsRef)
rm(CR20_fixed_resp)
rm(CR20_fixed_resp_C)
rm(CR20_fixed_resp_W)

##### CR20 - fix before/after PTME - shuffle brachiopods #####
CR20_fixedPTME_resp <- shuffle_responses(data = CR20_t1, reps = iter, stage = "PTME", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8), shuffle_predictor = F, fix_stages = T, n_cores = 8)

## Run models and record warnings
CR20_fixedPTME_resp_M <- list()
CR20_fixedPTME_resp_W <- list()
for(i in 1:length(CR20_fixedPTME_resp)){
  ## Run model
  warns <- list()
  withCallingHandlers(CR20_fixedPTME_resp_M <- append(CR20_fixedPTME_resp_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20_fixedPTME_resp[[i]], family = nbinom2(link = "log")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    CR20_fixedPTME_resp_W <- append(CR20_fixedPTME_resp_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    CR20_fixedPTME_resp_W <- append(CR20_fixedPTME_resp_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(CR20_fixedPTME_resp_W), function(w){
  if(all(!is.na(CR20_fixedPTME_resp_W[[w]]))){
    if(any(str_detect(unlist(CR20_fixedPTME_resp_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  CR20_fixedPTME_resp_M <- CR20_fixedPTME_resp_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(CR20_fixedPTME_resp_M)>100){
  CR20_fixedPTME_resp_M <- CR20_fixedPTME_resp_M[sample(seq(1,length(CR20_fixedPTME_resp_M),1), 100, replace = F)]
}

## Get coefficients
CR20_fixedPTME_resp_C <- mclapply(1:length(CR20_fixedPTME_resp_M), mc.cores = 8, function(x){
  out <- get_model_data(CR20_fixedPTME_resp_M[[x]], type = "est", transform = NULL)
})

## Define plot limits
CR20.plot.limits <- list("bivalve" = c(-2,0.5),
                         "PTMEPostPTME" = c(-2,0.5),
                         "bivalve:PTMEPostPTME" = c(-2,0.5),
                         "AbsLat" = c(-0.5,0.5),
                         "bivalve:AbsLat" = c(-0.5,0.5))

## CR20, within stages
title = "Comparing simulated and empirical coefficients of best model\nClassical rarefaction (sample size = 20)\n Brachiopod richness shuffled across Palaeozoic and Mesozoic-Cenozoic"
figure.name <- "genera_noCov_CR20_brach_fixedPTME_coeffs"
data.name <- "genera_noCov_CR20_brach_fixedPTME_coeffs"

isolate_and_compare_coeffs(simModels = CR20_fixedPTME_resp_C, mainModel = CR20mod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = CR20.plot.limits, visualsRef = visualsRef)
rm(CR20_fixedPTME_resp)
rm(CR20_fixedPTME_resp_C)
rm(CR20_fixedPTME_resp_W)

##### CR20 - fix before/after PTME - shuffle brachiopods and bivalves #####
## Shuffle brachiopods and bivalves across Palaeozoic and Mesozoic-Cenozoic
CR20_fixedPTME_both <- shuffle_responses(data = CR20_t1, reps = iter, stage = "PTME", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8), shuffle_predictor = T, fix_stages = T, n_cores = 8)

## Run models and record warnings
CR20_fixedPTME_both_M <- list()
CR20_fixedPTME_both_W <- list()
for(i in 1:length(CR20_fixedPTME_both)){
  ## Run model
  warns <- list()
  withCallingHandlers(CR20_fixedPTME_both_M <- append(CR20_fixedPTME_both_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20_fixedPTME_both[[i]], family = nbinom2(link = "log")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    CR20_fixedPTME_both_W <- append(CR20_fixedPTME_both_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    CR20_fixedPTME_both_W <- append(CR20_fixedPTME_both_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(CR20_fixedPTME_both_W), function(w){
  if(all(!is.na(CR20_fixedPTME_both_W[[w]]))){
    if(any(str_detect(unlist(CR20_fixedPTME_both_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  CR20_fixedPTME_both_M <- CR20_fixedPTME_both_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(CR20_fixedPTME_both_M)>100){
  CR20_fixedPTME_both_M <- CR20_fixedPTME_both_M[sample(seq(1,length(CR20_fixedPTME_both_M),1), 100, replace = F)]
}

## Get coefficients
CR20_fixedPTME_both_C <- mclapply(1:length(CR20_fixedPTME_both_M), mc.cores = 8, function(x){
  out <- get_model_data(CR20_fixedPTME_both_M[[x]], type = "est", transform = NULL)
})

## Define plot limits
CR20.plot.limits <- list("bivalve" = c(-2,0.5),
                         "PTMEPostPTME" = c(-2,0.5),
                         "bivalve:PTMEPostPTME" = c(-2,0.5),
                         "AbsLat" = c(-0.5,0.5),
                         "bivalve:AbsLat" = c(-0.5,0.5))

## CR20, within stages
title = "Comparing simulated and empirical coefficients of best model\nClassical rarefaction (sample size = 20)\n Brachiopod and bivalve richness shuffled across Palaeozoic and Mesozoic-Cenozoic"
figure.name <- "genera_noCov_CR20_bivNbrach_fixedPTME_coeffs"
data.name <- "genera_noCov_CR20_bivNbrach_fixedPTME_coeffs"

isolate_and_compare_coeffs(simModels = CR20_fixedPTME_both_C, mainModel = CR20mod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = CR20.plot.limits, visualsRef = visualsRef)
rm(CR20_fixedPTME_both)
rm(CR20_fixedPTME_both_C)
rm(CR20_fixedPTME_both_W)

##### CR20 - fix stages - shuffle brachiopods and bivalves #####
CR20_fixed_both <- shuffle_responses(data = CR20_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", c(4, 8), shuffle_predictor = T, fix_stages = T, n_cores = 8)

## Run models and record warnings
CR20_fixed_both_M <- list()
CR20_fixed_both_W <- list()
for(i in 1:length(CR20_fixed_both)){
  ## Run model
  warns <- list()
  withCallingHandlers(CR20_fixed_both_M <- append(CR20_fixed_both_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20_fixed_both[[i]], family = nbinom2(link = "log")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    CR20_fixed_both_W <- append(CR20_fixed_both_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    CR20_fixed_both_W <- append(CR20_fixed_both_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(CR20_fixed_both_W), function(w){
  if(all(!is.na(CR20_fixed_both_W[[w]]))){
    if(any(str_detect(unlist(CR20_fixed_both_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  CR20_fixed_both_M <- CR20_fixed_both_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(CR20_fixed_both_M)>100){
  CR20_fixed_both_M <- CR20_fixed_both_M[sample(seq(1,length(CR20_fixed_both_M),1), 100, replace = F)]
}

## Get coefficients
CR20_fixed_both_C <- mclapply(1:length(CR20_fixed_both_M), mc.cores = 8, function(x){
  out <- get_model_data(CR20_fixed_both_M[[x]], type = "est", transform = NULL)
})

## CR20, within stages
title = "Comparing simulated and empirical coefficients of best model\nClassical rarefaction (sample size = 20)\n Brachiopod and bivalve richness shuffled within stages"
figure.name <- "genera_noCov_CR20_bivNbrach_fixedStages_coeffs"
data.name <- "genera_noCov_CR20_bivNbrach_fixedStages_coeffs"

isolate_and_compare_coeffs(simModels = CR20_fixed_both_C, mainModel = CR20mod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = CR20.plot.limits, visualsRef = visualsRef)
rm(CR20_fixed_both)
rm(CR20_fixed_both_C)
rm(CR20_fixed_both_W)

##### CR20 - fluid stages - shuffle brachiopods #####
CR20_fluid_resp <- shuffle_responses(data = CR20_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", c(4, 8), shuffle_predictor = F, fix_stages = F, n_cores = 8)

## Run models and record warnings
CR20_fluid_resp_M <- list()
CR20_fluid_resp_W <- list()
for(i in 1:length(CR20_fluid_resp)){
  ## Run model
  warns <- list()
  withCallingHandlers(CR20_fluid_resp_M <- append(CR20_fluid_resp_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20_fluid_resp[[i]], family = nbinom2(link = "log")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    CR20_fluid_resp_W <- append(CR20_fluid_resp_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    CR20_fluid_resp_W <- append(CR20_fluid_resp_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(CR20_fluid_resp_W), function(w){
  if(all(!is.na(CR20_fluid_resp_W[[w]]))){
    if(any(str_detect(unlist(CR20_fluid_resp_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  CR20_fluid_resp_M <- CR20_fluid_resp_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(CR20_fluid_resp_M)>100){
  CR20_fluid_resp_M <- CR20_fluid_resp_M[sample(seq(1,length(CR20_fluid_resp_M),1), 100, replace = F)]
}

## Get coefficients
CR20_fluid_resp_C <- mclapply(1:length(CR20_fluid_resp_M), mc.cores = 8, function(x){
  out <- get_model_data(CR20_fluid_resp_M[[x]], type = "est", transform = NULL)
})

## CR20, within stages
title = "Comparing simulated and empirical coefficients of best model\nClassical rarefaction (sample size = 20)\n Brachiopod richness shuffled across stages"
figure.name <- "genera_noCov_CR20_brach_noRestrict_coeffs"
data.name <- "genera_noCov_CR20_brach_noRestrict_coeffs"

isolate_and_compare_coeffs(simModels = CR20_fluid_resp_C, mainModel = CR20mod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = CR20.plot.limits, visualsRef = visualsRef)
rm(CR20_fluid_resp)
rm(CR20_fluid_resp_C)
rm(CR20_fluid_resp_W)

##### CR20 - fluid stages - shuffle brachiopods and bivalves #####
CR20_fluid_both <- shuffle_responses(data = CR20_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", c(4, 8), shuffle_predictor = T, fix_stages = F, n_cores = 8)

## Run models and record warnings
CR20_fluid_both_M <- list()
CR20_fluid_both_W <- list()
for(i in 1:length(CR20_fluid_both)){
  ## Run model
  warns <- list()
  withCallingHandlers(CR20_fluid_both_M <- append(CR20_fluid_both_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20_fluid_both[[i]], family = nbinom2(link = "log")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    CR20_fluid_both_W <- append(CR20_fluid_both_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    CR20_fluid_both_W <- append(CR20_fluid_both_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(CR20_fluid_both_W), function(w){
  if(all(!is.na(CR20_fluid_both_W[[w]]))){
    if(any(str_detect(unlist(CR20_fluid_both_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  CR20_fluid_both_M <- CR20_fluid_both_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(CR20_fluid_both_M)>100){
  CR20_fluid_both_M <- CR20_fluid_both_M[sample(seq(1,length(CR20_fluid_both_M),1), 100, replace = F)]
}

## Get coefficients
CR20_fluid_both_C <- mclapply(1:length(CR20_fluid_both_M), mc.cores = 8, function(x){
  out <- get_model_data(CR20_fluid_both_M[[x]], type = "est", transform = NULL)
})

## CR20, within stages
title = "Comparing simulated and empirical coefficients of best model\nClassical rarefaction (sample size = 20)\n Brachiopod and bivalve richness shuffled across stages"
figure.name <- "genera_noCov_CR20_bivNbrach_noRestrict_coeffs"
data.name <- "genera_noCov_CR20_bivNbrach_noRestrict_coeffs"

isolate_and_compare_coeffs(simModels = CR20_fluid_both_C, mainModel = CR20mod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = CR20.plot.limits, visualsRef = visualsRef)
rm(CR20_fluid_both)
rm(CR20_fluid_both_C)
rm(CR20_fluid_both_W)
}
#### Shuffling richness values for type 1 error testing - raw ####
##### raw - fixed befoew/after PTME - shuffle brachiopods #####
{
raw_fixedPTME_resp <- shuffle_responses(data = raw_t1, reps = iter, stage = "PTME", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8), shuffle_predictor = F, fix_stages = T, n_cores = 8)

## Run models and record warnings
raw_fixedPTME_resp_M <- list()
raw_fixedPTME_resp_W <- list()
for(i in 1:length(raw_fixedPTME_resp)){
  ## Run model
  warns <- list()
  withCallingHandlers(raw_fixedPTME_resp_M <- append(raw_fixedPTME_resp_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw_fixedPTME_resp[[i]], family = nbinom2(link = "log")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    raw_fixedPTME_resp_W <- append(raw_fixedPTME_resp_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    raw_fixedPTME_resp_W <- append(raw_fixedPTME_resp_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(raw_fixedPTME_resp_W), function(w){
  if(all(!is.na(raw_fixedPTME_resp_W[[w]]))){
    if(any(str_detect(unlist(raw_fixedPTME_resp_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  raw_fixedPTME_resp_M <- raw_fixedPTME_resp_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(raw_fixedPTME_resp_M)>100){
  raw_fixedPTME_resp_M <- raw_fixedPTME_resp_M[sample(seq(1,length(raw_fixedPTME_resp_M),1), 100, replace = F)]
}

## Get coefficients
raw_fixedPTME_resp_C <- mclapply(1:length(raw_fixedPTME_resp_M), mc.cores = 8, function(x){
  out <- get_model_data(raw_fixedPTME_resp_M[[x]], type = "est", transform = NULL)
})

## Define plot limits
raw.plot.limits <- list("bivalve" = c(-5,5),
                        "PTMEPostPTME" = c(-5,5),
                        "bivalve:PTMEPostPTME" = c(-5,5),
                        "AbsLat" = c(-3,3),
                        "bivalve:AbsLat" = c(-3,3))

## raw, within stages
title = "Comparing simulated and empirical coefficients of best model\nRaw richness\n Brachiopod richness shuffled across Palaeozoic and Mesozoic-Cenozoic"
figure.name <- "genera_noCov_raw_brach_fixedPTME_coeffs"
data.name <- "genera_noCov_raw_brach_fixedPTME_coeffs"

isolate_and_compare_coeffs(simModels = raw_fixedPTME_resp_C, mainModel = rawmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = raw.plot.limits, visualsRef = visualsRef)
rm(raw_fixedPTME_resp)
rm(raw_fixedPTME_resp_C)
rm(raw_fixedPTME_resp_W)

##### raw - fixed before/after PTME - shuffle brachiopods and bivalves #####
raw_fixedPTME_both <- shuffle_responses(data = raw_t1, reps = iter, stage = "PTME", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8), shuffle_predictor = T, fix_stages = T, n_cores = 8)

## Run models and record warnings
raw_fixedPTME_both_M <- list()
raw_fixedPTME_both_W <- list()
for(i in 1:length(raw_fixedPTME_both)){
  ## Run model
  warns <- list()
  withCallingHandlers(raw_fixedPTME_both_M <- append(raw_fixedPTME_both_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw_fixedPTME_both[[i]], family = nbinom2(link = "log")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    raw_fixedPTME_both_W <- append(raw_fixedPTME_both_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    raw_fixedPTME_both_W <- append(raw_fixedPTME_both_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(raw_fixedPTME_both_W), function(w){
  if(all(!is.na(raw_fixedPTME_both_W[[w]]))){
    if(any(str_detect(unlist(raw_fixedPTME_both_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  raw_fixedPTME_both_M <- raw_fixedPTME_both_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(raw_fixedPTME_both_M)>100){
  raw_fixedPTME_both_M <- raw_fixedPTME_both_M[sample(seq(1,length(raw_fixedPTME_both_M),1), 100, replace = F)]
}

## Get coefficients
raw_fixedPTME_both_C <- mclapply(1:length(raw_fixedPTME_both_M), mc.cores = 8, function(x){
  out <- get_model_data(raw_fixedPTME_both_M[[x]], type = "est", transform = NULL)
})

## Define plot limits
raw.plot.limits <- list("bivalve" = c(-5,5),
                        "PTMEPostPTME" = c(-5,5),
                        "bivalve:PTMEPostPTME" = c(-5,5),
                        "AbsLat" = c(-3,3),
                        "bivalve:AbsLat" = c(-3,3))

## raw, within stages
title = "Comparing simulated and empirical coefficients of best model\nRaw richness\n Brachiopod and bivalve richness shuffled across Palaeozoic and Mesozoic-Cenozoic"
figure.name <- "genera_noCov_raw_bivNbrach_fixedPTME_coeffs"
data.name <- "genera_noCov_raw_bivNbrach_fixedPTME_coeffs"

isolate_and_compare_coeffs(simModels = raw_fixedPTME_both_C, mainModel = rawmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = raw.plot.limits, visualsRef = visualsRef)
rm(raw_fixedPTME_both)
rm(raw_fixedPTME_both_C)
rm(raw_fixedPTME_both_W)

##### raw - fixed stages - shuffle brachiopods #####
raw_fixed_resp <- shuffle_responses(data = raw_t1, reps = 200, stage = "stage", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8), shuffle_predictor = F, fix_stages = T, n_cores = 8)

## Run models and record warnings
raw_fixed_resp_M <- list()
raw_fixed_resp_W <- list()
for(i in 1:length(raw_fixed_resp)){
  ## Run model
  warns <- list()
  withCallingHandlers(raw_fixed_resp_M <- append(raw_fixed_resp_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw_fixed_resp[[i]], family = nbinom12(link = "sqrt")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    raw_fixed_resp_W <- append(raw_fixed_resp_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    raw_fixed_resp_W <- append(raw_fixed_resp_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(raw_fixed_resp_W), function(w){
  if(all(!is.na(raw_fixed_resp_W[[w]]))){
    if(any(str_detect(unlist(raw_fixed_resp_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  raw_fixed_resp_M <- raw_fixed_resp_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(raw_fixed_resp_M)>100){
  raw_fixed_resp_M <- raw_fixed_resp_M[sample(seq(1,length(raw_fixed_resp_M),1), 100, replace = F)]
}

## Get coefficients
raw_fixed_resp_C <- mclapply(1:length(raw_fixed_resp_M), mc.cores = 8, function(x){
  out <- get_model_data(raw_fixed_resp_M[[x]], type = "est", transform = NULL)
})

## Define plot limits
raw.plot.limits <- list("bivalve" = c(-5,5),
                        "PTMEPostPTME" = c(-5,5),
                        "bivalve:PTMEPostPTME" = c(-5,5),
                        "AbsLat" = c(-3,3),
                        "bivalve:AbsLat" = c(-3,3))

## raw, within stages
title = "Comparing simulated and empirical coefficients of best model\nRaw richness\n Brachiopod richness shuffled within stages"
figure.name <- "genera_noCov_raw_brach_fixedStages_coeffs"
data.name <- "genera_noCov_raw_brach_fixedStages_coeffs"

isolate_and_compare_coeffs(simModels = raw_fixed_resp_C, mainModel = rawMod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = raw.plot.limits, visualsRef = visualsRef)
rm(raw_fixed_resp)
rm(raw_fixed_resp_C)
rm(raw_fixed_resp_W)

##### raw - fixed stages - shuffle brachiopods and bivalves #####
raw_fixed_both <- shuffle_responses(data = raw_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", c(4, 8), shuffle_predictor = T, fix_stages = T, n_cores = 8)

## Run models and record warnings
raw_fixed_both_M <- list()
raw_fixed_both_W <- list()
for(i in 1:length(raw_fixed_both)){
  ## Run model
  warns <- list()
  withCallingHandlers(raw_fixed_both_M <- append(raw_fixed_both_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw_fixed_both[[i]], family = nbinom12(link = "sqrt")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    raw_fixed_both_W <- append(raw_fixed_both_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    raw_fixed_both_W <- append(raw_fixed_both_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(raw_fixed_both_W), function(w){
  if(all(!is.na(raw_fixed_both_W[[w]]))){
    if(any(str_detect(unlist(raw_fixed_both_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  raw_fixed_both_M <- raw_fixed_both_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(raw_fixed_both_M)>100){
  raw_fixed_both_M <- raw_fixed_both_M[sample(seq(1,length(raw_fixed_both_M),1), 100, replace = F)]
}

## Get coefficients
raw_fixed_both_C <- mclapply(1:length(raw_fixed_both_M), mc.cores = 8, function(x){
  out <- get_model_data(raw_fixed_both_M[[x]], type = "est", transform = NULL)
})

## raw, within stages
title = "Comparing simulated and empirical coefficients of best model\nRaw richness\n Brachiopod and bivalve richness shuffled within stages"
figure.name <- "genera_noCov_raw_bivNbrach_fixedStages_coeffs"
data.name <- "genera_noCov_raw_bivNbrach_fixedStages_coeffs"

isolate_and_compare_coeffs(simModels = raw_fixed_both_C, mainModel = rawMod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = raw.plot.limits, visualsRef = visualsRef)
rm(raw_fixed_both)
rm(raw_fixed_both_C)
rm(raw_fixed_both_W)

##### raw - fluid stages - shuffle brachiopods #####
raw_fluid_resp <- shuffle_responses(data = raw_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", c(4, 8), shuffle_predictor = F, fix_stages = F, n_cores = 8)

## Run models and record warnings
raw_fluid_resp_M <- list()
raw_fluid_resp_W <- list()
for(i in 1:length(raw_fluid_resp)){
  ## Run model
  warns <- list()
  withCallingHandlers(raw_fluid_resp_M <- append(raw_fluid_resp_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw_fluid_resp[[i]], family = nbinom12(link = "sqrt")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    raw_fluid_resp_W <- append(raw_fluid_resp_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    raw_fluid_resp_W <- append(raw_fluid_resp_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(raw_fluid_resp_W), function(w){
  if(all(!is.na(raw_fluid_resp_W[[w]]))){
    if(any(str_detect(unlist(raw_fluid_resp_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  raw_fluid_resp_M <- raw_fluid_resp_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(raw_fluid_resp_M)>100){
  raw_fluid_resp_M <- raw_fluid_resp_M[sample(seq(1,length(raw_fluid_resp_M),1), 100, replace = F)]
}

## Get coefficients
raw_fluid_resp_C <- mclapply(1:length(raw_fluid_resp_M), mc.cores = 8, function(x){
  out <- get_model_data(raw_fluid_resp_M[[x]], type = "est", transform = NULL)
})

## raw, within stages
title = "Comparing simulated and empirical coefficients of best model\nRaw richness\n Brachiopod richness shuffled across stages"
figure.name <- "genera_noCov_raw_brach_noRestrict_coeffs"
data.name <- "genera_noCov_raw_brach_noRestrict_coeffs"

isolate_and_compare_coeffs(simModels = raw_fluid_resp_C, mainModel = rawMod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = raw.plot.limits, visualsRef = visualsRef)
rm(raw_fluid_resp)
rm(raw_fluid_resp_C)
rm(raw_fluid_resp_W)

##### raw - fluid stages - shuffle brachiopods and bivalves #####
raw_fluid_both <- shuffle_responses(data = raw_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", c(4, 8), shuffle_predictor = T, fix_stages = F, n_cores = 8)

## Run models and record warnings
raw_fluid_both_M <- list()
raw_fluid_both_W <- list()
for(i in 1:length(raw_fluid_both)){
  ## Run model
  warns <- list()
  withCallingHandlers(raw_fluid_both_M <- append(raw_fluid_both_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw_fluid_both[[i]], family = nbinom12(link = "sqrt")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    raw_fluid_both_W <- append(raw_fluid_both_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    raw_fluid_both_W <- append(raw_fluid_both_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(raw_fluid_both_W), function(w){
  if(all(!is.na(raw_fluid_both_W[[w]]))){
    if(any(str_detect(unlist(raw_fluid_both_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  raw_fluid_both_M <- raw_fluid_both_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(raw_fluid_both_M)>100){
  raw_fluid_both_M <- raw_fluid_both_M[sample(seq(1,length(raw_fluid_both_M),1), 100, replace = F)]
}

## Get coefficients
raw_fluid_both_C <- mclapply(1:length(raw_fluid_both_M), mc.cores = 8, function(x){
  out <- get_model_data(raw_fluid_both_M[[x]], type = "est", transform = NULL)
})

## raw, within stages
title = "Comparing simulated and empirical coefficients of best model\nRaw richness\n Brachiopod and bivalve richness shuffled across stages"
figure.name <- "genera_noCov_raw_bivNbrach_noRestrict_coeffs"
data.name <- "genera_noCov_raw_bivNbrach_noRestrict_coeffs"

isolate_and_compare_coeffs(simModels = raw_fluid_both_C, mainModel = rawMod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = raw.plot.limits, visualsRef = visualsRef)
rm(raw_fluid_both)
rm(raw_fluid_both_C)
rm(raw_fluid_both_W)
}
#### Shuffling richness values for type 1 error testing - SQS ####
##### SQS - fix stages - shuffle brachiopods #####
{
SQS_fixed_resp <- shuffle_responses(data = SQS_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8), shuffle_predictor = F, fix_stages = T, n_cores = 8)

## Run models and record warnings
SQS_fixed_resp_M <- list()
SQS_fixed_resp_W <- list()
for(i in 1:length(SQS_fixed_resp)){
  ## Run model
  warns <- list()
  withCallingHandlers(SQS_fixed_resp_M <- append(SQS_fixed_resp_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = SQS_fixed_resp[[i]], family = nbinom12(link = "sqrt")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    SQS_fixed_resp_W <- append(SQS_fixed_resp_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    SQS_fixed_resp_W <- append(SQS_fixed_resp_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(SQS_fixed_resp_W), function(w){
  if(all(!is.na(SQS_fixed_resp_W[[w]]))){
    if(any(str_detect(unlist(SQS_fixed_resp_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  SQS_fixed_resp_M <- SQS_fixed_resp_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(SQS_fixed_resp_M)>100){
  SQS_fixed_resp_M <- SQS_fixed_resp_M[sample(seq(1,length(SQS_fixed_resp_M),1), 100, replace = F)]
}

## Get coefficients
SQS_fixed_resp_C <- mclapply(1:length(SQS_fixed_resp_M), mc.cores = 8, function(x){
  out <- get_model_data(SQS_fixed_resp_M[[x]], type = "est", transform = NULL)
})

## Define plot limits
SQS.plot.limits <- list("bivalve" = c(-2,0.5),
                         "PTMEPostPTME" = c(-2,0.5),
                         "bivalve:PTMEPostPTME" = c(-2,0.5),
                         "AbsLat" = c(-0.5,0.5),
                         "bivalve:AbsLat" = c(-0.5,0.5))

## SQS, within stages
title = "Comparing simulated and empirical coefficients of best model\nSQS (quorum = 0.7)\n Brachiopod richness shuffled within stages"
figure.name <- "genera_noCov_SQS_brach_fixedStages_coeffs"
data.name <- "genera_noCov_SQS_brach_fixedStages_coeffs"

isolate_and_compare_coeffs(simModels = SQS_fixed_resp_C, mainModel = SQSmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = SQS.plot.limits, visualsRef = visualsRef)
rm(SQS_fixed_resp)
rm(SQS_fixed_resp_C)
rm(SQS_fixed_resp_W)

##### SQS - fix stages - shuffle brachiopods and bivalves #####
SQS_fixed_both <- shuffle_responses(data = SQS_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", c(4, 8), shuffle_predictor = T, fix_stages = T, n_cores = 8)

## Run models and record warnings
SQS_fixed_both_M <- list()
SQS_fixed_both_W <- list()
for(i in 1:length(SQS_fixed_both)){
  ## Run model
  warns <- list()
  withCallingHandlers(SQS_fixed_both_M <- append(SQS_fixed_both_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = SQS_fixed_both[[i]], family = nbinom12(link = "sqrt")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    SQS_fixed_both_W <- append(SQS_fixed_both_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    SQS_fixed_both_W <- append(SQS_fixed_both_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(SQS_fixed_both_W), function(w){
  if(all(!is.na(SQS_fixed_both_W[[w]]))){
    if(any(str_detect(unlist(SQS_fixed_both_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  SQS_fixed_both_M <- SQS_fixed_both_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(SQS_fixed_both_M)>100){
  SQS_fixed_both_M <- SQS_fixed_both_M[sample(seq(1,length(SQS_fixed_both_M),1), 100, replace = F)]
}

## Get coefficients
SQS_fixed_both_C <- mclapply(1:length(SQS_fixed_both_M), mc.cores = 8, function(x){
  out <- get_model_data(SQS_fixed_both_M[[x]], type = "est", transform = NULL)
})

## SQS, within stages
title = "Comparing simulated and empirical coefficients of best model\nSQS (quorum = 0.7)\n Brachiopod and bivalve richness shuffled within stages"
figure.name <- "genera_noCov_SQS_bivNbrach_fixedStages_coeffs"
data.name <- "genera_noCov_SQS_bivNbrach_fixedStages_coeffs"

isolate_and_compare_coeffs(simModels = SQS_fixed_both_C, mainModel = SQSmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = SQS.plot.limits, visualsRef = visualsRef)
rm(SQS_fixed_both)
rm(SQS_fixed_both_C)
rm(SQS_fixed_both_W)

##### SQS - fix before/after PTME - shuffle brachiopods #####
SQS_fixedPTME_resp <- shuffle_responses(data = SQS_t1, reps = iter, stage = "PTME", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8), shuffle_predictor = F, fix_stages = T, n_cores = 8)

## Run models and record warnings
SQS_fixedPTME_resp_M <- list()
SQS_fixedPTME_resp_W <- list()
for(i in 1:length(SQS_fixedPTME_resp)){
  ## Run model
  warns <- list()
  withCallingHandlers(SQS_fixedPTME_resp_M <- append(SQS_fixedPTME_resp_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = SQS_fixedPTME_resp[[i]], family = nbinom12(link = "sqrt")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    SQS_fixedPTME_resp_W <- append(SQS_fixedPTME_resp_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    SQS_fixedPTME_resp_W <- append(SQS_fixedPTME_resp_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(SQS_fixedPTME_resp_W), function(w){
  if(all(!is.na(SQS_fixedPTME_resp_W[[w]]))){
    if(any(str_detect(unlist(SQS_fixedPTME_resp_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  SQS_fixedPTME_resp_M <- SQS_fixedPTME_resp_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(SQS_fixedPTME_resp_M)>100){
  SQS_fixedPTME_resp_M <- SQS_fixedPTME_resp_M[sample(seq(1,length(SQS_fixedPTME_resp_M),1), 100, replace = F)]
}

## Get coefficients
SQS_fixedPTME_resp_C <- mclapply(1:length(SQS_fixedPTME_resp_M), mc.cores = 8, function(x){
  out <- get_model_data(SQS_fixedPTME_resp_M[[x]], type = "est", transform = NULL)
})

## Define plot limits
SQS.plot.limits <- list("bivalve" = c(-2,0.5),
                         "PTMEPostPTME" = c(-2,0.5),
                         "bivalve:PTMEPostPTME" = c(-2,0.5),
                         "AbsLat" = c(-0.5,0.5),
                         "bivalve:AbsLat" = c(-0.5,0.5))

## SQS, within stages
title = "Comparing simulated and empirical coefficients of best model\nSQS (quorum = 0.7)\n Brachiopod richness shuffled across Palaeozoic and Mesozoic-Cenozoic"
figure.name <- "genera_noCov_SQS_brach_fixedPTME_coeffs"
data.name <- "genera_noCov_SQS_brach_fixedPTME_coeffs"

isolate_and_compare_coeffs(simModels = SQS_fixedPTME_resp_C, mainModel = SQSmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = SQS.plot.limits, visualsRef = visualsRef)
rm(SQS_fixedPTME_resp)
rm(SQS_fixedPTME_resp_C)
rm(SQS_fixedPTME_resp_W)

##### SQS - fix before/after PTME - shuffle brachiopods and bivalves #####
## Shuffle brachiopods and bivalves across Palaeozoic and Mesozoic-Cenozoic
SQS_fixedPTME_both <- shuffle_responses(data = SQS_t1, reps = iter, stage = "PTME", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8), shuffle_predictor = T, fix_stages = T, n_cores = 8)

## Run models and record warnings
SQS_fixedPTME_both_M <- list()
SQS_fixedPTME_both_W <- list()
for(i in 1:length(SQS_fixedPTME_both)){
  ## Run model
  warns <- list()
  withCallingHandlers(SQS_fixedPTME_both_M <- append(SQS_fixedPTME_both_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = SQS_fixedPTME_both[[i]], family = nbinom12(link = "sqrt")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    SQS_fixedPTME_both_W <- append(SQS_fixedPTME_both_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    SQS_fixedPTME_both_W <- append(SQS_fixedPTME_both_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(SQS_fixedPTME_both_W), function(w){
  if(all(!is.na(SQS_fixedPTME_both_W[[w]]))){
    if(any(str_detect(unlist(SQS_fixedPTME_both_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  SQS_fixedPTME_both_M <- SQS_fixedPTME_both_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(SQS_fixedPTME_both_M)>100){
  SQS_fixedPTME_both_M <- SQS_fixedPTME_both_M[sample(seq(1,length(SQS_fixedPTME_both_M),1), 100, replace = F)]
}

## Get coefficients
SQS_fixedPTME_both_C <- mclapply(1:length(SQS_fixedPTME_both_M), mc.cores = 8, function(x){
  out <- get_model_data(SQS_fixedPTME_both_M[[x]], type = "est", transform = NULL)
})

## Define plot limits
SQS.plot.limits <- list("bivalve" = c(-2,0.5),
                         "PTMEPostPTME" = c(-2,0.5),
                         "bivalve:PTMEPostPTME" = c(-2,0.5),
                         "AbsLat" = c(-0.5,0.5),
                         "bivalve:AbsLat" = c(-0.5,0.5))

## SQS, within stages
title = "Comparing simulated and empirical coefficients of best model\nSQS (quorum = 0.7)\n Brachiopod and bivalve richness shuffled across Palaeozoic and Mesozoic-Cenozoic"
figure.name <- "genera_noCov_SQS_bivNbrach_fixedPTME_coeffs"
data.name <- "genera_noCov_SQS_bivNbrach_fixedPTME_coeffs"

isolate_and_compare_coeffs(simModels = SQS_fixedPTME_both_C, mainModel = SQSmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = SQS.plot.limits, visualsRef = visualsRef)
rm(SQS_fixedPTME_both)
rm(SQS_fixedPTME_both_C)
rm(SQS_fixedPTME_both_W)

##### SQS - fluid stages - shuffle brachiopods #####
SQS_fluid_resp <- shuffle_responses(data = SQS_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", c(4, 8), shuffle_predictor = F, fix_stages = F, n_cores = 8)

## Run models and record warnings
SQS_fluid_resp_M <- list()
SQS_fluid_resp_W <- list()
for(i in 1:length(SQS_fluid_resp)){
  ## Run model
  warns <- list()
  withCallingHandlers(SQS_fluid_resp_M <- append(SQS_fluid_resp_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = SQS_fluid_resp[[i]], family = nbinom12(link = "sqrt")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    SQS_fluid_resp_W <- append(SQS_fluid_resp_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    SQS_fluid_resp_W <- append(SQS_fluid_resp_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(SQS_fluid_resp_W), function(w){
  if(all(!is.na(SQS_fluid_resp_W[[w]]))){
    if(any(str_detect(unlist(SQS_fluid_resp_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  SQS_fluid_resp_M <- SQS_fluid_resp_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(SQS_fluid_resp_M)>100){
  SQS_fluid_resp_M <- SQS_fluid_resp_M[sample(seq(1,length(SQS_fluid_resp_M),1), 100, replace = F)]
}

## Get coefficients
SQS_fluid_resp_C <- mclapply(1:length(SQS_fluid_resp_M), mc.cores = 8, function(x){
  out <- get_model_data(SQS_fluid_resp_M[[x]], type = "est", transform = NULL)
})

## SQS, within stages
title = "Comparing simulated and empirical coefficients of best model\nSQS (quorum = 0.7)\n Brachiopod richness shuffled across stages"
figure.name <- "genera_noCov_SQS_brach_noRestrict_coeffs"
data.name <- "genera_noCov_SQS_brach_noRestrict_coeffs"

isolate_and_compare_coeffs(simModels = SQS_fluid_resp_C, mainModel = SQSmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = SQS.plot.limits, visualsRef = visualsRef)
rm(SQS_fluid_resp)
rm(SQS_fluid_resp_C)
rm(SQS_fluid_resp_W)

##### SQS - fluid stages - shuffle brachiopods and bivalves #####
SQS_fluid_both <- shuffle_responses(data = SQS_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", c(4, 8), shuffle_predictor = T, fix_stages = F, n_cores = 8)

## Run models and record warnings
SQS_fluid_both_M <- list()
SQS_fluid_both_W <- list()
for(i in 1:length(SQS_fluid_both)){
  ## Run model
  warns <- list()
  withCallingHandlers(SQS_fluid_both_M <- append(SQS_fluid_both_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = SQS_fluid_both[[i]], family = nbinom12(link = "sqrt")))), warning = function(warn) {warns <<- append(warns, warn)})
  if(length(warns)==0){
    SQS_fluid_both_W <- append(SQS_fluid_both_W, NA)
  } else {
    warns <- warns[which(names(warns)=="message")]
    SQS_fluid_both_W <- append(SQS_fluid_both_W, list(warns))
  }
}

## Identify warnings with "model convergence error" strings and convert these to NA.
drop.ind <- sapply(1:length(SQS_fluid_both_W), function(w){
  if(all(!is.na(SQS_fluid_both_W[[w]]))){
    if(any(str_detect(unlist(SQS_fluid_both_W[[w]]), "Model convergence problem"))){
      out <- F
    } else {
      out <- T
    }
  } else {
    out <- T
  }
  return(out)
})

## If any F in drop.ind, apply
if(any(!drop.ind)){
  SQS_fluid_both_M <- SQS_fluid_both_M[drop.ind]
}

## If length is greater than 100, randomly sample down to 100.
if(length(SQS_fluid_both_M)>100){
  SQS_fluid_both_M <- SQS_fluid_both_M[sample(seq(1,length(SQS_fluid_both_M),1), 100, replace = F)]
}

## Get coefficients
SQS_fluid_both_C <- mclapply(1:length(SQS_fluid_both_M), mc.cores = 8, function(x){
  out <- get_model_data(SQS_fluid_both_M[[x]], type = "est", transform = NULL)
})

## SQS, within stages
title = "Comparing simulated and empirical coefficients of best model\nSQS (quorum = 0.7)\n Brachiopod and bivalve richness shuffled across stages"
figure.name <- "genera_noCov_SQS_bivNbrach_noRestrict_coeffs"
data.name <- "genera_noCov_SQS_bivNbrach_noRestrict_coeffs"

isolate_and_compare_coeffs(simModels = SQS_fluid_both_C, mainModel = SQSmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = SQS.plot.limits, visualsRef = visualsRef)
rm(SQS_fluid_both)
rm(SQS_fluid_both_C)
rm(SQS_fluid_both_W)

}
#### Spot testing model assumptions ####
## Spot checking models
source("functions/test.model.assumptions.R")

### Raw
## Randomly select 10 models
raw_fixed_resp_S <- raw_fixed_resp_M[sample(seq(1,100,1), size = 10, replace = F)]
raw_fluid_resp_S <- raw_fluid_resp_M[sample(seq(1,100,1), size = 10, replace = F)]
raw_fixed_both_S <- raw_fixed_both_M[sample(seq(1,100,1), size = 10, replace = F)]
raw_fluid_both_S <- raw_fluid_both_M[sample(seq(1,100,1), size = 10, replace = F)]
## Test model assumptions
for(i in 1:length(raw_fixed_resp_S)){
  test.model.assumptions(raw_fixed_resp_S[[i]])
}
## Test model assumptions
for(i in 1:length(raw_fluid_both_S)){
  test.model.assumptions(raw_fluid_both_S[[i]])
}
## Test model assumptions
for(i in 1:length(raw_fixed_resp_S)){
  test.model.assumptions(raw_fixed_resp_S[[i]])
}
## Test model assumptions
for(i in 1:length(raw_fluid_both_S)){
  test.model.assumptions(raw_fluid_both_S[[i]])
}

### SQS
## Randomly select 10 models
SQS_fixed_resp_S <- SQS_fixed_resp_M[sample(seq(1,100,1), size = 10, replace = F)]
SQS_fluid_resp_S <- SQS_fluid_resp_M[sample(seq(1,100,1), size = 10, replace = F)]
SQS_fixed_both_S <- SQS_fixed_both_M[sample(seq(1,100,1), size = 10, replace = F)]
SQS_fluid_both_S <- SQS_fluid_both_M[sample(seq(1,100,1), size = 10, replace = F)]
## Test model assumptions
for(i in 1:length(SQS_fixed_resp_S)){
  test.model.assumptions(SQS_fixed_resp_S[[i]])
}
## Test model assumptions
for(i in 1:length(SQS_fluid_both_S)){
  test.model.assumptions(SQS_fluid_both_S[[i]])
}
## Test model assumptions
for(i in 1:length(SQS_fixed_resp_S)){
  test.model.assumptions(SQS_fixed_resp_S[[i]])
}
## Test model assumptions
for(i in 1:length(SQS_fluid_both_S)){
  test.model.assumptions(SQS_fluid_both_S[[i]])
}

### CR20
## Randomly select 10 models
CR20_fixed_resp_S <- CR20_fixed_resp_M[sample(seq(1,100,1), size = 10, replace = F)]
CR20_fluid_resp_S <- CR20_fluid_resp_M[sample(seq(1,100,1), size = 10, replace = F)]
CR20_fixed_both_S <- CR20_fixed_both_M[sample(seq(1,100,1), size = 10, replace = F)]
CR20_fluid_both_S <- CR20_fluid_both_M[sample(seq(1,100,1), size = 10, replace = F)]
## Test model assumptions
for(i in 1:length(CR20_fixed_resp_S)){
  test.model.assumptions(CR20_fixed_resp_S[[i]])
}
## Test model assumptions
for(i in 1:length(CR20_fluid_both_S)){
  test.model.assumptions(CR20_fluid_both_S[[i]])
}
## Test model assumptions
for(i in 1:length(CR20_fixed_resp_S)){
  test.model.assumptions(CR20_fixed_resp_S[[i]])
}
## Test model assumptions
for(i in 1:length(CR20_fluid_both_S)){
  test.model.assumptions(CR20_fluid_both_S[[i]])
}

## Experimentation
NCR_out <- list()
for(i in 1:nrow(NCR_t1)){
  NCR_out <- append(NCR_out, str_flatten(c(as.character(NCR_t1[i,"PTME"]),NCR_t1[i,"brachiopod"]),collapse = "_"))
}
View(as.data.frame(table(unlist(NCR_out))))


