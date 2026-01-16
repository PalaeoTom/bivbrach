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
## Load data
NCR <- read.csv("data/analysis_data/genera_NC_CRV.csv", header = T, row.names = 1)
raw <- read.csv("data/analysis_data/genera_NC_raw.csv", header = T, row.names = 1)
CR20 <- read.csv("data/analysis_data/genera_NC_CR20.csv", header = T, row.names = 1)

## Check numeric entries are numeric
## NCR
for(i in c(4:8, 10)){
  if(!is.numeric(NCR[,i])){
    NCR[,i] <- as.numeric(NCR[,i])
  }
}

for(i in c(2,9)){
  if(!is.factor(NCR[,i])){
    NCR[,i] <- as.factor(NCR[,i])
  }
}

## Raw
for(i in c(4:8, 10)){
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

## Relevel PTME factor
NCR[,"PTME"] <- relevel(NCR[,"PTME"], ref = "PrePTME")
raw[,"PTME"] <- relevel(raw[,"PTME"], ref = "PrePTME")
CR20[,"PTME"] <- relevel(CR20[,"PTME"], ref = "PrePTME")

## Rename to simplify next few steps
colnames(NCR) <- c("stage_cell", "stage", "cell", "bivalve", "brachiopod", "long", "lat", "AbsLat", "PTME", "n")
colnames(raw) <- c("stage_cell", "stage", "cell", "bivalve", "brachiopod", "long", "lat", "AbsLat", "PTME", "n")
colnames(CR20) <- c("stage_cell", "stage", "cell", "bivalve", "brachiopod", "long", "lat", "AbsLat", "PTME")

## Some counts are decimal. Round these to discrete values to work with negative binomial models
# No need to do this for bivalve counts - will be standardised
NCR$brachiopod <- round(NCR$brachiopod, digits = 0)
raw$brachiopod <- round(raw$brachiopod, digits = 0)
CR20$brachiopod <- round(CR20$brachiopod, digits = 0)

## Standardise predictors
NCR <- std(NCR, NCR[,c(4, 8, 10)])
raw <- std(raw, raw[,c(4, 8, 10)])
CR20 <- std(CR20, CR20[,c(4, 8)])

## Drop non-standardized predictors
NCR <- NCR[,c(-4, -8, -10)]
raw <- raw[,c(-4, -8, -10)]
CR20 <- CR20[,c(-4, -8)]

## Re-do names
colnames(NCR) <- c("stage_cell", "stage", "cell", "brachiopod", "long", "lat", "PTME", "bivalve", "AbsLat", "n")
colnames(raw) <- c("stage_cell", "stage", "cell", "brachiopod", "long", "lat", "PTME", "bivalve", "AbsLat", "n")
colnames(CR20) <- c("stage_cell", "stage", "cell", "brachiopod", "long", "lat", "PTME", "bivalve", "AbsLat")

## Define best models
NCRmod <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR, family = nbinom12(link = "sqrt"))
rawMod<- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw, family = nbinom12(link = "sqrt"))
CR20mod <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20, family = nbinom2(link = "log"))
}
#### Testing for type 2 errors ####
## Create output for predictor terms
labels <- c("Generic\nbivalve\nrichness", "PTME", "Absolute\nlatitude", "Generic\nbivalve\nrichness +\nPTME", "Generic\nbivalve\nrichness\n + absolute\nlatitude")
term <- c("bivalve", "PTMEPostPTME", "AbsLat", "bivalve:PTMEPostPTME", "bivalve:AbsLat")
powerSimTerms <- c("bivalve", "PTME", "AbsLat", "bivalve:PTME", "bivalve:AbsLat")
type2_out <- data.frame(cbind("term" = term, "powerSimTerms" = powerSimTerms,  "labels" = labels))

## Create model containers
type2_out$NCR <- NA
type2_out$raw <- NA
type2_out$CR20 <- NA

## list models
bestModels <- list(NCRmod, rawMod, CR20mod)

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
  for(t in 1:length(terms)){
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
saveRDS(type2_results, "data/sensitivity_testing/genera_noCov_power_analyses_raw.Rds")
write.csv(type2_out, "data/sensitivity_testing/genera_noCov_power_analyses_power.csv")

#### Shuffling occurrences for type 1 error testing ####
## Need to read in occurrence data
occs <- readRDS("data/final/final_100_genera_noCov.Rds")

table(occs$source)/nrow(occs)

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

## Set iter
iter = 1000

#data = occs
#reps = iter
#stage = "stage"
#cell = "stage_cell"
#cell_abun = ref
#cell_covariates = NA
#CR_nOccs = 20
#fix_stages = F
#n_cores = 8
#CR = T

## CR20
## 1. Shuffle occurrences across stages
CR20_btwnStgs <- shuffle_occurrences(data = occs, reps = iter, stage = "stage", cell = "stage_cell", cell_abun = ref,
                                     cell_covariates = NA,
                                     CR_nOccs = 20,
                                     fix_stages = F, n_cores = 8) ; beep('complete')
## Export
saveRDS(CR20_btwnStgs, file = "data/sensitivity_testing/genera_noCov_CR20_btwnStgs.Rds")
rm(CR20_btwnStgs)

## 2. Shuffle occurrences within stages
CR20_wthnStgs <- shuffle_occurrences(occs, iter, "stage", "stage_cell", cell_abun = ref,
                                     cell_covariates = NA,
                                     CR_nOccs = 20,
                                     fix_stages = T, n_cores = 8) ; beep('fanfare')
saveRDS(CR20_wthnStgs, file = "data/sensitivity_testing/genera_noCov_CR20_wthnStgs.Rds")
rm(CR20_wthnStgs)

## raw
## 1. Shuffle occurrences across stages
raw_btwnStgs <- shuffle_occurrences(data = occs, reps = iter, stage = "stage", cell = "stage_cell", cell_abun = ref,
                                     cell_covariates = NA,
                                     CR_nOccs = 20, CR = F,
                                     fix_stages = F, n_cores = 8) ; beep('fanfare')
## Export
saveRDS(raw_btwnStgs, file = "data/sensitivity_testing/genera_noCov_raw_btwnStgs.Rds")
rm(raw_btwnStgs)

## 2. Shuffle occurrences within stages
raw_wthnStgs <- shuffle_occurrences(occs, iter, "stage", "stage_cell", cell_abun = ref,
                                     cell_covariates = NA,
                                     CR_nOccs = 20, CR = F,
                                     fix_stages = T, n_cores = 8) ; beep('fanfare')
saveRDS(raw_wthnStgs, file = "data/sensitivity_testing/genera_noCov_raw_wthnStgs.Rds")
rm(raw_wthnStgs)

## NCR
## First, need to reload rarefaction curve data
gen_RareC <- readRDS("data/sensitivity_testing/master_100_2_2_min3gen_min20occs_RCs.Rds")[,-1]
RC_GCs <- colnames(gen_RareC)

## Split
gen_NC_RareC <- gen_RareC[,which(RC_GCs %in% unique(NCR$stage_cell))]

## Test asymptotes of cells present to make sure all have some.
source("functions/test_RC_tail_asymptote.R")
asymptote.occs <- 5
slope.threshold <- 0.25
gen_NC_RareC_bool <- test_RC_tail_asymptote(RCs = gen_NC_RareC, n = asymptote.occs, threshold = slope.threshold)
names(which(!gen_NC_RareC_bool))

## None - that means all have a slope of less than 0.25 at some point.
## Now to proceed to find occurrence number capping the first run of 5 occurrences to produce an RC slope below 0.25. Minimum of 20 applied.
source("functions/find_slope.R")
gen_NC_n <- find_slope(gen_NC_RareC, n = 5, min.n = 20, threshold = 0.25, n.cores = 8)

## NCR
## 1. Shuffle occurrences across stages
NCR_btwnStgs <- shuffle_occurrences(data = occs, reps = iter, stage = "stage", cell = "stage_cell", cell_abun = ref,
                                     cell_covariates = NA,
                                     CR_nOccs = gen_NC_n,
                                     fix_stages = F, n_cores = 8) ; beep('fanfare')
## Export
saveRDS(NCR_btwnStgs, file = "data/sensitivity_testing/genera_noCov_NCR_btwnStgs.Rds")
rm(NCR_btwnStgs)

## 2. Shuffle occurrences within stages
NCR_wthnStgs <- shuffle_occurrences(occs, iter, "stage", "stage_cell", cell_abun = ref,
                                     cell_covariates = NA,
                                     CR_nOccs = gen_NC_n,
                                     fix_stages = T, n_cores = 8) ; beep('fanfare')
saveRDS(NCR_wthnStgs, file = "data/sensitivity_testing/genera_noCov_NCR_wthnStgs.Rds")
rm(NCR_wthnStgs)

## Standardise
## Load function for standardising
standardiseShuffledData <- function(string, characters = NA, numbers = NA, factors = NA, factor.ref, round = NA, column.names = NA, standardise = NA, n.cores = 1){
  ## read in data
  data <- readRDS(paste0(string, ".Rds"))
  ## Clean up
  output <- mclapply(1:length(data), mc.cores = n.cores, function(x){
    dat <- data[[x]]
    ## Add column names
    if(all(!is.na(column.names))){
      colnames(dat) <- column.names
    }
    ## Check columns are correct type
    if(all(!is.na(characters))){
      for(i in characters){
        dat[,i] <- as.character(dat[,i])
      }
    }
    if(all(!is.na(numbers))){
      for(i in numbers){
        dat[,i] <- as.numeric(dat[,i])
      }
    }
    if(all(!is.na(factors))){
      for(i in 1:length(factors)){
        dat[,factors[i]] <- as.factor(dat[,factors[i]])
        dat[,factors[i]] <- relevel(dat[,factors[i]], ref = factor.ref[i])
      }
    }
    ## Round counts if prescribed
    if(all(!is.na(round))){
      for(i in round){
        dat[,i] <- round(dat[,i], digits = 0)
      }
    }
    ## Standardise if required
    if(all(!is.na(standardise))){
        dat[,standardise] <- std(dat, dat[,standardise])[,seq(ncol(dat)+1, ncol(dat)+length(standardise),1)]
    }
    return(dat)
  })
  ## Export
  saveRDS(output, paste0(string, "_std.Rds"))
}

## Run function for each set of data
strings <- c("data/sensitivity_testing/genera_noCov_CR20_btwnStgs", "data/sensitivity_testing/genera_noCov_CR20_wthnStgs",
             "data/sensitivity_testing/genera_noCov_raw_btwnStgs", "data/sensitivity_testing/genera_noCov_raw_wthnStgs",
             "data/sensitivity_testing/genera_noCov_NCR_btwnStgs", "data/sensitivity_testing/genera_noCov_NCR_wthnStgs")

for(s in 1:length(strings)){
  print(s)
  standardiseShuffledData(string = strings[s],
                          characters = c(1,3),
                          numbers = c(4,5,6,7,8),
                          factors = c(2,9),
                          factor.ref = c("3","PrePTME"),
                          round = c(5),
                          standardise = c(4,8),
                          column.names = c("stage_cell", "stage", "cell", "bivalve", "brachiopod", "pLong", "pLat", "AbsLat", "PTME"),
                          n.cores = 8)
}

#### Type 1 error testing ####
strings.in <- c("data/sensitivity_testing/genera_noCov_CR20_btwnStgs_std.Rds", "data/sensitivity_testing/genera_noCov_CR20_wthnStgs_std.Rds",
             "data/sensitivity_testing/genera_noCov_raw_btwnStgs_std.Rds", "data/sensitivity_testing/genera_noCov_raw_wthnStgs_std.Rds",
             "data/sensitivity_testing/genera_noCov_NCR_btwnStgs_std.Rds", "data/sensitivity_testing/genera_noCov_NCR_wthnStgs_std.Rds")

## Run models
## CR20, shuffled between stages
CR20_btwn <- readRDS(strings.in[1])
CR20_btwn_models <- mclapply(1:length(CR20_btwn), mc.cores = 8, function(y){
  model <- tryCatch(
    {glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20_btwn[[y]], family = nbinom2(link = "log"))},
    error = function(msg){
      return(NA)
    })
  if(all(!is.na(model))){
    out <- get_model_data(model, type = "est", transform = NULL)
  } else {
    out <- NA
  }
  return(out)
})
saveRDS(CR20_btwn_models, file = "data/sensitivity_testing/genera_noCov_CR20_btwnStgs_simModels.Rds")
rm(CR20_btwn_models)
rm(CR20_btwn)

## CR20, shuffled within stages
CR20_wthn <- readRDS(strings.in[2])
CR20_wthn_models <- mclapply(1:length(CR20_wthn), mc.cores = 8, function(y){
  model <- tryCatch(
    {glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = CR20_wthn[[y]], family = nbinom2(link = "log"))},
    error = function(msg){
      return(NA)
    })
  if(all(!is.na(model))){
    out <- get_model_data(model, type = "est", transform = NULL)
  } else {
    out <- NA
  }
  return(out)
})
saveRDS(CR20_wthn_models, file = "data/sensitivity_testing/genera_noCov_CR20_wthnStgs_simModels.Rds")
rm(CR20_wthn_models)
rm(CR20_wthn)

## raw, shuffled between stages
raw_btwn <- readRDS(strings.in[3])
raw_btwn_models <- mclapply(1:length(raw_btwn), mc.cores = 8, function(y){
  model <- tryCatch(
    {glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw_btwn[[y]], family = nbinom12(link = "sqrt"))},
    error = function(msg){
      return(NA)
    })
  if(all(!is.na(model))){
    out <- get_model_data(model, type = "est", transform = NULL)
  } else {
    out <- NA
  }
  return(out)
})
saveRDS(raw_btwn_models, file = "data/sensitivity_testing/genera_noCov_raw_btwnStgs_simModels.Rds")
rm(raw_btwn_models)
rm(raw_btwn)

## raw, shuffled within stages
raw_wthn <- readRDS(strings.in[4])
raw_wthn_models <- mclapply(1:length(raw_wthn), mc.cores = 8, function(y){
  model <- tryCatch(
    {glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw_wthn[[y]], family = nbinom12(link = "sqrt"))},
    error = function(msg){
      return(NA)
    })
  if(all(!is.na(model))){
    out <- get_model_data(model, type = "est", transform = NULL)
  } else {
    out <- NA
  }
  return(out)
})
saveRDS(raw_wthn_models, file = "data/sensitivity_testing/genera_noCov_raw_wthnStgs_simModels.Rds")
rm(raw_wthn_models)
rm(raw_wthn)

## NCR, shuffled between stages
NCR_btwn <- readRDS(strings.in[5])

## First, we spot check 10
source("functions/test.model.assumptions.R")
NCR_spot <- NCR_btwn[sample(seq(1,1000,1), 10, replace = F)]
m1 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[1]], family = nbinom12(link = "sqrt"))
m2 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[2]], family = nbinom12(link = "sqrt"))
m3 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[3]], family = nbinom12(link = "sqrt"))
m4 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[4]], family = nbinom12(link = "sqrt"))
m5 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[5]], family = nbinom12(link = "sqrt"))
m6 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[6]], family = nbinom12(link = "sqrt"))
m7 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[7]], family = nbinom12(link = "sqrt"))
m8 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[8]], family = nbinom12(link = "sqrt"))
m9 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[9]], family = nbinom12(link = "sqrt"))
m10 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[10]], family = nbinom12(link = "sqrt"))
test.model.assumptions(m1)
test.model.assumptions(m2)
test.model.assumptions(m3)
test.model.assumptions(m4)
test.model.assumptions(m5)
test.model.assumptions(m6)
test.model.assumptions(m7)
test.model.assumptions(m8)
test.model.assumptions(m9)
test.model.assumptions(m10)

NCR_btwn_models <- mclapply(1:length(NCR_btwn), mc.cores = 8, function(y){
  model <- tryCatch(
    {glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_btwn[[y]], family = nbinom12(link = "sqrt"))},
    error = function(msg){
      return(NA)
    })
  if(all(!is.na(model))){
    out <- get_model_data(model, type = "est", transform = NULL)
  } else {
    out <- NA
  }
  return(out)
})
saveRDS(NCR_btwn_models, file = "data/sensitivity_testing/genera_noCov_NCR_btwnStgs_simModels.Rds")
rm(NCR_btwn_models)
rm(NCR_btwn)

## NCR, shuffled within stages
NCR_wthn <- readRDS(strings.in[6])

## First, we spot check 10
source("functions/test.model.assumptions.R")
NCR_spot <- NCR_wthn[sample(seq(1,1000,1), 10, replace = F)]
m1 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[1]], family = nbinom12(link = "sqrt"))
m2 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[2]], family = nbinom12(link = "sqrt"))
m3 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[3]], family = nbinom12(link = "sqrt"))
m4 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[4]], family = nbinom12(link = "sqrt"))
m5 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[5]], family = nbinom12(link = "sqrt"))
m6 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[6]], family = nbinom12(link = "sqrt"))
m7 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[7]], family = nbinom12(link = "sqrt"))
m8 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[8]], family = nbinom12(link = "sqrt"))
m9 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[9]], family = nbinom12(link = "sqrt"))
m10 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_spot[[10]], family = nbinom12(link = "sqrt"))
test.model.assumptions(m1)
test.model.assumptions(m2)
test.model.assumptions(m3)
test.model.assumptions(m4)
test.model.assumptions(m5)
test.model.assumptions(m6)
test.model.assumptions(m7)
test.model.assumptions(m8)
test.model.assumptions(m9)
test.model.assumptions(m10)

NCR_wthn_models <- mclapply(1:length(NCR_wthn), mc.cores = 8, function(y){
  model <- tryCatch(
    {glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR_wthn[[y]], family = nbinom12(link = "sqrt"))},
    error = function(msg){
      return(NA)
    })
  if(all(!is.na(model))){
    out <- get_model_data(model, type = "est", transform = NULL)
  } else {
    out <- NA
  }
  return(out)
})
saveRDS(NCR_wthn_models, file = "data/sensitivity_testing/genera_noCov_NCR_wthnStgs_simModels.Rds")
rm(NCR_wthn_models)
rm(NCR_wthn)

### Summarise
## Read in functions
source("functions/isolate_coeffs.R")
source("functions/compare_coeffs.R")
source("functions/isolate_and_compare_coeffs.R")

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

## Define plotlimits
CR20.plot.limits <- list("bivalve" = c(-2,0.5),
                        "PTMEPostPTME" = c(-2,0.5),
                        "bivalve:PTMEPostPTME" = c(-2,0.5),
                        "AbsLat" = c(-0.5,0.5),
                        "bivalve:AbsLat" = c(-0.5,0.5))

## CR20, within stages
CR20_wthn_models <- readRDS("data/sensitivity_testing/genera_noCov_CR20_wthnStgs_simModels.Rds")
title = "Comparing simulated and empirical coefficients of best model\nClassical rarefaction (sample size = 20)\n Occurrences shuffled within stages"
figure.name <- "genera_noCov_CR20_withinStgs_coeffs"
data.name <- "genera_noCov_CR20_withinStgs_coeffs"

isolate_and_compare_coeffs(simModels = CR20_wthn_models, mainModel = CR20mod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = CR20.plot.limits, visualsRef = visualsRef)
rm(CR20_wthn_models)

## CR20, between stages
CR20_btwn_models <- readRDS("data/sensitivity_testing/genera_noCov_CR20_btwnStgs_simModels.Rds")
title = "Comparing simulated and empirical coefficients of best model\nClassical rarefaction (sample size = 20)\n Occurrences shuffled between stages"
figure.name <- "genera_noCov_CR20_betweenStgs_coeffs"
data.name <- "genera_noCov_CR20_betweenStgs_coeffs"

isolate_and_compare_coeffs(simModels = CR20_btwn_models, mainModel = CR20mod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = CR20.plot.limits, visualsRef = visualsRef)
rm(CR20_btwn_models)

## Define plotlimits
NCR.plot.limits <- list("bivalve" = c(-0.5,8),
                        "PTMEPostPTME" = c(-8,1),
                        "bivalve:PTMEPostPTME" = c(-8,1),
                        "AbsLat" = c(-1,1),
                        "bivalve:AbsLat" = c(-1,1))

## NCR, within stages
NCR_wthn_models <- readRDS("data/sensitivity_testing/genera_noCov_NCR_wthnStgs_simModels.Rds")
title = "Comparing simulated and empirical coefficients of best model\nNon-classical rarefaction\n Occurrences shuffled within stages"
figure.name <- "genera_noCov_NCR_withinStgs_coeffs"
data.name <- "genera_noCov_NCR_withinStgs_coeffs"

isolate_and_compare_coeffs(simModels = NCR_wthn_models, mainModel = NCRmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = NCR.plot.limits, visualsRef = visualsRef)
rm(NCR_wthn_models)

## NCR, between stages
NCR_btwn_models <- readRDS("data/sensitivity_testing/genera_noCov_NCR_btwnStgs_simModels.Rds")
title = "Comparing simulated and empirical coefficients of best model\nNon-classical rarefaction\n Occurrences shuffled between stages"
figure.name <- "genera_noCov_NCR_betweenStgs_coeffs"
data.name <- "genera_noCov_NCR_betweenStgs_coeffs"

isolate_and_compare_coeffs(simModels = NCR_btwn_models, mainModel = NCRmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = NCR.plot.limits, visualsRef = visualsRef)
rm(NCR_btwn_models)


## Define plotlimits
raw.plot.limits <- list("bivalve" = c(-0.5,12),
                         "PTMEPostPTME" = c(-12,3),
                         "bivalve:PTMEPostPTME" = c(-12,5),
                         "AbsLat" = c(-2,2),
                         "bivalve:AbsLat" = c(-2,5))

## raw, within stages
raw_wthn_models <- readRDS("data/sensitivity_testing/genera_noCov_raw_wthnStgs_simModels.Rds")
title = "Comparing simulated and empirical coefficients of best model\nRaw richness\n Occurrences shuffled within stages"
figure.name <- "genera_noCov_raw_withinStgs_coeffs"
data.name <- "genera_noCov_raw_withinStgs_coeffs"

isolate_and_compare_coeffs(simModels = raw_wthn_models, mainModel = rawMod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = raw.plot.limits, visualsRef = visualsRef)
rm(raw_wthn_models)

## raw, between stages
raw_btwn_models <- readRDS("data/sensitivity_testing/genera_noCov_raw_btwnStgs_simModels.Rds")
title = "Comparing simulated and empirical coefficients of best model\nRaw richness\n Occurrences shuffled between stages"
figure.name <- "genera_noCov_raw_betweenStgs_coeffs"
data.name <- "genera_noCov_raw_betweenStgs_coeffs"

isolate_and_compare_coeffs(simModels = raw_btwn_models, mainModel = rawMod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = raw.plot.limits, visualsRef = visualsRef)
rm(raw_btwn_models)

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
figure.name <- "genera_noCov_CR20_brach_wthnStgs_coeffs"
data.name <- "genera_noCov_CR20_brach_wthnStgs_coeffs"

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
figure.name <- "genera_noCov_CR20_brach_wthnPTME_coeffs"
data.name <- "genera_noCov_CR20_brach_wthnPTME_coeffs"

isolate_and_compare_coeffs(simModels = CR20_fixedPTME_resp_C, mainModel = CR20mod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = CR20.plot.limits, visualsRef = visualsRef)
rm(CR20_fixedPTME_resp)
rm(CR20_fixedPTME_resp_C)
rm(CR20_fixedPTME_resp_W)

##### CR20 - fix before/after PTME - shuffle brachiopods and bivalves #####
## Shuffle brachiopods and bivalves across Palaeozoic and Mesozoic-Cenozoic
CR20_fixedPTME_both <- shuffle_bothonses(data = CR20_t1, reps = iter, stage = "PTME", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8), shuffle_predictor = T, fix_stages = T, n_cores = 8)

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
figure.name <- "genera_noCov_CR20_bivNbrach_wthnPTME_coeffs"
data.name <- "genera_noCov_CR20_bivNbrach_wthnPTME_coeffs"

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
figure.name <- "genera_noCov_CR20_bivNbrach_wthnStgs_coeffs"
data.name <- "genera_noCov_CR20_bivNbrach_wthnStgs_coeffs"

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
figure.name <- "genera_noCov_CR20_brach_btwnStgs_coeffs"
data.name <- "genera_noCov_CR20_brach_btwnStgs_coeffs"

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
figure.name <- "genera_noCov_CR20_bivNbrach_btwnStgs_coeffs"
data.name <- "genera_noCov_CR20_bivNbrach_btwnStgs_coeffs"

isolate_and_compare_coeffs(simModels = CR20_fluid_both_C, mainModel = CR20mod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = CR20.plot.limits, visualsRef = visualsRef)
rm(CR20_fluid_both)
rm(CR20_fluid_both_C)
rm(CR20_fluid_both_W)

#### Shuffling richness values for type 1 error testing - raw ####
##### raw - fixed befoew/after PTME - shuffle brachiopods #####
raw_fixedPTME_resp <- shuffle_responses(data = raw_t1, reps = iter, stage = "PTME", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8, 10), shuffle_predictor = F, fix_stages = T, n_cores = 8)

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
figure.name <- "genera_noCov_raw_brach_wthnPTME_coeffs"
data.name <- "genera_noCov_raw_brach_wthnPTME_coeffs"

isolate_and_compare_coeffs(simModels = raw_fixedPTME_resp_C, mainModel = rawmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = raw.plot.limits, visualsRef = visualsRef)
rm(raw_fixedPTME_resp)
rm(raw_fixedPTME_resp_C)
rm(raw_fixedPTME_resp_W)

##### raw - fixed before/after PTME - shuffle brachiopods and bivalves #####
raw_fixedPTME_both <- shuffle_bothonses(data = raw_t1, reps = iter, stage = "PTME", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8, 10), shuffle_predictor = T, fix_stages = T, n_cores = 8)

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
figure.name <- "genera_noCov_raw_bivNbrach_wthnPTME_coeffs"
data.name <- "genera_noCov_raw_bivNbrach_wthnPTME_coeffs"

isolate_and_compare_coeffs(simModels = raw_fixedPTME_both_C, mainModel = rawmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = raw.plot.limits, visualsRef = visualsRef)
rm(raw_fixedPTME_both)
rm(raw_fixedPTME_both_C)
rm(raw_fixedPTME_both_W)

##### raw - fixed stages - shuffle brachiopods #####
raw_fixed_resp <- shuffle_responses(data = raw_t1, reps = 200, stage = "stage", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8, 10), shuffle_predictor = F, fix_stages = T, n_cores = 8)

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
figure.name <- "genera_noCov_raw_brach_wthnStgs_coeffs"
data.name <- "genera_noCov_raw_brach_wthnStgs_coeffs"

isolate_and_compare_coeffs(simModels = raw_fixed_resp_C, mainModel = rawMod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = raw.plot.limits, visualsRef = visualsRef)
rm(raw_fixed_resp)
rm(raw_fixed_resp_C)
rm(raw_fixed_resp_W)

##### raw - fixed stages - shuffle brachiopods and bivalves #####
raw_fixed_both <- shuffle_responses(data = raw_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", c(4, 8, 10), shuffle_predictor = T, fix_stages = T, n_cores = 8)

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
figure.name <- "genera_noCov_raw_bivNbrach_wthnStgs_coeffs"
data.name <- "genera_noCov_raw_bivNbrach_wthnStgs_coeffs"

isolate_and_compare_coeffs(simModels = raw_fixed_both_C, mainModel = rawMod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = raw.plot.limits, visualsRef = visualsRef)
rm(raw_fixed_both)
rm(raw_fixed_both_C)
rm(raw_fixed_both_W)

##### raw - fluid stages - shuffle brachiopods #####
raw_fluid_resp <- shuffle_responses(data = raw_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", c(4, 8, 10), shuffle_predictor = F, fix_stages = F, n_cores = 8)

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
figure.name <- "genera_noCov_raw_brach_btwnStgs_coeffs"
data.name <- "genera_noCov_raw_brach_btwnStgs_coeffs"

isolate_and_compare_coeffs(simModels = raw_fluid_resp_C, mainModel = rawMod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = raw.plot.limits, visualsRef = visualsRef)
rm(raw_fluid_resp)
rm(raw_fluid_resp_C)
rm(raw_fluid_resp_W)

##### raw - fluid stages - shuffle brachiopods and bivalves #####
raw_fluid_both <- shuffle_responses(data = raw_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", c(4, 8, 10), shuffle_predictor = T, fix_stages = F, n_cores = 8)

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
figure.name <- "genera_noCov_raw_bivNbrach_btwnStgs_coeffs"
data.name <- "genera_noCov_raw_bivNbrach_btwnStgs_coeffs"

isolate_and_compare_coeffs(simModels = raw_fluid_both_C, mainModel = rawMod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = raw.plot.limits, visualsRef = visualsRef)
rm(raw_fluid_both)
rm(raw_fluid_both_C)
rm(raw_fluid_both_W)

#### Shuffling richness values for type 1 error testing - SQS ####
##### SQS - fix stages - shuffle brachiopods #####
SQS_fixed_resp <- shuffle_responses(data = SQS_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8), shuffle_predictor = F, fix_stages = T, n_cores = 8)

## Run models and record warnings
SQS_fixed_resp_M <- list()
SQS_fixed_resp_W <- list()
for(i in 1:length(SQS_fixed_resp)){
  ## Run model
  warns <- list()
  withCallingHandlers(SQS_fixed_resp_M <- append(SQS_fixed_resp_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = SQS_fixed_resp[[i]], family = nbinom2(link = "log")))), warning = function(warn) {warns <<- append(warns, warn)})
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
figure.name <- "genera_noCov_SQS_brach_wthnStgs_coeffs"
data.name <- "genera_noCov_SQS_brach_wthnStgs_coeffs"

isolate_and_compare_coeffs(simModels = SQS_fixed_resp_C, mainModel = SQSmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = SQS.plot.limits, visualsRef = visualsRef)
rm(SQS_fixed_resp)
rm(SQS_fixed_resp_C)
rm(SQS_fixed_resp_W)

##### SQS - fix before/after PTME - shuffle brachiopods #####
SQS_fixedPTME_resp <- shuffle_responses(data = SQS_t1, reps = iter, stage = "PTME", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8), shuffle_predictor = F, fix_stages = T, n_cores = 8)

## Run models and record warnings
SQS_fixedPTME_resp_M <- list()
SQS_fixedPTME_resp_W <- list()
for(i in 1:length(SQS_fixedPTME_resp)){
  ## Run model
  warns <- list()
  withCallingHandlers(SQS_fixedPTME_resp_M <- append(SQS_fixedPTME_resp_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = SQS_fixedPTME_resp[[i]], family = nbinom2(link = "log")))), warning = function(warn) {warns <<- append(warns, warn)})
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
figure.name <- "genera_noCov_SQS_brach_wthnPTME_coeffs"
data.name <- "genera_noCov_SQS_brach_wthnPTME_coeffs"

isolate_and_compare_coeffs(simModels = SQS_fixedPTME_resp_C, mainModel = SQSmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = SQS.plot.limits, visualsRef = visualsRef)
rm(SQS_fixedPTME_resp)
rm(SQS_fixedPTME_resp_C)
rm(SQS_fixedPTME_resp_W)

##### SQS - fix before/after PTME - shuffle brachiopods and bivalves #####
## Shuffle brachiopods and bivalves across Palaeozoic and Mesozoic-Cenozoic
SQS_fixedPTME_both <- shuffle_bothonses(data = SQS_t1, reps = iter, stage = "PTME", response = "brachiopod", predictor = "bivalve", standardise =  c(4, 8), shuffle_predictor = T, fix_stages = T, n_cores = 8)

## Run models and record warnings
SQS_fixedPTME_both_M <- list()
SQS_fixedPTME_both_W <- list()
for(i in 1:length(SQS_fixedPTME_both)){
  ## Run model
  warns <- list()
  withCallingHandlers(SQS_fixedPTME_both_M <- append(SQS_fixedPTME_both_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = SQS_fixedPTME_both[[i]], family = nbinom2(link = "log")))), warning = function(warn) {warns <<- append(warns, warn)})
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
figure.name <- "genera_noCov_SQS_bivNbrach_wthnPTME_coeffs"
data.name <- "genera_noCov_SQS_bivNbrach_wthnPTME_coeffs"

isolate_and_compare_coeffs(simModels = SQS_fixedPTME_both_C, mainModel = SQSmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = SQS.plot.limits, visualsRef = visualsRef)
rm(SQS_fixedPTME_both)
rm(SQS_fixedPTME_both_C)
rm(SQS_fixedPTME_both_W)

##### SQS - fix stages - shuffle brachiopods and bivalves #####
SQS_fixed_both <- shuffle_responses(data = SQS_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", c(4, 8), shuffle_predictor = T, fix_stages = T, n_cores = 8)

## Run models and record warnings
SQS_fixed_both_M <- list()
SQS_fixed_both_W <- list()
for(i in 1:length(SQS_fixed_both)){
  ## Run model
  warns <- list()
  withCallingHandlers(SQS_fixed_both_M <- append(SQS_fixed_both_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = SQS_fixed_both[[i]], family = nbinom2(link = "log")))), warning = function(warn) {warns <<- append(warns, warn)})
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
figure.name <- "genera_noCov_SQS_bivNbrach_wthnStgs_coeffs"
data.name <- "genera_noCov_SQS_bivNbrach_wthnStgs_coeffs"

isolate_and_compare_coeffs(simModels = SQS_fixed_both_C, mainModel = SQSmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = SQS.plot.limits, visualsRef = visualsRef)
rm(SQS_fixed_both)
rm(SQS_fixed_both_C)
rm(SQS_fixed_both_W)

##### SQS - fluid stages - shuffle brachiopods #####
SQS_fluid_resp <- shuffle_responses(data = SQS_t1, reps = iter, stage = "stage", response = "brachiopod", predictor = "bivalve", c(4, 8), shuffle_predictor = F, fix_stages = F, n_cores = 8)

## Run models and record warnings
SQS_fluid_resp_M <- list()
SQS_fluid_resp_W <- list()
for(i in 1:length(SQS_fluid_resp)){
  ## Run model
  warns <- list()
  withCallingHandlers(SQS_fluid_resp_M <- append(SQS_fluid_resp_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = SQS_fluid_resp[[i]], family = nbinom2(link = "log")))), warning = function(warn) {warns <<- append(warns, warn)})
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
figure.name <- "genera_noCov_SQS_brach_btwnStgs_coeffs"
data.name <- "genera_noCov_SQS_brach_btwnStgs_coeffs"

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
  withCallingHandlers(SQS_fluid_both_M <- append(SQS_fluid_both_M, list(glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = SQS_fluid_both[[i]], family = nbinom2(link = "log")))), warning = function(warn) {warns <<- append(warns, warn)})
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
figure.name <- "genera_noCov_SQS_bivNbrach_btwnStgs_coeffs"
data.name <- "genera_noCov_SQS_bivNbrach_btwnStgs_coeffs"

isolate_and_compare_coeffs(simModels = SQS_fluid_both_C, mainModel = SQSmod, coeffs = coeffs, fig.export.dir = fig.export.dir, data.export.dir = data.export.dir, figure.name = figure.name, data.name = data.name,
                           plot.title = title, plot.limits = SQS.plot.limits, visualsRef = visualsRef)
rm(SQS_fluid_both)
rm(SQS_fluid_both_C)
rm(SQS_fluid_both_W)












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


