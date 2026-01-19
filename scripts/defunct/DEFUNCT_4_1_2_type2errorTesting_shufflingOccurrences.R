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
             "data/sensitivity_testing/genera_noCov_raw_btwnStgs", "data/sensitivity_testing/genera_noCov_raw_wthnStgs")

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
