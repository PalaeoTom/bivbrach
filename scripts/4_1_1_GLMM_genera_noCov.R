## 4.1.1 GLMM - genera, no covariates
## Started by TJS on 27/06/2025

#### Startup ####
## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("sjmisc", "stringr", "DHARMa", "performance", "glmmTMB", "terra", "lme4", "sjPlot", "paleoverse")
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
library(sjPlot)
library(palaeoverse)

#### Read in and prepare data ####
## Load data
NCR <- read.csv("data/analysis_data/genera_NC_CRV.csv", header = T, row.names = 1)
raw <- read.csv("data/analysis_data/genera_NC_raw.csv", header = T, row.names = 1)
CR20 <- read.csv("data/analysis_data/genera_NC_CR20.csv", header = T, row.names = 1)

## Read in functions for testing assumptions
source("functions/test.model.assumptions.R")
source("functions/test.spatial.autocorrelation.R")

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

#### Non-classical rarefaction model fitting ####
## Non-classical rarefaction
## Poisson. Link = log
NCRm1 <- glmer(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"))
NCRm1_diag <- test.model.assumptions(NCRm1)

## Poisson. Link = sqrt
NCRm2 <- glmer(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))
NCRm2_diag <- test.model.assumptions(NCRm2)

## Poisson. Link = log, zero-inflation
NCRm1zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = NCR, family = poisson(link = "log"))
NCRm1zi_diag <- test.model.assumptions(NCRm1zi)

## Poisson. Link = sqrt, zero-inflation
NCRm2zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = NCR, family = poisson(link = "sqrt"))
NCRm2zi_diag <- test.model.assumptions(NCRm2zi)

## nbinom2. link = sqrt marginally better than log.
NCRm3 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR, family = nbinom2(link = "sqrt"))
NCRm3_diag <- test.model.assumptions(NCRm3)

## nbinom2. Zero-inflation. Neither sqrt or log particularly good.
NCRm3zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = NCR, family = nbinom2(link = "sqrt"))
NCRm3zi_diag <- test.model.assumptions(NCRm3zi)

## nbinom1. Link = sqrt marginally better
NCRm4 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR, family = nbinom1(link = "sqrt"))
NCRm4_diag <- test.model.assumptions(NCRm4)

## nbinom1. Zero inflation. Convergence issue with link=log.
NCRm4zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = NCR, family = nbinom1(link = "sqrt"))
NCRm4zi_diag <- test.model.assumptions(NCRm4zi)

## nbinom12. link = sqrt gives us first normal distribution. Significantly underdispersed.Best so far.
NCRm5 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR, family = nbinom12(link = "sqrt"))
NCRm5_diag <- test.model.assumptions(NCRm5)
write.csv(NCRm5_diag, file = "data/sensitivity_testing/non_classicalRarefaction_bestModel_diagnostics.csv")

## nbinom12. Zero inflation. Link = log doesn't converge.
NCRm5zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = NCR, family = nbinom12(link = "sqrt"))
NCRm5zi_diag <- test.model.assumptions(NCRm5zi)

## Conway-Maxwell Poisson (link = log). Link = sqrt just runs forever
NCRm6 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR, family = compois(link = "log"))
NCRm6_diag <- test.model.assumptions(NCRm6)

## Conway-Maxwell Poisson (link = log), zero inflation
NCRm6zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = NCR, family = compois(link = "log"))
NCRm6zi_diag <- test.model.assumptions(NCRm6zi)

## Generalized Poisson model
NCRm7 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = NCR, family = genpois(link = "sqrt"))
NCRm7_diag <- test.model.assumptions(NCRm7)

## Generalized Poisson model with zero inflation
NCRm7zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = NCR, family = genpois(link = "sqrt"))
NCRm7zi_diag <- test.model.assumptions(NCRm7zi)

##

### Best model for NCR: nbinom12, link= sqrt, no zero-inflation (NCRm5) ###

##

#### Raw richness model fitting ####
## Poisson. Link = log
rawm1 <- glmer(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = raw, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"))
rawm1_diag <- test.model.assumptions(rawm1)

## Poisson. Link = sqrt
rawm2 <- glmer(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = raw, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))
rawm2_diag <- test.model.assumptions(rawm2)

## Poisson. Link = log, zero-inflation
rawm1zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw, family = poisson(link = "log"))
rawm1zi_diag <- test.model.assumptions(rawm1zi)

## Poisson. Link = sqrt, zero-inflation
rawm2zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw, family = poisson(link = "sqrt"))
rawm2zi_diag <- test.model.assumptions(rawm2zi)

## nbinom2. link = sqrt marginally better than log. Normally distributed.
rawm3 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = raw, family = nbinom2(link = "sqrt"))
rawm3_diag <- test.model.assumptions(rawm3)

## nbinom2. Zero-inflation.
rawm3zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw, family = nbinom2(link = "sqrt"))
rawm3zi_diag <- test.model.assumptions(rawm3zi)

## nbinom1. Link = sqrt marginally better. Normally distributed. Great all round apart from residuals.
rawm4 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = raw, family = nbinom1(link = "sqrt"))
rawm4_diag <- test.model.assumptions(rawm4)

## nbinom1. Zero inflation. Again, all assumptions fine. But residuals have this gap. Probably better than previous
rawm4zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw, family = nbinom1(link = "sqrt"))
rawm4zi_diag <- test.model.assumptions(rawm4zi)

## nbinom12. link = log marginally worse in terms of assumptions but residuals a little better.
rawm5 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = raw, family = nbinom12(link = "log"))
rawm5_diag <- test.model.assumptions(rawm5)

## nbinom12. Zero inflation. Square root is better. Dispersion is an issue but residuals look better.
rawm5zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw, family = nbinom12(link = "sqrt"))
rawm5zi_diag <- test.model.assumptions(rawm5zi)
write.csv(rawm5zi_diag, file = "data/sensitivity_testing/rawRichness_bestModel_diagnostics.csv")

## Conway-Maxwell Poisson (link = log). Link = sqrt just runs forever
rawm6 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = raw, family = compois(link = "log"))
rawm6_diag <- test.model.assumptions(rawm6)

## Conway-Maxwell Poisson (link = log), zero inflation
rawm6zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw, family = compois(link = "log"))
rawm6zi_diag <- test.model.assumptions(rawm6zi)

## Generalized Poisson model
rawm7 <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), data = raw, family = genpois(link = "sqrt"))
rawm7_diag <- test.model.assumptions(rawm7)

## Generalized Poisson model with zero inflation
rawm7zi <- glmmTMB(brachiopod ~ bivalve * PTME + AbsLat + bivalve:AbsLat + (bivalve|stage), ziformula = ~1, data = raw, family = genpois(link = "sqrt"))
rawm7zi_diag <- test.model.assumptions(rawm7zi)

##

### Best model for raw: nbinom12, link= sqrt, zero-inflation (rawm5zi) ###

##

#### Classical rarefaction model fitting ####
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

##

### Best model for CR20 - nbinom2, link = log, zero inflation (CR20m3zi) ###

##

#### Empirical versus predicted richness through time (NCR only) ####
## Read in clean data
richPlotData <- read.csv("data/analysis_data/genera_NC_CR20.csv", row.names = 1)

## Read in stages
stages <- read.csv("data/metadata/binning_timescale.csv")
stages$number <- seq(1,92,1)

## Add midpoints to data
richPlotData$midpoint <- stages[match(richPlotData$stage,stages$number),"midpoint"]

## Isolate stages and midpoints
richnessEMP <- richPlotData[,c(2,10)]
richnessEMP <- richnessEMP[!duplicated(richnessEMP),]

## Get median, 5th quantile, and 95th quantile
richnessEMP$brachiopod_med <- NA
richnessEMP$brachiopod_5th <- NA
richnessEMP$brachiopod_95th <- NA
richnessEMP$brachiopod_mean <- NA
richnessEMP$brachiopod_plus1sd <- NA
richnessEMP$brachiopod_minus1sd <- NA

## Get stages in file
stages <- richnessEMP$stage

for(b in stages){
  ## Isolate relevant occurrences
  bin <- richPlotData[which(richPlotData$stage==b),]
  ## brachiopod information
  richnessEMP[which(richnessEMP$stage == b),"brachiopod_med"] <- median(bin[,"brachiopod"])
  richnessEMP[which(richnessEMP$stage == b),"brachiopod_5th"] <- quantile(bin[,"brachiopod"], probs = 0.05)
  richnessEMP[which(richnessEMP$stage == b),"brachiopod_95th"] <- quantile(bin[,"brachiopod"], probs = 0.95)
  richnessEMP[which(richnessEMP$stage == b),"brachiopod_mean"] <- mean(bin[,"brachiopod"])
  richnessEMP[which(richnessEMP$stage == b),"brachiopod_plus1sd"] <- mean(bin[,"brachiopod"])+sd(bin[,"brachiopod"])
  richnessEMP[which(richnessEMP$stage == b),"brachiopod_minus1sd"] <- mean(bin[,"brachiopod"])-sd(bin[,"brachiopod"])
}

## Any NAs
richnessEMP[which(is.na(richnessEMP$brachiopod_plus1sd)),"brachiopod_plus1sd"] <- richnessEMP[which(is.na(richnessEMP$brachiopod_plus1sd)),"brachiopod_mean"]
richnessEMP[which(is.na(richnessEMP$brachiopod_minus1sd)),"brachiopod_minus1sd"] <- richnessEMP[which(is.na(richnessEMP$brachiopod_minus1sd)),"brachiopod_mean"]

## Any negatives, convert to 0
richnessEMP[which(richnessEMP$brachiopod_minus1sd<0),"brachiopod_minus1sd"] <- 0

## Order by stage
richnessEMP <- richnessEMP[order(richnessEMP$stage),]

## Comparing NCRm5 predictions
## Comparing through time
## Isolate predictors
pred <- NCR[,-which(colnames(NCR)=="brachiopod")]
brachiopod <- glmmTMB:::predict.glmmTMB(NCRm5, type = "response", newdata = pred)
pred <- cbind(brachiopod, pred)

## Read in stages
stages <- read.csv("data/metadata/binning_timescale.csv")
stages$number <- seq(1,92,1)

## Add midpoints to data
pred$midpoint <- stages[match(pred$stage,stages$number),"midpoint"]

## Isolate stages and midpoints
richness <- pred[,c(3,11)]
richness <- richness[!duplicated(richness),]

## Get median, 5th quantile, and 95th quantile
richness$brachiopod_med <- NA
richness$brachiopod_5th <- NA
richness$brachiopod_95th <- NA
richness$brachiopod_mean <- NA
richness$brachiopod_plus1sd <- NA
richness$brachiopod_minus1sd <- NA

## Get stages in file
stages <- richness$stage

for(b in stages){
  ## Isolate relevant occurrences
  bin <- pred[which(pred$stage==b),]
  ## brachiopod information
  richness[which(richness$stage == b),"brachiopod_med"] <- median(bin[,"brachiopod"])
  richness[which(richness$stage == b),"brachiopod_5th"] <- quantile(bin[,"brachiopod"], probs = 0.05)
  richness[which(richness$stage == b),"brachiopod_95th"] <- quantile(bin[,"brachiopod"], probs = 0.95)
  richness[which(richness$stage == b),"brachiopod_mean"] <- mean(bin[,"brachiopod"])
  richness[which(richness$stage == b),"brachiopod_plus1sd"] <- mean(bin[,"brachiopod"])+sd(bin[,"brachiopod"])
  richness[which(richness$stage == b),"brachiopod_minus1sd"] <- mean(bin[,"brachiopod"])-sd(bin[,"brachiopod"])
}

## Any NAs
richness[which(is.na(richness$brachiopod_plus1sd)),"brachiopod_plus1sd"] <- richness[which(is.na(richness$brachiopod_plus1sd)),"brachiopod_mean"]
richness[which(is.na(richness$brachiopod_minus1sd)),"brachiopod_minus1sd"] <- richness[which(is.na(richness$brachiopod_minus1sd)),"brachiopod_mean"]

## Any negatives, convert to 0
richness[which(richness$brachiopod_minus1sd<0),"brachiopod_minus1sd"] <- 0

## Order by stage
richness <- richness[order(richness$stage),]

## Plot empirical and predicted richness through time
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_NC_predictions_versus_empirical_richness_through_time.pdf")
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = richness$midpoint, y = richness$brachiopod_mean, main = "Spatially standardised generic richness (mean +/- 1 SD)\nNon-classical rarefaction \n 100km grid cells, no covariate data required", axes = FALSE, xlim = c(520, 0), ylim = c(0,20), yaxs="i", xaxs="i",
     xlab = "Time (mya)", ylab = "Generic richness", type = "l", col = rgb(red = 1, green = 0, blue = 0, alpha = 0.75))
polygon(c(rev(richness$midpoint), richness$midpoint), c(rev(richness$brachiopod_plus1sd), richness$brachiopod_minus1sd), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.25), border = NA)
## Plot empirical using below
lines(x = richnessEMP$midpoint, y = richnessEMP$brachiopod_mean, xlim = c(520, 0), ylim = c(0,75),
      xlab = NA, ylab = "", type = "l", col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75))
polygon(c(rev(richnessEMP$midpoint), richnessEMP$midpoint), c(rev(richnessEMP$brachiopod_plus1sd), richnessEMP$brachiopod_minus1sd), col = rgb(red = 0, green = 0, blue = 1, alpha = 0.25), border = NA)
box()
axis(2)
legend("topleft",legend = c("Predicted mean brachiopods", "Empirical mean brachiopods"),bg = "white", col = c(rgb(red = 1, green = 0, blue = 0, alpha = 0.75),col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75)),lty = 1)
axis_geo(side = 1, intervals = "international periods")
dev.off()

#### Richness versus absolute latitude, empirical versus predicted (NCR only) ####
## Read in absolute latitude
abslat <- read.csv("data/analysis_data/genera_NC_CRV.csv", header = T, row.names = 1)$cellAbsLat

## Plot relationship
pdf("figures/final/supplemental/genera_CO_latitude_versus_richness.pdf")
par(mfrow = c(1,2))
plot(pred[which(pred$PTME == "PrePTME"),"brachiopod"], abslat[which(pred$PTME == "PrePTME")], main = "Predicted brachiopod values", ylab = "Absolute latitude", xlab = "Brachiopod generic richness", col = "red", xlim = c(0,40))
points(pred[which(pred$PTME == "PostPTME"),"brachiopod"], abslat[which(pred$PTME == "PostPTME")], col = "blue")
legend("topright", legend = c("Pre-PTME", "Post-PTME"),bg = "white", col = c("red", "blue"), pch = 1)
plot(NCR[which(NCR$PTME == "PrePTME"),"brachiopod"], abslat[which(NCR$PTME == "PrePTME")], main = "Empirical brachiopod values", ylab = "Absolute latitude", xlab = "Brachiopod generic richness", col = "red", xlim = c(0,40))
points(NCR[which(NCR$PTME == "PostPTME"),"brachiopod"], abslat[which(NCR$PTME == "PostPTME")], col = "blue")
legend("topright", legend = c("Pre-PTME", "Post-PTME"),bg = "white", col = c("red", "blue"), pch = 1)
dev.off()

#### Dredging model (NCR only) ####
## First, re-define model so as to specify na.activity. We include interaction between bivalves and PTME to see if relationship was reset.
testModel <- glmmTMB(brachiopod ~ bivalve * PTME * AbsLat + (bivalve|stage), data = NCR, family = nbinom12(link = "sqrt"), na.action = "na.fail")

## Get summary
tab_model(testModel)

## Trying with two-way interactions first, will try to add. Sticking with AICc due to relatively small sample sizes in some stages.
combos <- dredge(testModel, rank = "AICc")

## Export
saveRDS(combos, file = "data/sensitivity_testing/genera_NC_CR20_dredge_results.Rds")

## Get models with delta AIC of 2 or less
bestModels <- get.models(combos, delta<2)

## Two models with delta AIC less than 2
## Explore best models. Test for convergence and assumptions.
summary(bestModels[[1]])
bestModels_1 <- glmmTMB(brachiopod ~ bivalve * PTME + (bivalve|stage), data = NCR, family = nbinom12(link = "sqrt"))
test.model.assumptions(bestModels_1)
# Dispersion: ratio 0.68107, p , 2.2E-16, significant.
# ZI: ratio 1.3232, p < 2.2e-16, significant.
# Outliers: expected 0.00663, observed 0.00357, p = 0.28, Insignificant.
# Normality: p = 04442, significant.
# Collinearity: PTME VIF 5.45, bivalve:PTME VIF 5.99, all other VIFs below 2.5.
test.spatial.autocorrelation(bestModels_1, CR20)
## Expected -0.00045579, observed = -0.002219, p = 0.1613, insignificant.

summary(bestModels[[2]])
bestModels_2 <- glmmTMB(brachiopod ~ bivalve * PTME + (bivalve|stage), data = CR20, family = nbinom2(link = "log"))
test.model.assumptions(bestModels_2)
# Dispersion: ratio 0.68871, p < 2.2e16, insignificant.
# ZI: ratio 1.3205,  p < 2.2e-16, significant.
# Outliers: expected 0.00607866, observed 0.00516, p = 0.86, insignificant.
# Normality: p = 0.06373, insignificant.
# Collinearity: PTME VIF 5.49, bivalve:PTME VIF 6.03, all other VIFs below 2.5.
test.spatial.autocorrelation(bestModels_2, CR20)
## Expected -0.00045579, observed = -0.00109, p = 0.6116, insignificant.

#### Plotting coefficients for top models ####
## Define new title
plot.title2 <- "Model coefficients from 100km grid cells\nClassical rarefaction (sample size = 20), no covariate data required"

## Define axis labels and colours
axis.labels <- c("Bathymetry", "Generic\nbivalve\nrichness", "Lithology", "PTME", "Reefs", "Generic\nbivalve\nrichness\n+ PTME", "Absolute\nlatitude")
colours <- c("lightblue", "darkgreen", "purple", "darkgrey", "pink", "orange", "cyan")
term <- c("bath", "bivalve", "lith", "PTMEPostPTME", "reef", "bivalve:PTMEPostPTME", "AbsLat")
visualsRef <- data.frame(cbind("term" = term, "labels" = axis.labels, "colour" = colours))

## Get model data for each of the best models
coeffs <- lapply(1:length(bestModels), function(x){
  out <- get_model_data(bestModels[[x]], type = "est", transform = NULL)
})

## Function to reorgaise visualsRef into right order
reorganise_visualsRef <- function(visualsRef, coeffData){
  out <- visualsRef[rev(match(coeffData[,"term"], visualsRef[,"term"])),]
}
visuals <- lapply(1:length(bestModels), function(x){
  out <- reorganise_visualsRef(visualsRef, coeffs[[x]])
})

## Generate plots
plots <- lapply(1:length(bestModels), function(x){
  p <- ggplot() +
    scale_fill_manual(values = visuals[[x]][,"colour"]) +
    scale_color_manual(values = visuals[[x]][,"colour"]) +
    scale_x_discrete(labels = visuals[[x]][,"labels"]) +
    scale_y_continuous(expand = c(0,0), limits = c(-1.5,1)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
    geom_point(data = coeffs[[x]], (aes(x = term, y = estimate, fill = term, colour = term))) +
    geom_errorbar(data = coeffs[[x]], aes(x = term, ymin = conf.low, ymax = conf.high, colour = term), width=0.01) +
    ylab("Log-Odds") +
    xlab("") +
    theme(text = element_text(family = "Helvetica"),
          title = element_text(size = 12),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "none")
})

## Plot as grid
grid <- plot_grid(plotlist = plots, labels = LETTERS[1:length(bestModels)])
title <- ggdraw() + draw_label(plot.title2, fontface='bold')
output <- plot_grid(title, grid, ncol=1, rel_heights=c(0.075, 1)) # rel_heights values control title margins
## Check
output

## Export
pdf("figures/final/main/genera_NC_CR20_coefficients.pdf", width = 13)
print(output)
dev.off()

#### Plotting spatially standardised richness ####
## Read in clean data
richPlotData <- read.csv("data/analysis_data/genera_NC_CR20.csv", row.names = 1)

## Read in stages
stages <- read.csv("data/metadata/binning_timescale.csv")
stages$number <- seq(1,92,1)

## Add midpoints to data
richPlotData$midpoint <- stages[match(richPlotData$stage,stages$number),"midpoint"]

## Isolate stages and midpoints
richness <- richPlotData[,c(2,10)]
richness <- richness[!duplicated(richness),]

## Get median, 5th quantile, and 95th quantile
richness$bivalve_med <- NA
richness$bivalve_5th <- NA
richness$bivalve_95th <- NA
richness$brachiopod_med <- NA
richness$brachiopod_5th <- NA
richness$brachiopod_95th <- NA
richness$bivalve_mean <- NA
richness$bivalve_plus1sd <- NA
richness$bivalve_minus1sd <- NA
richness$brachiopod_mean <- NA
richness$brachiopod_plus1sd <- NA
richness$brachiopod_minus1sd <- NA

## Get stages in file
stages <- richness$stage

for(b in stages){
  ## Isolate relevant occurrences
  bin <- richPlotData[which(richPlotData$stage==b),]
  ## bivalve information
  richness[which(richness$stage == b),"bivalve_med"] <- median(bin[,"bivalve"])
  richness[which(richness$stage == b),"bivalve_5th"] <- quantile(bin[,"bivalve"], probs = 0.05)
  richness[which(richness$stage == b),"bivalve_95th"] <- quantile(bin[,"bivalve"], probs = 0.95)
  richness[which(richness$stage == b),"bivalve_mean"] <- mean(bin[,"bivalve"])
  richness[which(richness$stage == b),"bivalve_plus1sd"] <- mean(bin[,"bivalve"])+sd(bin[,"bivalve"])
  richness[which(richness$stage == b),"bivalve_minus1sd"] <- mean(bin[,"bivalve"])-sd(bin[,"bivalve"])
  ## brachiopod information
  richness[which(richness$stage == b),"brachiopod_med"] <- median(bin[,"brachiopod"])
  richness[which(richness$stage == b),"brachiopod_5th"] <- quantile(bin[,"brachiopod"], probs = 0.05)
  richness[which(richness$stage == b),"brachiopod_95th"] <- quantile(bin[,"brachiopod"], probs = 0.95)
  richness[which(richness$stage == b),"brachiopod_mean"] <- mean(bin[,"brachiopod"])
  richness[which(richness$stage == b),"brachiopod_plus1sd"] <- mean(bin[,"brachiopod"])+sd(bin[,"brachiopod"])
  richness[which(richness$stage == b),"brachiopod_minus1sd"] <- mean(bin[,"brachiopod"])-sd(bin[,"brachiopod"])
}

## Any NAs
richness[which(is.na(richness$bivalve_plus1sd)),"bivalve_plus1sd"] <- richness[which(is.na(richness$bivalve_plus1sd)),"bivalve_mean"]
richness[which(is.na(richness$bivalve_minus1sd)),"bivalve_minus1sd"] <- richness[which(is.na(richness$bivalve_minus1sd)),"bivalve_mean"]
richness[which(is.na(richness$brachiopod_plus1sd)),"brachiopod_plus1sd"] <- richness[which(is.na(richness$brachiopod_plus1sd)),"brachiopod_mean"]
richness[which(is.na(richness$brachiopod_minus1sd)),"brachiopod_minus1sd"] <- richness[which(is.na(richness$brachiopod_minus1sd)),"brachiopod_mean"]

## Any negatives, convert to 0
richness[which(richness$bivalve_minus1sd<0),"bivalve_minus1sd"] <- 0
richness[which(richness$brachiopod_minus1sd<0),"brachiopod_minus1sd"] <- 0

## Order by stage
richness <- richness[order(richness$stage),]

## Plot
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_NC_CR20_spatially_standardised_richness.pdf")
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = richness$midpoint, y = richness$bivalve_mean, main = "Spatially standardised generic richness (mean +/- 1 SD)\nClassical rarefaction (sample size = 20)\n 100km grid cells, no covariate data required", axes = FALSE, xlim = c(520, 0), ylim = c(0,20), yaxs="i", xaxs="i",
     xlab = "Time (mya)", ylab = "Generic richness", type = "l", col = rgb(red = 1, green = 0, blue = 0, alpha = 0.75))
polygon(c(rev(richness$midpoint), richness$midpoint), c(rev(richness$bivalve_plus1sd), richness$bivalve_minus1sd), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.25), border = NA)
lines(x = richness$midpoint, y = richness$brachiopod_mean, xlim = c(520, 0), ylim = c(0,75),
      xlab = NA, ylab = "", type = "l", col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75))
polygon(c(rev(richness$midpoint), richness$midpoint), c(rev(richness$brachiopod_plus1sd), richness$brachiopod_minus1sd), col = rgb(red = 0, green = 0, blue = 1, alpha = 0.25), border = NA)
box()
axis(2)
legend("topleft",legend = c("Bivalves", "Brachiopods"),bg = "white", col = c(rgb(red = 1, green = 0, blue = 0, alpha = 0.75),col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75)),lty = 1)
axis_geo(side = 1, intervals = "international periods")
dev.off()

#### Plotting richness by group ####
## Get clean data for plotting
plotting.data <- read.csv("data/analysis_data/genera_NC_CR20.csv", row.names = 1)

## Round data
plotting.data$bivalve <- round(plotting.data$bivalve, digits = 0)
plotting.data$brachiopod <- round(plotting.data$brachiopod, digits = 0)

## Re-do model
plotModel <- glmmTMB(brachiopod ~ bivalve + (bivalve|stage), data = plotting.data, family = nbinom2(link = "log"))

## Get model data
line.df <- get_model_data(plotModel, type = "pred", terms = "bivalve")

## Assign color and shapes to data
prePTMErows <- which(plotting.data$PTME == "PrePTME")
postPTMErows <- which(plotting.data$PTME == "PostPTME")
plotting.data$PTME_color <- NA
plotting.data$PTME_shape <- NA
plotting.data[prePTMErows, "PTME_color"] <- "darkblue"
plotting.data[prePTMErows, "PTME_shape"] <- 4
plotting.data[postPTMErows, "PTME_color"] <- "darkorange"
plotting.data[postPTMErows, "PTME_shape"] <- 19

## Get unique point and color vectors
point.col <- unique(plotting.data$PTME_color)
point.shape <- unique(plotting.data$PTME_shape)
legend.shape <- point.shape[c(2,1)]

## Define plot title
plot.title <- "Generic richness from 100km grid cells\nClassical rarefaction (sample size = 20), no covariate data required"

## Plot model bit by bit
## basic scatter plot
scatter <- ggplot() +
  xlab("Bivalve generic richness") +
  ylab("Brachiopod generic richness") +
  ggtitle(plot.title) +
  geom_point(data = plotting.data, aes(x = bivalve, y = brachiopod, colour = PTME, shape = PTME), ) +
  scale_color_manual(breaks = unique(plotting.data[,"PTME"]), values = point.col) +
  scale_shape_manual(values = point.shape) +
  geom_line(data = line.df, aes(x = x, y = predicted), linetype = "dashed") +
  geom_ribbon(data = line.df, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3)+
  scale_x_continuous(expand = c(0,0), limits = c(0,18)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,18)) +
  guides(shape = "none",
         color = guide_legend(override.aes = list(shape = legend.shape))) +
  theme(text = element_text(family = "Helvetica"),,
        title = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.key.size = unit(10,"point"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))
scatter

## Export
pdf(file = paste0("figures/final/main/genera_NC_CR20_richness.pdf"))
print(scatter)
dev.off()
