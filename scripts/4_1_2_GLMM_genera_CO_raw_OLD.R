## 4.1.2 GLMM - genera, covPrun, raw
## Started by TJS on 27/06/2025

#### Startup ####
## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("lme4", "MuMIn", "sjmisc", "glmmTMB", "DHARMa", "performance", "car", "divDyn", "gstat", "raster", "terra", "xtable", "sjPlot", "ggplot2", "palaeoverse", "ggeffects", "stringr", "simr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divDyn)
library(MuMIn)
library(sjmisc)
library(lme4)
library(car)
library(glmmTMB)
library(DHARMa)
library(performance)
library(gstat)
library(raster)
library(terra)
library(sjPlot)
library(ggplot2)
library(palaeoverse)
library(ggeffects)
library(stringr)
library(simr)
library(parallel)

## Load data
raw_data <- read.csv("data/analysis_data/genera_CO_raw.csv", header = T, row.names = 1)

#### Prepping data ####
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

#### Explore relationship between variables and brachiopod richness ####
plot(x = raw_data$bivalve, y = raw_data$brachiopod)
plot(x = raw_data$lith, y = raw_data$brachiopod)
plot(x = raw_data$bath, y = raw_data$brachiopod)
plot(x = raw_data$reef, y = raw_data$brachiopod)
plot(x = raw_data$AbsLat, y = raw_data$brachiopod)
## Pre/Post-PTME
prePTME <- raw_data[which(raw_data$PTME == "PrePTME"),"brachiopod"]
postPTME <- raw_data[which(raw_data$PTME == "PostPTME"),"brachiopod"]
d <- list("prePTME" = prePTME, "postPTME" = postPTME)
boxplot(d)

#### Testing best model to deal with overdispersion ####
## Define model - starting with poisson
model1 <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = poisson(link = "log"))
model2 <- update(model1, control = glmerControl(optimizer="bobyqa"))

## Testing for overdispersion using DHARMa
## Simulate conditional on fitted random effects (more sensitive for mixed models)
simulationOutput2 <- simulateResiduals(model2, refit = F, re.form = NULL)
## Check with DHARMa simulation approach
testDispersion(simulationOutput2, type = "DHARMa")
## Result: ratio is 3.3521, significantly overdispersed

## Let's try negative binomial with classic paramterisation
model3 <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom2(link = "log"))

## Testing for overdispersion using DHARMa
simulationOutput3 <- simulateResiduals(model3, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutput3, type = "DHARMa")
## Result: ratio of 0.021144, significant.

## Let's try negative binomial with quasi-Poisson parameterisation
model4 <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom1(link = "log"))
## Testing for overdispersion using DHARMa
simulationOutput4 <- simulateResiduals(model4, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutput4, type = "DHARMa")
## Result: ratio of 0.47282, insignificant but only just.

## Any better with the third binomial model?
modelMixNB <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom12(link = "log"))
## Testing for overdispersion using DHARMa
simulationOutputMixNB <- simulateResiduals(modelMixNB, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutputMixNB, type = "DHARMa")
## Result: ratio of 0.27905, significant.

## Trying other models. Truncated negative binomial models are incompatible. Try Conway-Maxwell Poisson Distribution. Slow!
modelcompois <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = compois(link = "log"))
## Testing for overdispersion using DHARMa
simulationOutputcompois <- simulateResiduals(modelcompois, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutputcompois, type = "DHARMa")
## Result: ratio of 0.081623. Significant.

## Genpois
modelgenpois <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = genpois(link = "log"))
## Testing for overdispersion using DHARMa o
simulationOutputgenpois <- simulateResiduals(modelgenpois, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutputgenpois, type = "DHARMa")
## Result: ratio of 0.34723, significant

## This is my best model thus far. Let's try other link functions to see if there is any improvement. Let's check if different link functions will help.
bestModel <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom1(link = "log"))

## Check all assumptions at this point
resBestModel <- simulateResiduals(bestModel, re.form = NULL)
testDispersion(resBestModel, type = "DHARMa") ## Ratio of 0.47282, insignificant.
testZeroInflation(resBestModel) ## Ratio of 1.0942, insignificant.
testOutliers(resBestModel, type = "bootstrap") ## Expect freq. of 0.0064 versus observed 0.0014. Insignificant.
plot(resBestModel) ## All fine except normality test. However, this is a minor deviation.
check_collinearity(bestModel) ## All fine - highest is 2.77.

## Try sqrt link function
bestModel_sqrt <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom1(link = "sqrt"))

## Check assumptions
resbestModel_sqrt <- simulateResiduals(bestModel_sqrt, re.form = NULL)
testDispersion(resbestModel_sqrt, type = "DHARMa") ## Ratio of 0.58874, significant. Worse!
testZeroInflation(resbestModel_sqrt) ## Ratio of 0.944, insignificant. Better!
testOutliers(resbestModel_sqrt, type = "bootstrap") ## Expect freq. of 0.0065 versus observed 0. Significant. Worse!
plot(resbestModel_sqrt) ## Much worse!
check_collinearity(bestModel_sqrt) ## Low VIFs!

## Reset best model
bestModel <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom1(link = "log"))
resbestModel <- simulateResiduals(bestModel, re.form = NULL)

## Can this be improved with a dispersion model?
bestModel_disp <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), dispformula = ~stage, data = raw_data, family = nbinom1(link = "log"))
resbestModel_disp <- simulateResiduals(bestModel_disp, re.form = NULL)

## Check all assumptions at this point
testDispersion(resbestModel_disp, type = "DHARMa") ## Ratio of 0.58384, insignificant. A little better
testZeroInflation(resbestModel_disp) ## Ratio of 1.0792, insignificant. A little better.
testOutliers(resbestModel_disp, type = "bootstrap") ## Expect freq. of 0.0070 versus observed 0.0014. Significant. A little worse.
plot(resbestModel_disp) ## All fine except normality test.
check_collinearity(bestModel_disp) ## All fine - highest is 2.74.

## Best model is still regular overall. Will just have to live with low dispersion and failed KS test
bestModel <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom1(link = "log"))
resBestModel <- simulateResiduals(bestModel, re.form = NULL)

## Final assumptions check.
testDispersion(resBestModel, type = "DHARMa") ## Ratio of 0.47282, insignificant.
testZeroInflation(resBestModel) ## Ratio of 1.0942, insignificant.
testOutliers(resBestModel, type = "bootstrap") ## Expect freq. of 0.0064 versus observed 0.0014. Insignificant.
plot(resBestModel) ## All fine except normality test. However, this is a minor deviation.
check_collinearity(bestModel) ## All fine - highest is 2.77.

#### Testing for spatial autocorrelation using DHARMa ####
## First we need to group by location - lets use 50km grid cells, as this is the grain of our analysis
rPrj <- terra::project(x = terra::rast(), y =  "EPSG:8857",
                       res = 100000)
terra::values(rPrj) <- 1:terra::ncell(rPrj)
llOccs <- terra::vect(raw_data, geom = c("long","lat"), crs = "EPSG:8857")
raw_data[, "cell"] <- terra::cells(rPrj, llOccs)[, "cell"]
raw_data[, c("cellLong", "cellLat")] <- terra::xyFromCell(rPrj, raw_data$cell)

## Extract and remove duplicates
groupLocations <- raw_data[,c(11:13)]
groupLocations <- groupLocations[!duplicated(groupLocations),]

## Recalculate residuals for each grid cell
resBestModel2 <- recalculateResiduals(resBestModel, raw_data$cell, mean)

## Now test for spatialautocorrelation
testSpatialAutocorrelation(resBestModel2, x = groupLocations$cellLong, y = groupLocations$cellLat)

## p = 0.001909. However, no visible correlation from plot.

#### Dredge to see if we retain everything ####
## Clean up model data
modelData <- raw_data[,c(1,2,5,6,7,8,9,10)]

## First, re-define model so as to specify na.activity. We include interaction between bivalves and PTME to see if relationship was reset.
model5 <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = modelData, family = nbinom1(link = "log"), na.action = "na.fail")

## Get summary
tab_model(model5)

## Trying with two-way interactions first, will try to add. Sticking with AICc due to relatively small sample sizes in some stages.
combos <- dredge(model5, rank = "AICc")
## Don't worry about convergence warnings,

## Get best model
bestModel <- get.models(combos, delta==0)[[1]]
summary(bestModel)

## Re-do best model just to make sure it has no convergence issues
bestModel <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = modelData, family = nbinom1(link = "log"))

## Test assumptions once again to be sure!
resBestModel <- simulateResiduals(bestModel, re.form = NULL)
testDispersion(resBestModel, type = "DHARMa") ## Ratio of 0.47282, insignificant.
testZeroInflation(resBestModel) ## Ratio of 1.0942, insignificant.
testOutliers(resBestModel, type = "bootstrap") ## Expect freq. of 0.0064 versus observed 0.0014. Insignificant.
plot(resBestModel) ## All fine except normality test. However, this is a minor deviation.
check_collinearity(bestModel) ## All fine - highest is 2.77.

## Spatial autocorrelation once again
## Recalculate residuals for each grid cell
resBestModel2 <- recalculateResiduals(resBestModel, raw_data$cell, mean)
## Now test for spatialautocorrelation
testSpatialAutocorrelation(resBestModel2, x = groupLocations$cellLong, y = groupLocations$cellLat)

## Inspection shows no obvious correlation but low p-value of 0.0019 shows some. Undoubtedly a problem due to replicates.

#### Calculate marginal effects ####
